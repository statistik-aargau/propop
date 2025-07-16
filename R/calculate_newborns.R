#' Calculate the number of newborn babies.
#'
#' @description
#' Calculates the number of newborn babies based on birthrates and the female
#' population within a pre-defined fertility age range. In a second step, the
#' cohort component method is used to determine newborn's survival rates.
#'
#' @param population data frame including the starting population of each
#'        demographic group; either the initial population or the previous
#'        projected year.
#' @param parameters data frame containing the FSO rates and numbers to run the
#'        projection for a specific spatial level (e.g., canton, municipality).
#' @param fert_first numeric, first year of female fertility. Defaults to 16
#'        (FSO standard value).
#' @param fert_last numeric, last year of female fertility. Defaults to 50
#'        (FSO standard value).
#' @param share_born_female numeric, fraction of female babies. Defaults to
#'        100 / 205 (FSO standard value).
#' @param subregional boolean, TRUE indicates that subregional migration
#'        patterns (e.g., movement between municipalities within a canton)
#'        are part of the projection.
#'
#' @return Returns a data frame with the number of newborn children 'births'
#' structured in demographic groups.
#' @export
#'
#' @autoglobal
#'
calculate_newborns <- function(
    population,
    parameters,
    fert_first,
    fert_last,
    share_born_female,
    subregional = FALSE) {
  # Prepare population data ----
  population_prep <- population |>
    #Ffemales in the fertile age range are defined by `fert_first` and `fert_last`
    filter(sex == "f", age %in% c(fert_first:fert_last)) |>
    select(any_of(c(
      "year", "spatial_unit", "scen", "nat", "sex", "age", "birthrate",
      "int_mothers", "births", "n_jan", "n_dec"
    ))) |>
    # Calculate shares and rates
    mutate(
      births_int_int = 1 - int_mothers,
      share_born_female = share_born_female,
      share_born_male = 1 - share_born_female,
      int_mothers = case_when(nat == "ch" ~ 0, TRUE ~ int_mothers),
      births_int_int = case_when(nat == "ch" ~ 0, TRUE ~ births_int_int)
    )

  # Calculate newborns ----
  df_newborns_calc <- population_prep |>
    mutate(
      # average number of females between year t and year t+1
      n_average = (n_jan + n_dec) / 2,
      # newborn Swiss males
      ch_m = case_when(
        # Swiss females giving birth to Swiss males
        nat == "ch" ~ (n_average * birthrate) * share_born_male,
        # international females giving birth to Swiss males
        nat == "int" ~ (n_average * birthrate) * share_born_male * int_mothers
      ),
      # newborn Swiss females
      ch_f = case_when(
        # Swiss females giving birth to Swiss females
        nat == "ch" ~ (n_average * birthrate) * share_born_female,
        # international females giving birth to Swiss females
        nat == "int" ~ (n_average * birthrate) * share_born_female * int_mothers
      ),
      # newborn international males
      int_m = case_when(
        # international females giving birth to international males
        nat == "int" ~ (n_average * birthrate) * share_born_male * births_int_int
      ),
      # newborn international females
      int_f = case_when(
        # international females giving birth to international females
        nat == "int" ~ (n_average * birthrate) * share_born_female * births_int_int
      )
    ) |>
    filter(year == max(year))

  # Clean data ----
  df_newborns <- df_newborns_calc |>
    select(year, spatial_unit, ch_m, ch_f, int_m, int_f) |>
    # long format
    pivot_longer(
      names_to = "ID",
      values_to = "births",
      cols = c(ch_m, ch_f, int_m, int_f)
    ) |>
    # split ID-string into demographic units
    tidyr::separate(ID, into = c("nat", "sex"), sep = "_") |>
    # remove NAs due to the impossible combination of Swiss females giving birth
    # to international females/males
    filter(!is.na(births))

  # Aggregate newborns by demographic groups ----
  df_newborns_aggregated <- df_newborns |>
    summarize(births = sum(births), .by = c(year, spatial_unit, nat, sex))

  # Check if there are no NAs
  assertthat::assert_that(
    isTRUE(all((!is.na(df_newborns_aggregated$births)))),
    msg = "NA values were found in newborns aggregated across demographic groups."
  )

  # Cohort component method ----
  df_newborns_out <- df_newborns_aggregated |>
    # complement data
    mutate(
      # year = max(unique(population_prep$year)),
      age = 0,
      # scen = unique(population_prep$scen[!is.na(population_prep$scen)]),
      # spatial_unit =
      #   unique(population_prep$spatial_unit[!is.na(population_prep$spatial_unit)]),
      # n_dec = 0
    ) |>
    select(year, nat, sex, age, spatial_unit, births) |>
    # add info from parameters
    left_join(
      parameters,
      by = c("year", "nat", "sex", "age", "spatial_unit"),
      relationship = "one-to-one"
    ) |>
    # arrange data
    mutate(sex = factor(sex, levels = c("m", "f"))) |>
    arrange(spatial_unit, year, nat, sex, age) |>
    # apply FSO method for projections
    mutate(
      .by = spatial_unit,
      n_jan = NA,
      # international emigration
      emi_int_n = births * emi_int,
      # emigration to other cantons
      emi_nat_n = births * emi_nat,
      # emigration to other subregional units
      emi_sub_n = births * emi_sub,
      # acquisition of the Swiss citizenship
      acq_n = births * acq,
      # subtract new Swiss citizens from the international population
      acq_n = ifelse(
        nat == "ch",
        dplyr::lead(
          acq_n,
          n = 2,
          order_by = c(sex, nat)
        ),
        -acq_n
      ),
      # mortality (deaths)
      mor_n = mor *
        (births * (1 - (2 / 3) * (emi_int + acq + emi_nat)) +
          (2 / 3) * (imm_int_n + acq_n + imm_nat_n)),
      # calculate the population balance
      n_dec = births - mor_n - emi_int_n - emi_nat_n - emi_sub_n +
        acq_n + imm_int_n + imm_nat_n
    )

  # Redistribute subregional emigration back to all subregional units as immigration
  if (subregional) {
    # get total of subregional emigration
    dat_emi_sub_n_total <- df_newborns_out |>
      dplyr::summarise(.by = c(year, nat, sex, age), emi_sub_n_total = sum(emi_sub_n))

    # redistribution according to provided shares for each subregion
    df_newborns_out |>
      dplyr::left_join(dat_emi_sub_n_total,
        by = dplyr::join_by(year, nat, sex, age)
      ) |>
      dplyr::mutate(
        imm_sub_n = imm_sub * emi_sub_n_total,
        n_dec = n_dec + imm_sub_n
      )
  } else {
    return(df_newborns_out)
  }
}
