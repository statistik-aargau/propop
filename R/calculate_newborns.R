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
#' @param subregional character or NULL, indicates if subregional migration patterns
#'        (e.g., movement between municipalities within a canton) are part of
#'        the projection (default `subregional = NULL`). Two calculation methods
#'        are supported to distribute people between subregions:
#'        With `subregional = "net"`, the net migration between subregions is
#'        added to the population balance. Net migration must be specified in
#'        data column `mig_sub`. With `subregional = "rate"`, the numbers for
#'        subregional emigrants are subtracted from the population balance, then
#'        redistributed back to all subregional units as subregional immigration.
#'
#' @return Returns a data frame with the number of newborn children 'births'
#'         structured in demographic groups.
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
    subregional = subregional) {
  # Prepare population data ----
  population_prep <- population |>
    #Females in the fertile age range are defined by `fert_first` and `fert_last`
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
    select(year, scen, spatial_unit, ch_m, ch_f, int_m, int_f) |>
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
    summarize(births = sum(births), .by = c(year, scen, spatial_unit, nat, sex))

  # Check if there are no NAs
  assertthat::assert_that(
    isTRUE(all((!is.na(df_newborns_aggregated$births)))),
    msg = "NA values were found in newborns aggregated across demographic groups."
  )

  # Cohort component method ----
  df_newborns_out <- df_newborns_aggregated |>
    # complement data
    mutate(
      age = 0,
    ) |>
    select(year, scen, nat, sex, age, spatial_unit, births) |>
    # add info from parameters
    left_join(
      parameters,
      by = c("year", "scen", "nat", "sex", "age", "spatial_unit"),
      relationship = "one-to-one"
    ) |>
    # arrange data
    mutate(sex = factor(sex, levels = c("m", "f"))) |>
    arrange(spatial_unit, scen, year, nat, sex, age) |>
    # apply FSO method for projections
    mutate(
      .by = c(year, scen, spatial_unit, scen, sex, age),
      n_jan = NA,
      # international emigration
      emi_int_n = births * emi_int,
      # emigration to other cantons
      emi_nat_n = births * emi_nat,
      # acquisition of the Swiss citizenship
      acq_n = births * acq,
      # subtract new Swiss citizens from the international population
      acq_n = ifelse(nat == "ch", dplyr::lead(acq_n, order_by = nat), -acq_n),
      # mortality (deaths)
      mor_n = mor *
        (births * (1 - (2 / 3) * (emi_int + acq + emi_nat)) +
          (2 / 3) * (imm_int_n + acq_n + imm_nat_n)),
      # calculate the population balance
      n_dec = births - mor_n - emi_int_n - emi_nat_n - acq_n +
        imm_int_n + imm_nat_n
    )

  # Subregional migration ----
  if (!is.null(subregional) && subregional == "net") {

    # Add net saldo for subregional migration
    df_newborns_out |> mutate(n_dec = n_dec + mig_sub)

  } else if (!is.null(subregional) && subregional == "rate") {

    # Redistribute subregional emigration back to all subregional units as
    # subregional immigration
    df_newborns_out <- df_newborns_out |>
      mutate(
        # emigration to other subregional units
        emi_sub_n = n_jan * (emi_sub * (1 - (mor / 2))),
        # calculate the population balance
        n_dec = n_dec - emi_sub_n
      )

    # Get total of subregional emigration
    df_emi_sub_n_total <- df_newborns_out |>
      dplyr::summarise(
        emi_sub_n_total = sum(emi_sub_n),
        .by = c(year, scen, nat, sex, age)
      )

    # Redistribution according to provided shares for each subregion
    df_newborns_out |>
      dplyr::left_join(
        df_emi_sub_n_total,
        by = dplyr::join_by(year, scen, nat, sex, age)
      ) |>
      dplyr::mutate(
        imm_sub_n = imm_sub * emi_sub_n_total,
        n_dec = n_dec + imm_sub_n
      )

  } else {

    # No subregional migration
    return(df_newborns_out)

  }
}
