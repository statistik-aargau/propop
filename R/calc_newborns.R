#' Calculate newborn children per projection year and demographic group.
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
#'
#' @return Returns a data frame with the number of newborn children 'births'
#' structured in demographic groups.
#' @export
#'
calc_newborns <- function(
    population,
    parameters,
    fert_first,
    fert_last,
    share_born_female) {
  # Prepare population data ----
  population_prep <- population |>
    # females in the fertile age range are defined by `fert_first` and `fert_last`
    filter(sex == "f", age %in% c(fert_first:fert_last)) |>
    select(any_of(c(
      "year", "spatial_unit", "scen", "nat", "sex", "age", "birthrate",
      "int_mothers", "births", "n_jan", "n_dec"
    ))) |>
    # calculate shares and rates
    mutate(
      births_int_int = 1 - int_mothers,
      share_born_female = share_born_female,
      share_born_male = 1 - share_born_female,
      int_mothers = case_when(nat == "ch" ~ 0, TRUE ~ int_mothers),
      births_int_int = case_when(nat == "ch" ~ 0, TRUE ~ births_int_int)
    )

  # Calculate newborns ----
  # In documentation have gender proportion  tames all births,
  # These parts below seem no but mostly they average out the number of females with
  #  the relevant age this year with number of females in the age-1 from the previous
  #  year before and then multiplying by the birth rate of this year
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
    )

  # Clean data ----
  df_newborns <- df_newborns_calc |>
    select(ch_m, ch_f, int_m, int_f) |>
    # long format
    pivot_longer(
      names_to = "ID",
      values_to = "births",
      cols = everything()
    ) |>
    # split ID-string into demographic units
    tidyr::separate(ID, into = c("nat", "sex"), sep = "_") |>
    # remove NAs due to the impossible combination of Swiss females giving birth
    # to international females/males
    filter(!is.na(births))

  # Aggregate newborns by demographic groups ----
  df_newborns_aggregated <- df_newborns |>
    summarize(births = sum(births), .by = c(nat, sex))

  # Check if there are no NAs
  assertthat::assert_that(
    isTRUE(all((!is.na(df_newborns_aggregated$births)))),
    msg = "There are NA values in newborns aggregated across demographic groups."
  )

  # Cohort component method ----
  df_newborns_out <- df_newborns_aggregated |>
    # complement data
    mutate(
      year = max(unique(population_prep$year)),
      age = 0,
      scen = unique(population_prep$scen[!is.na(population_prep$scen)]),
      spatial_unit =
        unique(population_prep$spatial_unit[!is.na(population_prep$spatial_unit)]),
      n_dec = 0
    ) |>
    select(any_of(c(
      "year", "nat", "sex", "age", "scen", "spatial_unit", "n_dec", "births"
    ))) |>
    # add info from parameters
    left_join(parameters) |>
    # arrange data
    mutate(sex = factor(sex, levels = c("m", "f"))) |>
    arrange(year, nat, sex, age) |>
    # apply FSO method for projections
    mutate(
      # international emigration
      emi_int_n = births * emi_int_rate,
      # emigration to other cantons
      emi_nat_n = births * emi_nat_rate,
      # acquisition of the Swiss citizenship
      acq_n = births * acq_rate,
      # subtract new Swiss citizens from the international population
      acq_n = ifelse(nat == "ch", dplyr::lead(acq_n, 2), -acq_n),
      # mortality (deaths)
      mor_n = mor_rate *
        (births * (1 - (2 / 3) * (emi_int_rate + acq_rate + emi_nat_rate)) +
          (2 / 3) * (imm_int_n + acq_n + imm_nat_n)),
      # calculate the population balance
      n_dec = births - mor_n - emi_int_n - emi_nat_n + acq_n + imm_int_n +
        imm_nat_n
    )

  return(df_newborns_out)
}
