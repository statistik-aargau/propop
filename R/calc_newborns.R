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
  # Prepare population data
  population_prep <- population |>
    mutate(year = year + 1) |>
    # add parameters
    left_join(
      parameters |>
        select(any_of(c(
          "year", "spatial_unit", "scen", "nat", "sex", "age", "birthrate",
          "int_mothers"
      )))) |>
    # filter for females in the fertile age
    filter(age %in% c(fert_first:fert_last), sex != "m") |>
    # calculate shares and rates
    mutate(
      births_int_int = 1 - int_mothers,
      share_born_female = share_born_female,
      share_born_male = 1 - share_born_female,
      int_mothers = case_when(
        sex == "m" | nat == "ch" ~ 0,
        sex == "f" & nat == "ch" & !(age %in% c(fert_first:fert_last)) ~ 0,
        TRUE ~ int_mothers
      ),
      share_born_female = case_when(
        sex == "m" ~ 0,
        sex == "f" & !(age %in% c(fert_first:fert_last)) ~ 0,
        TRUE ~ share_born_female
      ),
      share_born_male = case_when(
        sex == "m" ~ 0,
        sex == "f" & !(age %in% c(fert_first:fert_last)) ~ 0,
        TRUE ~ share_born_male
      ),
      births_int_int = case_when(
        sex == "m" | nat == "ch" ~ 0,
        sex == "f" & !(age %in% c(fert_first:fert_last)) ~ 0,
        TRUE ~ births_int_int
      )
    )

  # Calculate newborns
  df_newborns_calc <- population_prep |>
    mutate(
      # newborn Swiss males
      ch_m = case_when(
        # swiss females giving birth to Swiss males
        nat == "ch" & age %in% c(fert_first:fert_last) ~
          n_dec * birthrate * share_born_male,
        # foreign females giving birth to Swiss males
        nat == "int" & age %in% c(fert_first:fert_last) ~
          n_dec * birthrate * share_born_male * int_mothers
      ),
      # newborn Swiss females
      ch_f = case_when(
        # swiss females giving birth to Swiss females
        nat == "ch" & age %in% c(fert_first:fert_last) ~
          n_dec * birthrate * share_born_female,
        nat == "int" & age %in% c(fert_first:fert_last) ~
          n_dec * birthrate * share_born_female * int_mothers
      ),
      # foreign females giving birth to foreign males
      int_m = case_when(
        nat == "int" & age %in% c(fert_first:fert_last) ~
          n_dec * birthrate * share_born_male * births_int_int
      ),
      # foreign females giving birth to foreign females
      int_f = case_when(
        nat == "int" & age %in% c(fert_first:fert_last) ~
          n_dec * birthrate * share_born_female * births_int_int
      )
    )

  # Clean data
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
    filter(!is.na(births))


  # Check if the number of rows is as expected (adapt for binational = FALSE?)
  assertthat::assert_that(
    nrow(df_newborns) == (
      # Swiss females: age range for fertility x two sexes of newborns
      ((fert_last + 1 - fert_first) * 2) +
        # International females: age range for fertility x two sexes of newborns
        # x two nationalities of newborns
        ((fert_last + 1 - fert_first) * 2 * 2)
    ),
    msg = "The number of rows in df_newborns is smaller or larger than expected."
  )

  # Aggregate newborns by demographic groups
  df_newborns_agg <- df_newborns |>
    summarize(births = sum(births), .by = c(nat, sex)) |>
    # complement data
    mutate(
      year = unique(population_prep$year),
      age = 0,
      scen = unique(population_prep$scen),
      spatial_unit = unique(population_prep$spatial_unit),
      n_dec = 0
    ) |>
    select(
      year, nat, sex, age, scen, spatial_unit, n_dec, births
    )


  # Check if there are no NAs
  assertthat::assert_that(
    isTRUE(all((!is.na(df_newborns_agg$births)))),
    msg = "There are NA values in newborns aggregated across demographic groups."
  )

  return(df_newborns_agg)
}
