#' Advances the population by one year.
#'
#' @description
#' Advances the population by one year; increases age by one year and aggregates
#' people aged 100 and older.
#'
#' @param .data data frame including the starting population of each
#' demographic group; either the initial population or the previous projected
#' year. Possible identifiers are the same as in `parameters` (apart from year).
#' The data frame only includes one year, usually the one preceding the first
#' projected year.
#'    * `year` numeric
#'    * `scen`, character, one or several projection scenario(s). The main
#'       scenarios are usually "reference", "low" growth, and "high" growth.
#'    * `spatial_unit` character.
#'    * `nat` character.
#'    * `sex` character.
#'    * `age` numeric.
#'    * `n_dec` numeric, number of people per demographic group in December.
#'
#' @return returns an identically structured data frame with the aged population.
#' @autoglobal
#' @noRd
#'
advance_population <- function(.data) {
  .data |>
    mutate(
      # advance the populations' age by one year
      age = age + 1,
      # group people aged 100 and older (age = 101) into one group (age = 100)
      age = ifelse(age == 101, 100, age)
    ) |>
    # aggregate people aged 100 and older
    summarize(n_dec = sum(n_dec), .by = c(year, nat, sex, age, spatial_unit)) |>
    # define factor levels
    mutate(sex = factor(sex, levels = c("m", "f"))) |>
    # arrange data
    arrange(spatial_unit, year, nat, sex, age) |>
    # select identifier columns and population
    select(year, nat, sex, age, spatial_unit, n_dec) |>
    # the population in December of year t becomes the population in January
    # of year t+1
    rename(n_jan = n_dec)
}
