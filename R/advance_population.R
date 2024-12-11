#' Advances the population by one year.
#'
#' @description
#' Advance the population by one year: increase the age by one year, aggregate
#' people aged 100 and older, and add newborns.
#'
#' @param .data data frame including the starting population of each
#' demographic group; either the initial population or the previous projected
#' year. Possible identifiers are the same as in `parameters` (apart from year).
#' The data frame only includes one year, usually the one preceding the first
#' projected year.
#'    * `year` numeric
#'    * `spatial_unit` character.
#'    * `nat` character.
#'    * `sex` character.
#'    * `age` numeric.
#'    * `n_dec` numeric, number of people per demographic group in December.
#'
#' @return returns an identically structured data frame with the aged population.
#' @export
#'
advance_population <- function(.data) {
  .data |>
    mutate(
      # advance the populations' age by one year
      age = age + 1,
      # group people aged 100 and older (age = 101) into one group (age = 100)
      age = case_when(age == 101 ~ 100, TRUE ~ age),
      # remove later:
      year = as.numeric(year)
    ) |>
    # aggregate people aged 100 and older
    summarize(n_dec = sum(n_dec), .by = c(year, nat, sex, age, spatial_unit))
}
