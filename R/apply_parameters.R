#' This function runs projections with tables instead of matrices
#' (work-in-progress)
#'
#' @description
#' This function is applied with `purrr::reduce()` to iterate across years.
#' An example is currently in development; see script: dev/reduce_function.R
#' The process uses three helper functions:
#' * `advance_population()`: Advances the population by one year; increases
#'    age by one year and aggregates people aged 100 and older.
#'
#' @param population data frame including the starting population of each
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
#' @param parameters list, data frames containing the FSO rates and numbers to
#' run the projection for a specific spatial level (e.g., canton, municipality).
#' @param fert_first numeric, first year of female fertility. Defaults to 16
#'        (FSO standard value).
#' @param fert_last numeric, last year of female fertility. Defaults to 50
#'        (FSO standard value).
#' @param share_born_female numeric, fraction of female babies. Defaults to
#'        100 / 205 (FSO standard value).
#'
#' @return
#' Returns a data frame that includes the number of people for each demographic
#'      group per year (for projected years) and spatial unit. The number of
#'      rows is the product of all years times all demographic groups times
#'      all spatial units.
#'      The output includes several \bold{identifiers} that indicate to which
#'      demographic group, year, and spatial unit the results in the rows refer
#'      to:
#'      \item{year}{integer, indicating the projected years.}
#'      \item{spatial_unit}{factor, spatial units for which the projection
#'            was run (e.g., canton, districts, municipalities).}
#'      \item{age}{integer, ranging from `0`n to `100` (including those older
#'      than 100).}
#'      \item{sex}{factor, female (f) and male (m).}
#'      \item{nat}{factor, indicates if the nationality is Swiss (ch) or
#'      international / foreign (int). This variable is only returned if
#'      `binational = TRUE`.}
#'      The output also includes columns related to the \bold{size and change
#'      of the population:}
#'      \item{n_jan}{numeric, start-of-year population per demographic group.}
#'      \item{n_dec}{numeric, end-of-year population per demographic group.}
#'      The \bold{components} that are used to project the development of the
#'      population are also included in the output:
#'      \item{births}{numeric, number of births (non-zero values are only
#'      available for age = 0).}
#'      \item{mor_n}{numeric, number of deaths.}
#'      \item{emi_int}{numeric, number of people who emigrate
#'      to other countries.}
#'      \item{emi_nat_n}{numeric, number of people who emigrate
#'      to other cantons.}
#'      \item{imm_int_n}{numeric, number of people who immigrate
#'      from other countries.}
#'      \item{imm_nat_n}{numeric, number of people who immigrate
#'      from other cantons.}
#'      \item{acq_n}{numeric, number of people who acquire Swiss citizenship.}
#'
#' @export
apply_parameters <- function(
    population,
    parameters,
    fert_first = 16,
    fert_last = 50,
    share_born_female = 100 / 205) {
  browser()

  # Checks ----
  # numeric year if not numeric

  # Define projection year ----
  # `pop_year` helps to identify the progress of the iteration. For the first
  # iteration, pop_year returns a single year. For all following iterations,
  # `pop_year` contains two years.
  pop_year <- unique(population$year)

  # If the length of `pop_year` is 1, the initial population is used here.
  # Otherwise, if the length of `pop_year` is larger than one, the population
  # is retrieved from the previous iterations' result.
  if (isTRUE(length(pop_year) != 1)) {
    population_prev <- population |>
      # filter for year from parameters
      filter(year == parameters$year - 1)
  } else {
    population_prev <- population
  }

  # Advance population ----
  # prepare the population from the previous iteration for the projection of the
  # next year.
  population_aged <- population_prev |>
    # use helper function to advance the population by one year
    advance_population() |>
    # adapt projection year to year in parameters
    mutate(year = unique(parameters$year))

  # Checks ----
  # population_aged does not contain newborns

  # Calculate the projection for ages 1-100 ----
  population_new <- parameters |>
    # exclude newborn children
    dplyr::filter(age > 0) |>
    # join population and parameters
    left_join(
      population_aged,
      by = c("year", "spatial_unit", "nat", "sex", "age")
    ) |>
    # use helper function to calculate projections
    calc_proj_tables() |>
    # bind results of year t and year t+1
    bind_rows(population)


  # Project newborns of year t+1 ----
  newborns <-
    calc_newborns(
      population = population_new,
      parameters = parameters,
      fert_first = fert_first,
      fert_last = fert_last,
      share_born_female = share_born_female
    )

  # Projection result ----
  # Bind results of year t and year t+1
  population_out <- bind_rows(population_new, newborns) |>
    # clean the data
    select(any_of(c(
      "year", "spatial_unit", "scen", "nat", "sex", "age", "n_jan", "births",
      "mor_n", "emi_int_n", "emi_nat_n", "imm_int_n", "imm_nat_n", "acq_n",
      "mig_sub", "n_dec"
    ))) |>
    mutate(
      sex = factor(sex, levels = c("m", "f")),
      nat = factor(nat, levels = c("ch", "int"))
    ) |>
    arrange(nat, desc(sex), age, year, spatial_unit)

  return(population_out)
}
