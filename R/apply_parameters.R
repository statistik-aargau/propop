#' This function runs projections with tables instead of matrices
#' (work-in-progress)
#'
#' @description
#' This function is applied with `purrr::reduce()` to iterate across years.
#' An example is currently in development; see script: dev/reduce_function.R
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
apply_parameters <- function(population, parameters) {
  # remove later:
  # browser()

  # `pop_year` helps to identify the progress of the iteration. For the first
  # iteration, pop_year returns a single year. For all following iterations,
  # `pop_year` contains two years.
  pop_year <- unique(population$year)

  # The calculation of the number of annual newborn children is still in
  # development. Thus, a proxy is used here where the number of newborns is
  # identical across demographic groups and years.
  proxy_newborn <- parameters |>
    filter(age == 0) |>
    select(year, nat, sex, age, spatial_unit) |>
    # for now, 2482 babies are born each year across demographic groups. This
    # number reflects the result from the example-run from propop() for newborn
    # Swiss males in the year 2019. The value has been chosen to compare results
    # from this demographic unit during the development process.
    mutate(
      n_dec = 0,
      births = 2482,
      year = as.double(year),
      year = max(pop_year)
    )

  # If the length of `pop_year` is 1, the initial population is used here.
  # Otherwise, if the length of `pop_year` is larger than one, the population
  # is retrieved from the previous iterations' result.
  if (isTRUE(length(pop_year) != 1)) {
    population_prev1 <- population |>
      # filter for year from parameters
      filter(year == parameters$year - 1) |>
      # advance the population by one year: increase the age by one year,
      # aggregate people aged 100 and older, and add newborns.
      advance_population(proxy_newborn = proxy_newborn)
  } else {
    population_prev1 <- population |>
      # advance the population by one year: increase the age by one year,
      # aggregate people aged 100 and older, and add newborns.
      advance_population(proxy_newborn = proxy_newborn)
  }

  # Prepare the population from the previous iteration to calculate the
  # projection of the next year
  population_prev2 <- population_prev1 |>
    # select identifier columns and population
    select(any_of(c(
      "year", "nat", "sex", "age", "spatial_unit", "n_jan", "births"
    ))) |>
    # adapt year to parameters
    mutate(year = parameters$year)

  # Calculate the projection
  population_new <- parameters |>
    # join population and parameters
    left_join(population_prev2) |>
    # calculate the projection
    calc_proj_tables()

  # Bind reults of year t and year t+1
  population_out <- bind_rows(population, population_new)

  return(population_out)
}
