#' Project population development
#'
#' @description
#' This function is applied within the wrapper function `propop_tables()`
#' to iterate across years and spatial units.
#'
#' The calculations involve three helper functions:
#' * `advance_population()`: Advances the population by one year (increases
#'    age by one year and aggregates people aged 100 and older).
#' * `calculate_projection()`: Calculates the projection for ages 1-100.
#' * `calculate_newborns()`: Calculate newborns per demographic group.
#'
#' @param population data frame including the starting population of each
#' demographic group; either the initial population or the previous projected
#' year. Possible identifiers are the same as in `parameters` (apart from year).
#' The data frame only includes one year, usually the one preceding the first
#' projected year.
#' @param parameters list, data frames containing the FSO rates and numbers to
#' run the projection for a specific spatial level (e.g., canton, municipality).
#' @param fert_first numeric, first year of female fertility. Defaults to 16
#'        (FSO standard value).
#' @param fert_last numeric, last year of female fertility. Defaults to 50
#'        (FSO standard value).
#' @param share_born_female numeric, fraction of female babies. Defaults to
#'        100 / 205 (FSO standard value).
#' @param subregional character or NULL, indicates if subregional migration
#'        patterns (e.g., movement between municipalities within a canton) are
#'        part of the projection (default `subregional = NULL`). Requires input
#'        on the level of subregions (in `parameters` and `population`).
#'        Two calculation methods are supported to distribute people between
#'        subregions: With `subregional = "net"`, the net migration between
#'        subregions is added to the population balance. Net migration numbers
#'        must be specified in a data column `mig_sub` in `parameters`.
#'        With `subregional = "rate"`, the numbers for subregional emigrants are
#'        subtracted from the population balance, then redistributed back to all
#'        subregional units as subregional immigration; `parameters` must contain
#'        the columns `emi_sub` and `imm_sub`.
#' @param binational boolean, `TRUE` indicates that projections discriminate
#'        between two groups of nationalities. `FALSE` indicates that the
#'        projection is run without distinguishing between nationalities.
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
#' @autoglobal
#' @noRd
#'
project_population <- function(
    population,
    parameters,
    fert_first = 16,
    fert_last = 50,
    share_born_female = 100 / 205,
    subregional = NULL,
    binational = TRUE) {
  # Checks ----
  ## Mandatory parameters ----
  assertthat::assert_that("scen" %in% names(parameters),
    msg = "Column `scen` is missing in parameters."
  )
  assertthat::assert_that("sex" %in% names(parameters),
    msg = "Column `sex` is missing in parameters."
  )
  assertthat::assert_that("age" %in% names(parameters),
    msg = "Column `age` is missing in parameters."
  )
  assertthat::assert_that("year" %in% names(parameters),
    msg = "Column `year` is missing in parameters."
  )
  assertthat::assert_that("birthrate" %in% names(parameters),
    msg = "Column `birthrate` is missing in parameters."
  )
  assertthat::assert_that("mor" %in% names(parameters),
    msg = "Column `mor` is missing in parameters."
  )
  assertthat::assert_that("emi_int" %in% names(parameters),
    msg = "Column `emi_int` is missing in parameters."
  )
  assertthat::assert_that("emi_nat" %in% names(parameters),
    msg = "Column `emi_nat` is missing in parameters."
  )
  assertthat::assert_that("imm_int_n" %in% names(parameters),
    msg = "Column `imm_int_n` is missing in parameters."
  )
  assertthat::assert_that("imm_nat_n" %in% names(parameters),
    msg = "Column `imm_nat_n` is missing in parameters."
  )
  assertthat::assert_that("spatial_unit" %in% names(parameters),
    msg = paste0("Column `spatial_unit` is missing in parameters.")
  )

  ## Optional parameter when requested ----
  # Subregional migration
  if (!is.null(subregional) && subregional == "net") {
    assertthat::assert_that("mig_sub" %in% names(parameters),
      msg = "Column `mig_sub` is missing in parameters."
    )
  } else if (!is.null(subregional) && subregional == "rate") {
    assertthat::assert_that("emi_sub" %in% names(parameters),
      msg = "Column `emi_sub` is missing in parameters."
    )
    assertthat::assert_that("imm_sub" %in% names(parameters),
      msg = "Column `imm_sub` is missing in parameters."
    )
  } else {
    parameters <- parameters
  }

  ## Population data frame ----
  assertthat::assert_that("year" %in% names(population),
    msg = "Column `year` is missing in `population`."
  )
  assertthat::assert_that(!any(is.na(population$year)),
    msg = "Column 'year' in `population` must not include any missing values (NA)."
  )

  ## Check other arguments ----
  # convert input in years to integer, results in error if not possible
  population$year <- vctrs::vec_cast(population$year, integer())
  parameters$year <- vctrs::vec_cast(parameters$year, integer())
  assertthat::assert_that(is.integer(population$year),
    msg = "The variable 'year' must be numeric in 'population'."
  )
  assertthat::assert_that(is.integer(parameters$year),
    msg = "The variable 'year' must be numeric in 'parameters'."
  )

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
      filter(year == unique(parameters$year) - 1)
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

  # Calculate the projection for ages 1-100 ----
  population_new <- population_aged |>
    # join parameters
    left_join(
      parameters,
      by = c("year", "spatial_unit", "nat", "sex", "age")
    ) |>
    # use helper function to calculate projections
    calculate_projection(subregional = subregional) |>
    # bind results of year t and year t+1
    bind_rows(population |> filter(year == unique(population_prev$year)))

  ## Progress feedback ----
  # Provide progress information
  cli::cli_alert_success("Year: {.val { max(population_new$year) }}")

  # Project newborns of year t+1 ----
  newborns <-
    calculate_newborns(
      population = population_new,
      parameters = parameters,
      fert_first = fert_first,
      fert_last = fert_last,
      share_born_female = share_born_female,
      subregional = subregional
    )

  # Projection result ----
  # Bind results of year t and year t+1
  population_out <- population_new |>
    filter(year == unique(population_aged$year)) |>
    bind_rows(newborns |> mutate(n_jan = 0)) |>
    bind_rows(population) |>
    mutate(births = ifelse(age == 0, births, 0)) |>
    # clean the data
    select(
      year, scen, spatial_unit, nat, sex, age, births, n_jan, mor_n, emi_int_n,
      emi_nat_n, any_of(c("emi_sub_n", "imm_sub_n", "mig_sub")), imm_int_n,
      imm_nat_n, acq_n, n_dec
    ) |>
    mutate(
      sex = factor(sex, levels = c("m", "f")),
      nat = factor(nat, levels = c("ch", "int"))
    ) |>
    arrange(spatial_unit, year, scen, age, nat, desc(sex))


  return(population_out)
}
