#' Project population development
#'
#' @description Wrapper function to project population development using the
#' cohort component method. This function iterates across years and spatial units
#' calling the function `project_population.R`, which performs the calculations.
#'
#' You can either use your own parameters and starting population or download
#' these data from the Swiss Federal Statistical Office (FSO). For instructions
#' on how to download this information from
#' [STAT-TAB](https://www.bfs.admin.ch/bfs/en/home/services/recherche/stat-tab-online-data-search.html),
#' see \code{vignette("prepare_data", package = "propop")}.
#'
#' The projection parameters need to be passed to `propop::propop()` as a
#' \bold{single data frame} (with the parameters as columns). The column types,
#' names, and factor levels need to match the specifications listed below under
#' `parameters`.
#'
#' If nothing else is indicated in argument `scenarios`, `propop()` runs and
#' returns all **scenarios** provided via `parameters`.
#'
#' For more details on how to use this function to project the population
#' development on the level of a canton, see
#' \code{vignette("project_single_region", package = "propop")}.
#'
#' @param parameters data frame containing the FSO rates and numbers to run the
#' projection for a specific spatial level (e.g., canton, municipality).
#'    * `year`, character, projection year.
#'    * `spatial_unit`, character, ID of spatial entity (e.g., canton,
#'    municipality) for which to run the projections.
#'    * `scen`, character, one or several projection scenario(s). The main
#'    scenarios are usually "reference", "low" growth, and "high" growth.
#'    * `nat`, character, nationality (`ch` = Swiss; `int` =
#'    foreign / international).
#'    Required if binational = `TRUE`.
#'    * `sex`, character (`f` = female, `m` = male).
#'    * `age`, numeric, typically ranging from 0 to 100 (incl. >100).
#'    * `birthrate`, numeric, number of births per mother
#'    * `int_mothers`, numeric, proportion of children with
#'    Swiss nationality born to non-Swiss mothers.
#'    Required if binational = `TRUE`.
#'    * `mor`, numeric, prospective mortality rate (probability of death).
#'    * `acq`, numeric, rate of acquisition of Swiss citizenship.
#'    Required if binational = `TRUE`.
#'    * `emi_int`, numeric, rate of people emigrating abroad
#'    (number of immigrants - number of emigrants).
#'    * `emi_nat`, rate of people emigrating to other cantons.
#'    * `imm_int_n`, numeric, number of people immigrating from abroad.
#'    * `imm_nat_n`, numeric, number of people immigrating from other cantons.
#'    * `mig_sub` \bold{(optional)}, numeric, net migration per subregion; this
#'    is the migration from / to other subregions (e.g., municipalities,
#'    districts) within the main superordinate projection unit (e.g., a canton).
#'    Accounts for movements between different subregions. Needs to be provided
#'    by the user.
#'
#' @param population data frame including the starting population of each
#' demographic group and each spatial unit. Possible values are the same as in
#' `parameters` (apart from year). The data frame only includes one year, usually
#' the one preceding the first year of the projection.
#'    * `year` character, should be `year_first` - 1.
#'    * `spatial_unit` character.
#'    * `nat` character.
#'    * `sex` character.
#'    * `age` numeric.
#'    * `n` numeric, number of people per demographic group.
#'
#' @param year_first numeric, first year to be projected.
#' @param year_last numeric, last year to be projected.
#' @param scenarios \bold{(optional)}, character, indicating which
#'        projection scenario(s) shall be run; the corresponding information
#'        must be available in `parameters`.
#'        Defaults to the values in variable `scen` in `parameters`.
#' @param age_groups numeric, number of age classes. Creates a vector with
#'        1-year age classes running from `0` to (`age_groups` - 1). Must
#'        currently be set to `= 101` (FSO standard number of age groups).
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
#' @param spatial_unit character, name of variable containing the names of the
#'        region or subregions for which the projection shall be performed.
#'
#' @returns
#' Returns a \bold{data frame} that includes the number of people for each
#'      demographic group per year (for projected years) and spatial unit.
#'      The number of rows is the product of all scenarios times all years
#'      times all demographic groups times all spatial units.
#'      The output includes several \bold{identifiers} that indicate to which
#'      scenario, demographic group, year, and spatial unit the results in the
#'      rows refer to:
#'      \item{year}{integer, indicating the projected years.}
#'      \item{scen}{character, indicating the projected scenario(s).}
#'      \item{spatial_unit}{factor, spatial units for which the projection
#'            was run (e.g., canton, districts, municipalities).}
#'      \item{age}{integer, ranging from `0`n to `100` years (including those
#'      older than 100).}
#'      \item{sex}{factor, female (`f`) and male (`m`).}
#'      \item{nat}{factor, indicates if the nationality is Swiss (`ch`) or
#'      international / foreign (`int`). This variable is only returned if
#'      `binational = TRUE`.}
#'      The output also includes columns related to the \bold{size and change
#'      of the population:}
#'      \item{n_jan}{numric, start-of-year population per demographic group.}
#'      \item{n_dec}{numeric, end-of-year population per demographic group.}
#'      \item{delta_n}{numeric, population change per demographic group from
#'      the start to the end of the year in \emph{absolute numbers}.}
#'      \item{delta_perc}{numeric, population change per demographic group from
#'      the start to the end of the year in \emph{percentages}.}
#'      The \bold{components} that are used to project the development of the
#'      population are also included in the output:
#'      \item{births}{numeric, number of births (non-zero values are only
#'      available for age = 0).}
#'      \item{mor_n}{numeric, number of deaths.}
#'      \item{emi_int_n}{numeric, number of people who emigrate
#'      to other countries.}
#'      \item{emi_nat_n}{numeric, number of people who emigrate
#'      to other cantons.}
#'      \item{imm_int_n}{numeric, number of people who immigrate
#'      from other countries.}
#'      \item{imm_nat_n}{numeric, number of people who immigrate
#'      from other cantons.}
#'      \item{acq_n}{numeric, number of people who acquire Swiss citizenship
#'      (only returned if  `binational = TRUE`.)}
#'
#' @export
#'
#' @autoglobal
#'
#' @examples
#' # Run projection for the sample data (whole canton of Aargau)
#' propop(
#'   parameters = fso_parameters,
#'   year_first = 2024,
#'   year_last = 2027,
#'   population = fso_population,
#'   subregional = NULL,
#'   binational = TRUE
#' )
#' propop(
#'   parameters = fso_parameters |>
#'     dplyr::filter(scen == "reference" | scen == "high"),
#'   year_first = 2024,
#'   year_last = 2026,
#'   scenarios = c("reference", "high"),
#'   population = fso_population,
#'   subregional = NULL,
#'   binational = TRUE
#' )
propop <- function(
    parameters,
    population,
    year_first,
    year_last,
    scenarios = NULL,
    age_groups = 101,
    fert_first = 16,
    fert_last = 50,
    share_born_female = 100 / 205,
    subregional = NULL,
    binational = TRUE,
    spatial_unit = "spatial_unit") {
  # Select relevant columns ----
  # Parameters
  parameters <- parameters |>
    select(any_of(c(
      "year", "scen", "spatial_unit", "nat", "sex", "age", "birthrate",
      "int_mothers", "mor", "emi_int", "emi_nat", "emi_sub", "acq", "imm_int_n",
      "imm_nat_n", "imm_sub", "mig_sub"
    )))

  # Population
  population <- population |>
    select(any_of(c("year", "scen", "spatial_unit", "nat", "sex", "age", "n")))

  # Check input ----
  # Check scenarios
  if (!is.null(scenarios)) {
    # If user has defined scenarios manually, order them alphabetically
    scenarios <- sort(unique(scenarios))
  }
  # If scenarios is empty, use all levels of scen as
  if (is.null(scenarios)) {
    # Get all values in scen and order them alphabetically
    scenarios <- sort(unique(parameters$scen))
  }
  # All requested scenarios available in parameters
  assertthat::assert_that(
    all(scenarios %in% parameters$scen),
    msg = paste0(
      "Not all requested scenarios ",
      "are available in `parameters`."
    )
  )

  ## Function arguments ----
  # Convert input in years to integer, results in error if not possible
  year_first <- vctrs::vec_cast(year_first, integer())
  year_last <- vctrs::vec_cast(year_last, integer())
  fert_first <- vctrs::vec_cast(fert_first, integer())
  fert_last <- vctrs::vec_cast(fert_last, integer())

  assertthat::assert_that(is.integer(year_first),
    is.integer(year_last), year_first <= year_last,
    msg = paste0(
      "year_first must be smaller than or",
      "equal to year_last"
    )
  )
  assertthat::assert_that(is.vector(age_groups),
    all(sapply(age_groups, is.numeric)),
    all(!is.na(age_groups)),
    msg = paste0(
      "The argument 'age_groups' must be a vector ",
      "containing only numeric values and no `NA` values."
    )
  )
  assertthat::assert_that(is.integer(fert_first),
    msg = paste0(
      "The argument 'fert_first' must be an integer or ",
      "a numeric value without decimals"
    )
  )
  assertthat::assert_that(is.integer(fert_last),
    msg = paste0(
      "The argument 'fert_last' must be an integer or a ",
      "numeric value without decimals"
    )
  )
  assertthat::assert_that(is.integer(fert_first),
    is.integer(fert_last), fert_first <= fert_last,
    msg = paste0(
      "'fert_first' must be smaller than or ",
      "equal to fert_last"
    )
  )
  assertthat::assert_that(is.numeric(share_born_female),
    msg = "The argument 'share_born_female' must be numeric."
  )
  assertthat::assert_that(is.character(spatial_unit),
    msg = paste0(
      "The argument 'spatial_unit' must be ",
      "of type `character`."
    )
  )

  ## Check years ----
  # Only 1 year in population
  assertthat::assert_that(
    length(unique(population$year)) == 1,
    msg = paste0(
      "The column `year` in `population` must only contain ",
      "one value (i.e., one year)."
    )
  )
  # All requested years available in parameters
  assertthat::assert_that(
    all(year_first:year_last %in% parameters$year),
    msg = paste0(
      "Not all requested years (", year_first, "-", year_last,
      ") are available in `parameters`."
    )
  )

  ## Nationality ----
  # Two groups in column `nat`
  if (binational == TRUE) {
    # Check if column `nat` is present in both, `parameters` and `population`
    # Parameters
    assertthat::assert_that(
      "nat" %in% names(parameters),
      msg = "Column `nat` is missing in `parameters`."
    )

    # Population
    assertthat::assert_that(
      "nat" %in% names(population),
      msg = "Column `nat` is missing in `population`."
    )

    # Check factor levels for nationality
    # Parameters
    assertthat::assert_that(
      length(unique(parameters$nat)) == length(c("int", "ch")) &&
        all(parameters$nat %in% c("int", "ch")),
      msg = paste0(
        "Column `nat` in `parameters` must include the factor levels",
        " `ch` and `int`. \nMissing values (NA), other values, or only one ",
        "factor level are not allowed. ",
        "\nIf demographic groups should not be projected for two",
        " nationalities, \nplease remove the column 'nat' from the data."
      )
    )

    # Population
    assertthat::assert_that(
      length(unique(population$nat)) == length(c("int", "ch")) &&
        all(population$nat %in% c("int", "ch")),
      msg = paste0(
        "Column `nat` in `population` can only include the factor levels",
        " `ch` and `int`. \nMissing values (NA) or only one factor level are",
        " not allowed. \nIf demographic groups should not be projected for two",
        " nationalities, \nplease remove the column 'nat' from the data."
      )
    )

    # Parameters
    # Acquisition of Swiss citizenship in case of two nationalities
    assertthat::assert_that("acq" %in% names(parameters),
      msg = "Column `acq` is missing in parameters."
    )
    # Births by international females in case of two nationalities
    assertthat::assert_that(
      "int_mothers" %in% names(parameters),
      msg = paste0("Column `int_mothers` is missing in parameters.")
    )
  } else if (binational == FALSE) {
    # No distinction between nationalities
    # Check if column `nat` is absent in both, `parameters` and `population`
    # Parameters
    assertthat::assert_that(
      !"nat" %in% names(parameters),
      msg = paste0(
        "Argument `binational` is `FALSE` suggesting that the projection \ndoes",
        " not discriminate between nationalities. \nHowever, `parameters` include",
        " column `nat` suggesting multiple nationalities. \nPlease change argument",
        " `binational` or remove column `nat` from `parameters`."
      )
    )

    # Population
    assertthat::assert_that(
      !"nat" %in% names(population),
      msg = paste0(
        "Column `nat` is present in `population` but not in `parameters`.\n",
        " The presence of column `nat` suggests that the projection should",
        " discriminate \nbetween nationalities. This conflicts with the argument",
        " `binational` = `FALSE`. \nPlease change argument `binational` or remove",
        " column `nat` from `population` data."
      )
    )

    # Parameters
    parameters <- parameters |>
      # duplicate the data and add column `nat` with two levels: "ch" and "int"
      tidyr::expand(tidyr::nesting(!!!syms(names(parameters))), rep = 1:2) |>
      mutate(nat = ifelse(rep == 2, "int", "ch")) |>
      # set all values for "int" at zero
      mutate(across(any_of(
        c(
          "birthrate", "int_mothers", "mor", "emi_int", "emi_nat", "acq",
          "imm_int_n", "imm_nat_n", "emi_sub", "imm_sub"
        )
      ), ~ if_else(nat == "int", 0, .x))) |>
      # add remaining columns `acq` and `int_mothers`
      mutate(acq = 0, int_mothers = 0) |>
      # arrange the data
      select(any_of(c(
        "nat", "sex", "age", "year", "scen", "birthrate", "int_mothers",
        "mor", "emi_int", "emi_nat", "acq", "imm_int_n", "imm_nat_n",
        "emi_sub", "imm_sub", "spatial_unit"
      ))) |>
      arrange(nat, desc(sex), year, spatial_unit)

    # Population
    population <- population |>
      # duplicate the data and add column `nat` with two levels: "ch" and "int"
      tidyr::expand(tidyr::nesting(!!!syms(names(population))), rep = 1:2) |>
      mutate(nat = ifelse(rep == 2, "int", "ch")) |>
      # set all values for "int" at zero
      mutate(n = ifelse(nat == "int", 0, n)) |>
      # arrange the data
      select(year, spatial_unit, nat, sex, age, n) |>
      arrange(nat, desc(sex), year, spatial_unit)
  }

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
    msg = paste0(
      "Column `spatial_unit` is missing in ",
      "parameters."
    )
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
    msg = "Column 'year' in `population` must not
      include any missing values (NA)."
  )
  assertthat::assert_that("spatial_unit" %in% names(population),
    msg = paste0(
      "Column `spatial_unit` is missing ",
      "in population."
    )
  )
  assertthat::assert_that(is.character(population$spatial_unit),
    !any(is.na(population$spatial_unit)),
    msg = paste0(
      "Column 'spatial_unit' in ",
      "`population` must be of type ",
      "`character`. Missing values (NA) are ",
      "not allowed."
    )
  )

  ## Equivalence of spatial_unit in `parameters` and `population` ----
  assertthat::assert_that(
    setequal(
      population$spatial_unit,
      parameters$spatial_unit
    ),
    msg = paste0(
      "The values in column 'spatial unit' ",
      "are not identical in the data frames ",
      "`population` and `parameters`."
    )
  )

  assertthat::assert_that("sex" %in% names(population),
    msg = "Column `sex` is missing in `population`"
  )
  assertthat::assert_that(all(population$sex %in% c("f", "m")),
    msg = paste0(
      "Column `sex` in `population` can",
      " only include the values `f` and `m`.",
      " Missing values (NA) are not allowed."
    )
  )
  assertthat::assert_that("age" %in% names(population),
    msg = "Column `age` is missing in `population`"
  )
  assertthat::assert_that(!any(is.na(population$age)),
    msg = paste0(
      "Column 'age' in `population` must be ",
      "numeric. Missing values (NA) are not",
      " allowed."
    )
  )
  assertthat::assert_that("n" %in% names(population),
    msg = "Column `n` is missing in `population`"
  )
  assertthat::assert_that(is.numeric(population$n),
    !any(is.na(population$n)),
    msg = paste0(
      "Column 'n' in `population` must be ",
      "numeric. Missing values (NA) are not",
      " allowed."
    )
  )

  ## Feedback if non-standard values are used ----
  ### Create vector with non-FSO conform parameters
  non_conform <- c()

  ### Check each condition and add variable name to vector if it fails
  if (age_groups != 101) {
    non_conform <- c(non_conform, "age_groups")
  }

  if (fert_first != 16) {
    non_conform <- c(non_conform, "fert_first")
  }

  if (fert_last != 50) {
    non_conform <- c(non_conform, "fert_last")
  }

  if (share_born_female != 100 / 205) {
    non_conform <- c(non_conform, "share_born_female")
  }

  ### Feedback
  if (!is.null(non_conform)) {
    cli::cli_rule()
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("Some of the provided parameters do not correspond to the
                  standard values suggested by the Federal Statistical Office:")
    cli::cli_alert_warning(cli::col_magenta(
      paste(non_conform, collapse = ", ")
    ))
    cli::cli_alert_info("This may lead to incomplete, unexpected, or wrong
                        results.")
    cli::cli_rule()
  }

  # Prepare projection ----
  # Projection period

  cli::cli_h1("Starting population projection")
  cli::cli_progress_step("Processing...",
                         msg_done = "Processing completed in")

  proj_years <- year_first:year_last
  # filter for years within in the range between year_first and year_last
  parameters <- parameters |> filter(year %in% c(year_first:year_last))
  # Rename n to n_dec in the initial population
  init_population <- population |> rename(n_dec = n)

  # Split parameters by scenario
  list_parameters_scen <- split(parameters, parameters$scen)

  list_out <- lapply(list_parameters_scen, function(parameters_scen) {

    # Split parameters for each scenario into a list by year to iterate across
    list_parameters <- split(parameters_scen, parameters_scen$year)

    # Run projection ----
    # iterate across years
    df_result <- purrr::reduce(
      .x = list_parameters,
      .f = \(population, parameters) project_population(
        population, parameters,
        subregional = subregional
      ),
      .init = init_population
    ) |>
      # remove initial population's year
      filter(year != unique(init_population$year))
  })

  # Combine all groups back into one data frame
  df_result <- do.call(rbind, list_out) |>
    arrange(scen, year, spatial_unit, sex, nat, age) |>
    mutate(
      # calculate the annual change per demographic group
      ## total number of people
      delta_n = round(n_dec - n_jan, 0),
      ## percentage
      delta_perc = round((delta_n / n_jan) * 100, 3),
      # percentages for newborns are NAs
      delta_perc = ifelse(age == 0, NA, delta_perc),
    )

  # Remove row names
  rownames(df_result) <- NULL

  # Format output ----
  # No distinction between nationalities (binational = FALSE)
  if (binational == FALSE) {
    # remove empty rows for `nat` = "int"
    # remove the `nat` and `acq`-columns
    df_result <- df_result |>
      dplyr::filter(nat != "int") |>
      dplyr::select(-any_of(c("nat", "acq")))
  }

  cli::cli_progress_done()

  # Feedback about arguments used
  cli::cli_h1("Settings used for the projection")
  cli::cli_text(
    "Scenario(s): ",
    "{.val {unique(scenarios)}}"
  )
  cli::cli_text(
    "Year of starting population: ",
    "{.val {min(as.numeric(as.character(population$year)))}}"
  )
  cli::cli_text(
    "Number of age groups: ",
    "{.val {age_groups}}"
  )
  cli::cli_text(
    "Fertile period: ",
    "{.val {fert_first}}",
    "-",
    "{.val {fert_last}}"
  )
  cli::cli_text(
    "Share of female newborns: ",
    "{.val {round(share_born_female, digits = 3)}}"
  )
  cli::cli_text(
    "Size of starting population: ",
    "{.val {population |> dplyr:: summarise(sum(n, na.rm = TRUE)) |>
    dplyr::pull()}}"
  )
  cli::cli_text(
    "Projection period: ",
    "{.val {year_first}}",
    "-",
    "{.val {year_last}}"
  )
  cli::cli_text(
    "Nationality-specific projection: ",
    "{.val {if (binational) 'yes' else 'no'}}"
  )
  cli::cli_text(
    "Subregional migration: ",
    "{.val {if (is.null(subregional)) 'no' else 'yes'}}"
  )
  cli::cli_rule()
  cli::cli_text(
    "{.emph Projected} population size by ",
    "{.val {year_last}}: "
  )

  purrr::walk(scenarios, function(scenario) {
    pop_size <- df_result |>
      filter(year == year_last, scen == scenario) |>
      summarise(total = sum(n_dec, na.rm = TRUE)) |>
      pull(total) |>
      round(0)

    cli::cli_text(
      "- Scenario ",
      "{.val {scenario}}",
      ": ",
      "{.emph {.val {pop_size}}}"
    )
  })
  cli::cli_div(theme = list(rule = list("line-type" = "double")))
  cli::cli_rule()

  cli::cli_h1("Please note")

  # Temporary info
  cli::cli_alert_info(
    "As of propop v2.0.0, {.pkg propop()} uses tables instead of matrices to calculate projections.")
  cli::cli_alert_info(
    "The old function is still available as {.pkg propop_legacy()} but won't be further maintained.")
  cli::cli_rule()


  return(df_result)
}
