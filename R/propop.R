#' Project population development (enriched results)
#'
#' @description
#' Wrapper function to project population development using the cohort
#' component method (see e.g., [here](https://www.ag.ch/media/kanton-aargau/dfr/dokumente/statistik/statistische-daten/oeffentliche-statistik/01-bevoelkerung/kantonsdaten/bevoelkerungsprognosen/bevoelkerungsprojektionen-2020-technischer-begleitbericht.pdf)
#' for more details).
#'
#' You can either use your own parameters and starting population or download these
#' data from the Swiss Federal Statistical Office (FSO). For instructions on how
#' to download this information from
#' [STAT-TAB](https://www.bfs.admin.ch/bfs/en/home/services/recherche/stat-tab-online-data-search.html),
#' see \code{vignette("prepare_data", package = "propop")}.
#'

#' For more details on how to use this function to project the population
#' development on the level of a canton, see
#' \code{vignette("run_projections", package = "propop")}.
#'
#' The projection parameters need to be passed to `propop::propop()` as a
#' \bold{single data frame} (with the parameters as columns). The column types,
#' names, and factor levels need to match the specifications listed below under
#' `parameters`:
#'
#' @param parameters data frame containing the FSO rates and numbers to run the
#' projection for a specific spatial level (e.g., canton, municipality).
#'    * `year`, character, projection year.
#'    * `spatial_unit`, character, ID of spatial entity (e.g., canton,
#'    municipality) for which to run the projections.
#'    * `scen`, character, projection scenario, is used to subset data frames
#'    with multiple scenarios (r = reference, l = low growth, h = high growth).
#'    * `nat` \bold{(optional)}, character, nationality (ch = Swiss; int =
#'    foreign / international).
#'    * `sex`, character (f = female, m = male).
#'    * `age`, numeric, typically ranging from 0 to 100 (incl. >100).
#'    * `birthrate`, numeric, number of children per year.
#'    * `int_mothers` \bold{(optional)}, numeric, proportion of children with
#'    Swiss nationality born to non-Swiss mothers.
#'    * `mor`, numeric, prospective mortality rate (probability of death).
#'    * `acq` \bold{(optional)}, numeric, rate of acquisition of Swiss citizenship.
#'    * `emi_int`, numeric, rate of people emigrating abroad.
#'    (number of immigrants - number of emigrants).
#'    * `emi_nat`: rate of people emigrating to other cantons.
#'    * `imm_int_n`, numeric, number of people immigrating from abroad.
#'    * `imm_nat_n`: number of people immigrating from other cantons.
#'    * `mig_sub` \bold{(optional)}, numeric, within canton net migration. Useful
#'    to account for movements between different subregions (e.g., municipalities).
#'
#' @param population data frame including the starting population of each
#' demographic group. Possible values are the same as in `parameters` (apart
#' from year). The data frame only includes one year, usually the one preceding
#' the first projected year.
#'    * `year` character, should be `year_first` - 1.
#'    * `spatial_unit` character.
#'    * `nat` character.
#'    * `sex` character.
#'    * `age` numeric.
#'    * `n` numeric, number of people per demographic group.
#'
#' @param year_first numeric, first year to be projected.
#' @param year_last numeric, last year to be projected.
#' @param age_groups numeric, number of age classes. Creates a vector with
#'        1-year age classes running from `0` to (`age_groups` - 1). Defaults to
#'        `101` (FSO standard number of age groups).
#' @param fert_first numeric, first year of female fertility. Defaults to 16
#'        (FSO standard value).
#' @param fert_last numeric, last year of female fertility. Defaults to 50
#'        (FSO standard value).
#' @param share_born_female numeric, fraction of female babies. Defaults to
#'        100 / 205 (FSO standard value).
#' @param subregional boolean, `TRUE` indicates that subregional migration
#'        patterns (e.g., movement between municipalities within a canton)
#'        are part of the projection. Requires input (parameters and population)
#'        on the level of subregions.
#' @param binational boolean, `TRUE` indicates that projections discriminate
#'        between two groups of nationalities. `FALSE` indicates that only one
#'        projection is run without distinguishing between nationalities.
#' @param spatial_unit character, name of variable containing the names of the
#'        region or subregions for which the projection shall be performed.
#'
#' @returns
#' Returns a data frame that includes the number of people for each demographic
#'      group per year (for the starting year and each projected year). The
#'      number of rows is the product of all years times all demographic groups.
#'      The output includes several \bold{identifiers} that indicate to which
#'      demographic group and year the results in the rows refer to.
#'      \item{year}{integer, indicating starting year / projected years.}
#'      \item{spatial_unit}{factor, spatial units for which the projection
#'            was run (e.g., canton, municipalities, districts).}
#'      \item{age}{integer.}
#'      \item{sex}{factor, female (f) and male (m).}
#'      \item{nat}{factor, indicates if the nationality is Swiss (ch) or
#'      international / foreign (int). This variable is only returned if
#'      `binational = TRUE`.}
#'      The output also includes columns related to the \bold{size and change
#'      of the population:}
#'       \item{n}{numric, end-of-year population per demographic group.}
#'      \item{n_1}{numeric, number of people of the particular
#'      demographic group by the end of the following year.}
#'      \item{delta_n}{numeric, population change per demographic group from
#'      current to next year in absolute numbers.}
#'      \item{delta_perc}{numeric, population change per demographic group from
#'      current to next year in percentages.}
#'      The \bold{components} that are used to project the development of the population
#'      are also included in the output:
#'      \item{births}{numeric, number of births (values only available for
#'      age = 0).}
#'      \item{mor}{numeric, number of deaths.}
#'      \item{emi_int}{numeric, number of people who emigrate
#'      to other countries.}
#'      \item{emin_nat}{numeric, number of people who emigrate
#'      to other cantons.}
#'      \item{imm_int}{numeric, number of people who immigrate
#'      from other countries.}
#'      \item{imm_nat}{numeric, number of people who immigrate
#'      from other cantons.}
#'      \item{acq}{numeric, number of people who acquire Swiss citizenship.}
#'
#' @export
#'
#' @examples
#' # Run projection for the sample data (whole canton of Aargau)
#' propop(
#'   parameters = fso_parameters,
#'   year_first = 2019,
#'   year_last = 2022,
#'   population = fso_population,
#'   subregional = FALSE,
#'   binational = TRUE
#' )
propop <- function(
    parameters,
    population,
    year_first,
    year_last,
    age_groups = 101,
    fert_first = 16,
    fert_last = 50,
    share_born_female = 100 / 205,
    subregional = FALSE,
    binational = TRUE,
    spatial_unit = "spatial_unit") {
  # Check input ----
  # Select relevant columns
  parameters <- parameters |>
    select(any_of(c(
      "nat", "sex", "age", "year", "scen", "spatial_unit", "birthrate",
      "int_mothers", "mor", "emi_int", "emi_nat", "imm_int_n", "imm_nat_n",
      "acq", "mig_sub"
    )))
  population <- population |>
    select(any_of(c("year", "spatial_unit", "nat", "sex", "age", "n")))
  # Only 1 year in population
  assertthat::assert_that(
    length(unique(population$year)) == 1,
    msg = paste0("The column `year` in `population` must only contain ",
                 "one value (i.e., one year).")
  )
  # All requested years available in parameters
  assertthat::assert_that(
    all(year_first:year_last %in% parameters$year),
    msg = paste0("Not all requested years (", year_first, "-", year_last,
                 ") are available in `parameters`.")
  )

  # Nationality
  # Case 1: Two groups in column `nat`
  if (binational == TRUE) {
    # Check if column `nat` is present in both, `parameters` and `population`
    # Parameters
    assertthat::assert_that("nat" %in% names(parameters),
      msg = "Column `nat` is missing in `parameters`."
    )

    # Population
    assertthat::assert_that("nat" %in% names(population),
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
    assertthat::assert_that("int_mothers" %in% names(parameters),
      msg = paste0("Column `int_mothers` is missing in parameters.")
    )

    # Arrange columns
    parameters <- parameters |> arrange(nat, desc(sex), year, spatial_unit)

    population <- population |> arrange(nat, desc(sex), year, spatial_unit)

  } else if (binational == FALSE) {
    # Case 2: No distinction between nationalities
    # Check if column `nat` is absent in both, `parameters` and `population`
    # Parameters
    assertthat::assert_that(!"nat" %in% names(parameters),
      msg = paste0(
        "Argument `binational` is `FALSE` suggesting that the projection \ndoes",
        " not discriminate between nationalities. \nHowever, `parameters` include",
        " column `nat` suggesting multiple nationalities. \nPlease change argument",
        " `binational` or remove column `nat` from `parameters`."
      )
    )

    # Population
    assertthat::assert_that(!"nat" %in% names(population),
      msg = paste0(
        "Column `nat` is present in `population` but not in `parameters`.\n",
        " The presence of column `nat` suggests that the projection should",
        " discriminate \nbetween nationalities. This conflicts with the argument",
        " `binational` = `FALSE`. \nPlease change argument `binational` or remove",
        " column `nat` from `population` data."
      )
    )

    # Create required data structure for project_raw()
    # Parameters
    parameters <- parameters |>
      # duplicate the data and add column `nat` with two levels: "ch" and "int"
      tidyr::expand(tidyr::nesting(!!!syms(names(parameters))), rep = 1:2) |>
      mutate(nat = ifelse(rep == 2, "int", "ch")) |>
      # set all values for "int" at zero
      mutate(across(any_of(
        c("birthrate", "int_mothers", "mor", "emi_int", "emi_nat", "acq",
          "imm_int_n", "imm_nat_n", "mig_sub"
        )), ~if_else(nat == "int", 0, .x))
      ) |>
      # add remaining columns `acq` and `int_mothers`
      dplyr::mutate(acq = 0, int_mothers = 0) |>
      # arrange the data
      select(any_of(c(
        "nat", "sex", "age", "year", "scen", "birthrate", "int_mothers",
        "mor", "emi_int", "emi_nat", "acq", "imm_int_n", "imm_nat_n",
        "mig_sub", "spatial_unit"
      ))) |>
      arrange(nat, desc(sex), year, spatial_unit)

    # Population
    population <- population |>
      # duplicate the data and add column `nat` with two levels: "ch" and "int"
      tidyr::expand(tidyr::nesting(!!!syms(names(population))), rep = 1:2) |>
      mutate(nat = ifelse(rep == 2, "int", "ch")) |>
      # set all values for "int" at zero
      mutate(n = case_when(nat == "int" ~ 0, TRUE ~ n)) |>
      # arrange the data
      select(year, spatial_unit, nat, sex, age, n) |>
      arrange(nat, desc(sex), year, spatial_unit)
  }


  ## Only 1 value in scenario ----
  assertthat::assert_that(
    n_distinct(parameters$scen) == 1,
    msg = "The 'scen' column in the 'parameters' data frame must contain the
    identical value in all rows (either reference, high, or low)."
  )
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
  if (subregional == TRUE) {
    assertthat::assert_that("mig_sub" %in% names(parameters),
      msg = "Column `mig_sub` is missing in parameters."
    )
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


  ## Remaining arguments ----
  # Convert input in years to integer, results in error if not possible
  year_first <- vctrs::vec_cast(year_first, integer())
  year_last <- vctrs::vec_cast(year_last, integer())
  fert_first <- vctrs::vec_cast(fert_first, integer())
  fert_last <- vctrs::vec_cast(fert_last, integer())

  assertthat::assert_that(is.integer(year_first),
    dplyr::between(year_first, 2018, 2050),
    msg = paste0(
      "`year_first` must be an integer or a numeric ",
      "value without decimals between 2018 and 2050"
    )
  )
  assertthat::assert_that(
    is.integer(year_last), dplyr::between(year_last, 2018, 2050),
    msg = paste0(
      "`year_last` must be an integer or a numeric value without decimals",
      " between 2018 and 2050"
    )
  )
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
      "fert_first must be smaller than or ",
      "equal to fert_last"
    )
  )
  assertthat::assert_that(is.numeric(share_born_female),
    msg = "The argument 'share_born_female' must be numeric."
  )
  assertthat::assert_that(is.logical(subregional),
    msg = paste0(
      "The argument 'subregional' must ",
      "either be `TRUE` or `FALSE`."
    )
  )
  assertthat::assert_that(is.character(spatial_unit),
    msg = paste0(
      "The argument 'spatial_unit' must be ",
      "of type `character`."
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

  # Run projection (for all projection units) ----
  projection_raw <-
    purrr::map_df(
      .x = parameters |>
        dplyr::select(spatial_unit) |>
        dplyr::distinct() |>
        dplyr::pull(),
      .f = ~ project_raw(
        parameters = parameters |> filter(spatial_unit == .x),
        year_last = year_last,
        year_first = year_first,
        age_groups = age_groups,
        fert_first = fert_first,
        fert_last = fert_last,
        share_born_female = share_born_female,
        n = population |>
          dplyr::filter(!!sym(spatial_unit) == .x) |>
          dplyr::pull(n),
        subregional = subregional
      )
    )


  # Prepare empty data frame with meta data ----
  skeleton <- prepare_skeleton(
    age_groups = age_groups,
    year_first = year_first,
    year_last = year_last,
    spatial_unit = parameters |>
      dplyr::select(spatial_unit) |>
      dplyr::distinct() |>
      dplyr::pull()
  )


  # Add meta data to raw results ----
  projection_results <- complement_projection(
    skeleton = skeleton,
    projection_raw = projection_raw,
    subregional = subregional
  )

  # Format output for case 2: No distinction between nationalities
  # (argument `binational`= FALSE)
  if (binational == FALSE) {
      # remove empty rows for `nat` = "int"
      # remove the `nat` and `acq`-columns
      projection_results <- projection_results |>
        dplyr::filter(nat != "int") |>
        dplyr::select(-any_of(c("nat" , "acq")))
  }

  # Format output if subregional == FALSE
  if (subregional == FALSE) {
    # remove the `mig_sub`column (otherwise is filled with zeros if present)
    projection_results <- projection_results |>
      dplyr::select(-any_of(c("mig_sub")))
  }

  # Feedback about arguments used
  cli::cli_h1("Settings used for the projection")
  cli::cli_text(
    "Year of starting population: ",
    "{.val {min(as.numeric(as.character(population$year)))}}")
  cli::cli_text(
    "Size of starting population: ",
    "{.val {population |> dplyr:: summarise(sum(n, na.rm = TRUE)) |>
    dplyr::pull()}}")
  cli::cli_text(
    "Projection period: ",
    "{.val {year_first}}",
    "-",
    "{.val {year_last}}")
  cli::cli_text(
    "{.emph Projected} population size (",
    "{.val {year_last}}): ",
    "{.emph {.val {projection_results |>
    dplyr::filter(year == year_last) |>
    dplyr:: summarise(sum(n_jan, na.rm = TRUE)) |>
    dplyr::pull() |> round(digits = 0)}}}")
  cli::cli_text(
    "Nationality-specific projection: ",
    "{.val {if (binational) 'yes' else 'no'}}")
  cli::cli_text(
    "Subregional migration: ",
    "{.val {if (subregional) 'yes' else 'no'}}")
  cli::cli_rule()


  if (subregional == FALSE) {
    population |> dplyr::reframe(n, .by = year)
  } #else if (subregional == TRUE) {
  #   population |> dplyr::reframe(n, .by = c("year", "spatial_unit"))
  # }

  return(projection_results)
}
