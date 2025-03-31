#' Project population development
#'
#' @description
#' Wrapper function to project population development using the cohort
#' component method (see e.g., [here](https://www.ag.ch/media/kanton-aargau/dfr/dokumente/statistik/statistische-daten/oeffentliche-statistik/01-bevoelkerung/kantonsdaten/bevoelkerungsprognosen/bevoelkerungsprojektionen-2020-technischer-begleitbericht.pdf)
#' for more details).

propop_tables <- function(
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
  # Select relevant columns ----
  # Parameters
  parameters <- parameters |>
    select(any_of(c(
      "year", "spatial_unit", "scen", "nat", "sex", "age", "birthrate",
      "int_mothers", "mor", "emi_int", "emi_nat", "imm_int_n", "imm_nat_n",
      "acq", "mig_sub"
    )))

  # Population
  population <- population |>
    select(any_of(c("year", "spatial_unit", "scen", "nat", "sex", "age", "n")))

  # Check input ----
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

    # Create required data structure for project_raw()
    # Parameters
    parameters <- parameters |>
      # duplicate the data and add column `nat` with two levels: "ch" and "int"
      tidyr::expand(tidyr::nesting(!!!syms(names(parameters))), rep = 1:2) |>
      mutate(nat = ifelse(rep == 2, "int", "ch")) |>
      # set all values for "int" at zero
      mutate(across(any_of(
        c(
          "birthrate", "int_mothers", "mor", "emi_int", "emi_nat", "acq",
          "imm_int_n", "imm_nat_n", "mig_sub"
        )
      ), ~ if_else(nat == "int", 0, .x))) |>
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
  } else if (subregional == FALSE) {
    parameters <- parameters |>
      # set subregional to null
      mutate(mig_sub = case_when(subregional == TRUE ~ subregional, .default = 0))
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

  ## Progress feedback ----
  cli::cli_text(
    "Running projection for: {.val { parameters |>",
    "dplyr::select(spatial_unit) |> dplyr::distinct()}}"
  )

  # Prepare projection ----
  # Projection period
  proj_years <- year_first:year_last
  # filter for years within in the range between year_first and year_last
  parameters <- parameters |> filter(year %in% c(year_first:year_last))
  # Rename n to n_dec in the initial population
  init_population <- population |> rename(n_dec = n)
  # Levels for subregional levels (spatial units)
  subregional_levels <- parameters |>
    dplyr::select(spatial_unit) |>
    dplyr::distinct() |>
    dplyr::pull()

  # Split parameters into a list to iterate across
  list_parameters <-
    # split parameters by year and spatial unit
    # split(parameters, list(parameters$year, parameters$spatial_unit)) |>
    split(parameters, parameters$spatial_unit) |>
    # years as names for list elements
    rlang::set_names(~ paste0("parameters_", .))

  split_list <- purrr::map(list_parameters, ~ split(.x, .x$year))

  # check if same order as split list
  init_list <- split(init_population, init_population$spatial_unit)

  # Run projection ----
  # iterate across spatial units and years
  df_result <- purrr::map2_df(
    .x = split_list,
    .y = init_list,
    ~ purrr::reduce(
      ..1,
      ~ project_population(
        population = ..1,
        parameters = ..2
      ),
      .init = ..2
    )
  ) |>
    # remove initial population's year
    filter(year != unique(init_population$year))

  # Format output ----
  # No distinction between nationalities (binational = FALSE)
  if (binational == FALSE) {
    # remove empty rows for `nat` = "int"
    # remove the `nat` and `acq`-columns
    df_result <- df_result |>
      dplyr::filter(nat != "int") |>
      dplyr::select(-any_of(c("nat", "acq")))
  }

  # Only one spatial_unit (subregional = FALSE)
  if (subregional == FALSE) {
    # remove the `mig_sub`column (otherwise is filled with zeros if present)
    df_result <- df_result |>
      dplyr::select(-any_of(c("mig_sub")))
  }

  # Feedback about arguments used ----
  cli::cli_h1("Settings used for the projection")
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
    "{.emph Projected} population size (",
    "{.val {year_last}}): ",
    "{.emph {.val {df_result |>
    dplyr::filter(year == year_last) |>
    dplyr:: summarise(sum(n_jan, na.rm = TRUE)) |>
    dplyr::pull() |> round(digits = 0)}}}"
  )
  cli::cli_text(
    "Nationality-specific projection: ",
    "{.val {if (binational) 'yes' else 'no'}}"
  )
  cli::cli_text(
    "Subregional migration: ",
    "{.val {if (subregional) 'yes' else 'no'}}"
  )
  cli::cli_rule()

  if (subregional == FALSE) {
    population |> dplyr::reframe(n, .by = year)
  }

  return(df_result)
}
