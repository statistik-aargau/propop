#' Get projection parameters from FSO
#'
#' @name get_parameters
#'
#' @description
#' Users who do not have the mandatory projection parameters for
#' `propop::propop()` can use this convenience function to download them from
#' the Federal Statistical Office (FSO). The parameters are only available
#' on the level of cantons. For smaller-scale projections, the parameters must
#' be scaled down.
#' In addition to the parameters, the function also returns the projected
#' population (i.e., number of people estimated in the FSO model released in 2025).
#' All parameters and projections are from the
#' [FSO model published in 2025](https://www.bfs.admin.ch/bfs/en/home/statistics/population/population-projections/national-projections.html).
#' The parameters are available for the years 2024-2055.
#' The variables `int_mothers` and `mig_nat_n` are not directly available from
#' the FSO. They are calculated within this function.
#'
#' To get projection parameters, you must use the spelling defined in the
#' corresponding FSO table. See
#' \code{vignette("prepare_data", package = "propop")}.
#'
#' Changes to the API interface may break this function. If problems occur,
#' we recommend following the step-by-step procedure described in
#' \code{vignette("prepare_data", package = "propop")}.
#'
#' @return A data frame with the rates and number of people from the
#' [Federal Statistical Office (FSO)](https://www.bfs.admin.ch/bfs/en/home.html)
#' required to project the population development of the requested spatial
#' entities. For each of the four demographic groups (nationality x sex),
#' there are 101 age classes, resulting in a total of 404 rows per requested
#' year and spatial unit.
#'
#' @section Demographic groups:
#' The returned data frame includes parameters for each unique combination of
#' the following demographic variables:
#'   * `nat`: ch = Swiss; int = foreign / international.
#'   * `sex`: f = female, m = male.
#'   * `age`: 101 one-year age classes, ranging from 0 to 100 (including those
#'   older than 100).
#'   * `start_n`: numeric, number of people in the corresponding demographic
#'   group on 1st of January.
#'
#' @section Parameters:
#' The following parameters are included in the returned data frame:
#'    * `year`: numeric, year of projection.
#'    * `scen`: character, projection scenario.
#'    * `spatial_unit`: character, indicating the user requested spatial
#'      unit(s).
#'    * `birthrate`: numeric, total number of live human births per 1,000
#'      inhabitants.
#'      (formerly `birth_rate`).
#'    * `int_mothers`: numeric, proportion of children with Swiss nationality
#'       born to non-Swiss mothers
#'       (formerly `births_int_ch`).
#'    * `mor`: numeric, prospective mortality (probability of death).
#'    * `emi_int`: numeric, rate of people emigrating to other countries
#'      (formerly `emi`).
#'    * `emi_nat`: numeric, rate of people emigrating to other cantons
#'      (new parameter).
#'    * `acq`: numeric, rate of acquisition of Swiss citizenship.
#'    * `imm_int_n`: numeric, number of people immigrating from abroad
#'      (formelry `imm_int`).
#'    * `imm_nat_n`: numeric, number of people immigrating from other cantons
#'      (new parameter).
#'    * `emi_nat_n`: numeric, number of people emigrating to other cantons
#'      (parameter previously used to compute `mig_nat_n`).
#'    * `mig_nat_n`: numeric, national / inter-cantonal net migration
#'      (number of immigrants minus number of emigrants).
#'      (formerly `mig_ch`, will soon be obsolete and removed).

#'
#' @section Projected population:
#' `n_projected` is the the number of people per demographic group and year on
#' December 31 (as projected by the FSO in the 2020 model).
#'
#' @section Details about calculated variables:
#'
#' `births_int_ch` is calculated by dividing the number of live newborns with
#' Swiss citizenship born to non-Swiss mothers by the number of all live
#' newborns born to non-Swiss mothers.
#'
#' `mig_ch` is calculated as the difference between the immigration from other
#' cantons and the emigration to other cantons.
#'
#' @param number_fso_ref character, px-x table ID for number parameters
#' (reference scenario), defaults to "px-x-0104020000_101".
#'
#' @param number_fso_high character, px-x table ID for number parameters
#' (high growth scenario), defaults to "px-x-0104020000_102".
#'
#' @param number_fso_low character, px-x table ID for for number parameters
#' (low growth scenario, defaults to "px-x-0104020000_103".
#'
#' @param number_fso_rates character, px-x table ID for rate parameters,
#' defaults to "px-x-0104020000_109".
#'
#' @param number_fso_births character, px-x table ID required to compute the
#' share of Swiss newborns from non-Swiss mothers,
#' defaults to "px-x-0104020000_106".
#'
#' @param year_first numeric,  first year for which the parameters and
#' projections are to be downloaded.
#'
#' @param year_last numeric, last year for which the parameters and projections
#' are to be downloaded.
#'
#' @param spatial_units character vector, indicating at least one spatial
#' entity for which the projection will be run. Typically a canton.
#'
#' @source
#'   Data obtained from the Swiss Federal Statistical Office (FSO):
#'   - <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_101/-/px-x-0104020000_101.px/>
#'   - <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_102/-/px-x-0104020000_102.px>
#'   - <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_103/-/px-x-0104020000_103.px/>
#'   - <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_106/-/px-x-0104020000_106.px/>
#'   - <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_109/-/px-x-0104020000_109.px/>
#'
#' @autoglobal
#'
#' @export
#'
#' @examplesIf (Sys.getenv("RUN_EXPENSIVE_TESTS") == "true")
#' \dontrun{
#' one_canton <- get_parameters(
#'   year_first = 2024,
#'   year_last = 2055,
#'   spatial_units = "Aargau"
#' )
#' two_cantons_4years <- get_parameters(
#'   year_first = 2025,
#'   year_last = 2028,
#'   spatial_units = c("Aargau", "Zug")
#' )
#' }
get_parameters <- function(number_fso_ref = "px-x-0104020000_101",
                           number_fso_high = "px-x-0104020000_102",
                           number_fso_low = "px-x-0104020000_103",
                           number_fso_rates = "px-x-0104020000_109",
                           number_fso_births = "px-x-0104020000_106",
                           year_first,
                           year_last,
                           spatial_units) {
  # Test input
  # convert input in years to integer, results in error if not possible
  year_first <- vctrs::vec_cast(year_first, integer())
  year_last <- vctrs::vec_cast(year_last, integer())

  assertthat::assert_that(
    is.integer(year_first),
    msg = paste0(
      "`year_first` must be an integer or a numeric value without decimals"
    )
  )
  assertthat::assert_that(
    is.integer(year_last),
    msg = paste0(
      "`year_last` must be an integer or a numeric value without decimals"
    )
  )
  assertthat::assert_that(
    is.integer(year_first),
    is.integer(year_last), year_first <= year_last,
    msg = "year_first must be smaller than or equal to year_last"
  )
  assertthat::assert_that(
    is.vector(spatial_units),
    length(spatial_units) > 0,
    any(sapply(spatial_units, is.character)),
    msg = paste0(
      "The argument 'spatial_unit' must be a non-empty vector with at least",
      " one character elementof type `character`."
    )
  )

  cli::cli_progress_step("Preparing number parameters")

  # Get "number of people" parameters ----
  ##  Prepare meta data to specify what to download ----
  metadata <- BFS::bfs_get_metadata(
    number_bfs = number_fso_ref # "px-x-0104020000_101")
  )
  metadata_tidy_ref <- metadata |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = metadata |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  # Meta data for version high
  metadata_high <- BFS::bfs_get_metadata(
    number_bfs = number_fso_high # "px-x-0104020000_102"
  )
  metadata_tidy_high <- metadata_high |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = metadata |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  # Ensure structue ref-high is identical
  assertthat::assert_that(
    identical(metadata_tidy_ref[, -ncol(metadata_tidy_ref)],
              metadata_tidy_high[, -ncol(metadata_tidy_high)]),
    msg = paste0(
      "The metadata of the following px objects differ: ",
      "px-x-0104020000_101 (reference) & ",
      "px-x-0104020000_102 (high). ",
      "Check if the FSO has changed the structure of the data cube."
    )
  )

  # Meta data for version low
  metadata_low <- BFS::bfs_get_metadata(
    number_bfs = number_fso_low # "px-x-0104020000_103")
  )
  metadata_tidy_low <- metadata_low |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = metadata |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  # Ensure structue ref-low is identical
  assertthat::assert_that(
    identical(metadata_tidy_ref[, -ncol(metadata_tidy_ref)],
              metadata_tidy_low[, -ncol(metadata_tidy_low)]),
    msg = paste0(
      "The metadata of the following px objects differ: ",
      "px-x-0104020000_101 (reference) & ",
      "px-x-0104020000_103 (low). ",
      "Check if the FSO has changed the structure of the data cube."
    )
  )

  # Check if spatial units are available in cubes 101-103
  assertthat::assert_that(
    all(spatial_units
        %in%
          metadata_tidy_ref$valueTexts[
            metadata_tidy_ref$text ==
              "Kanton"]),
    msg = paste0("At least one of the requested spatial units is not available.",
                 " Check the spelling against those in the STATTAB cubes ",
                 number_fso_ref, " / ", number_fso_high, " / ", number_fso_low,
                 ". Inspecting the column 'valueTexts' in the following package ",
                 "data may help to identify the correct spelling: ",
                 "data('stattab_101_snap') / ",
                 "data('stattab_102_snap') / ",
                 "data('stattab_103_snap')"))


  # Specify the elements to download
  dim1 <- metadata_tidy_ref |>
    dplyr::filter(
      text == "Kanton" & # Canton
        valueTexts %in% spatial_units
    )

  dim2 <- metadata_tidy_ref |>
    dplyr::filter(
      text == "Geschlecht" & # sex
        valueTexts %in% c("Mann", "Frau")
    )

  dim3 <- metadata_tidy_ref |>
    dplyr::filter(
      text == "Alter" & # age
        !(valueTexts %in% "Alter - Total")
    ) # exclude "Total"

  dim4 <- metadata_tidy_ref |>
    dplyr::filter(
      text == "Jahr" & # get years
        valueTexts %in% year_first:year_last
    )

  dim5 <- metadata_tidy_ref |>
    dplyr::filter(
      # "Staatsangehörigkeit (Kategorie)" & # nationality
      text == stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ) & valueTexts %in% c("Schweiz", "Ausland")
    )

  dim6 <- metadata_tidy_ref |>
    dplyr::filter(text == "Beobachtungseinheit" & # type of parameter types
                    valueTexts %in% c(
                      stringi::stri_unescape_unicode(
                        "Bev\\u00f6lkerungsstand am 1. Januar"
                      ),
                      "Einwanderungen",
                      "Auswanderungen",
                      "Interkantonale Zuwanderungen",
                      "Interkantonale Abwanderungen",
                      stringi::stri_unescape_unicode(
                        "Bev\\u00f6lkerungsstand am 31. Dezember"
                      )
                    ))

  # build dimensions list object
  dimensions <- list(
    dim1$values,
    dim2$values,
    dim3$values,
    dim4$values,
    dim5$values,
    dim6$values
  )

  # add names
  names(dimensions) <- c(
    unique(dim1$code),
    unique(dim2$code),
    unique(dim3$code),
    unique(dim4$code),
    unique(dim5$code),
    unique(dim6$code)
  )


  # download number of people parameters

  cli::cli_progress_step("Downloading number parameters (reference scenario)")

  ## Get px-data for reference scenario ----
  fso_numbers_r <- BFS::bfs_get_data(
    number_bfs = number_fso_ref, # "px-x-0104020000_101",
    query = dimensions
  )

  # Check structure
  assertthat::assert_that(is.data.frame(fso_numbers_r))
  assertthat::assert_that(ncol(fso_numbers_r) == 7)

  # Column names
  expected_names <- c(
    "Kanton",
    stringi::stri_unescape_unicode("Staatsangeh\\u00f6rigkeit (Kategorie)"),
    "Geschlecht",
    "Alter",
    "Jahr",
    "Beobachtungseinheit"
  )

  # Check the first 6 columns by name
  assertthat::assert_that(all(names(fso_numbers_r)[1:6] == expected_names))

  # Check if column 7 starts with "Szenarien zur" and includes numeric values
  assertthat::assert_that(startsWith(names(fso_numbers_r)[7], "Szenarien zur"))
  assertthat::assert_that(is.numeric(fso_numbers_r[[7]]))

  # Rename column for consistency and easier handling
  fso_numbers_r <- fso_numbers_r |>
    dplyr::rename(value = 7)|>
    dplyr::mutate(scen = "reference")

  # Assert that "Jahr" contains no NAs and matches required values exactly
  assertthat::assert_that(
    is.character(fso_numbers_r$Jahr),
    all(!is.na(fso_numbers_r$Jahr)),
    setequal(fso_numbers_r$Jahr, as.character(year_first:year_last))
  )

  # End of reference scenario

  # Initiating scenario "high"
  cli::cli_progress_step(
    "Downloading number parameters (high growth scenario)"
  )

  ## Get px-data for scenario "high" ----
  fso_numbers_h <- BFS::bfs_get_data(
    number_bfs = number_fso_high, # "px-x-0104020000_102",
    query = dimensions
  )

  # Check structure
  assertthat::assert_that(is.data.frame(fso_numbers_h))
  assertthat::assert_that(ncol(fso_numbers_h) == 7)

  # Column names
  expected_names <- c(
    "Kanton",
    stringi::stri_unescape_unicode("Staatsangeh\\u00f6rigkeit (Kategorie)"),
    "Geschlecht",
    "Alter",
    "Jahr",
    "Beobachtungseinheit"
  )

  # Check the first 6 columns by name
  assertthat::assert_that(all(names(fso_numbers_h)[1:6] == expected_names))

  # Check if column 7 starts with "Szenarien zur" and includes numeric values
  assertthat::assert_that(startsWith(names(fso_numbers_h)[7], "Szenarien zur"))
  assertthat::assert_that(is.numeric(fso_numbers_h[[7]]))

  # rename column for consistency and easier handling
  fso_numbers_h <- fso_numbers_h |>
    dplyr::rename(value = 7)|>
    dplyr::mutate(scen = "high")

  # Assert that "Jahr" contains no NAs and matches required values exactly
  assertthat::assert_that(
    is.character(fso_numbers_h$Jahr),
    all(!is.na(fso_numbers_h$Jahr)),
    setequal(fso_numbers_h$Jahr, as.character(year_first:year_last))
  )

  # End of scenario "high"

  # Initiating scenario "low"

  cli::cli_progress_step(
    "Downloading number parameters (low growth scenario)"
  )

  ## Get px-data for scenario "low" ----
  fso_numbers_l <- BFS::bfs_get_data(
    number_bfs = number_fso_low, # "px-x-0104020000_103",
    query = dimensions
  )

  # Check structure
  assertthat::assert_that(is.data.frame(fso_numbers_l))
  assertthat::assert_that(ncol(fso_numbers_l) == 7)

  # Column names
  expected_names <- c(
    "Kanton",
    stringi::stri_unescape_unicode("Staatsangeh\\u00f6rigkeit (Kategorie)"),
    "Geschlecht",
    "Alter",
    "Jahr",
    "Beobachtungseinheit"
  )

  # Check the first 6 columns by name
  assertthat::assert_that(all(names(fso_numbers_l)[1:6] == expected_names))

  # Check if column 7 starts with "Szenarien zur" and includes numeric values
  assertthat::assert_that(startsWith(names(fso_numbers_l)[7], "Szenarien zur"))
  assertthat::assert_that(is.numeric(fso_numbers_l[[7]]))

  # rename column for consistency and easier handling
  fso_numbers_l <- fso_numbers_l |>
    dplyr::rename(value = 7)|>
    dplyr::mutate(scen = "low")

  # Assert that "Jahr" contains no NAs and matches required values exactly
  assertthat::assert_that(
    is.character(fso_numbers_l$Jahr),
    all(!is.na(fso_numbers_l$Jahr)),
    setequal(fso_numbers_l$Jahr, as.character(year_first:year_last))
  )

  # End of scenario "low"


  # combine into single data frame
  fso_numbers_raw <- dplyr::full_join(fso_numbers_r, fso_numbers_h) |>
    dplyr::full_join(fso_numbers_l)


  # Bring variable names and factor levels into the format required later
  fso_numbers <- fso_numbers_raw |>
    dplyr::rename(
      nat = stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ), # "Staatsangehörigkeit (Kategorie)",
      sex = Geschlecht,
      age = Alter,
      year = Jahr,
      fso_parameter = Beobachtungseinheit
    ) |>
    # change factor levels
    dplyr::mutate(
      fso_parameter = dplyr::case_match(
        fso_parameter,
        stringi::stri_unescape_unicode(
          "Bev\\u00f6lkerungsstand am 1. Januar"
        ) ~ "start_n",
        "Auswanderungen" ~ "emi_n",
        stringi::stri_unescape_unicode(
          "Bev\\u00f6lkerungsstand am 31. Dezember"
        ) ~ "fso_projection_n",
        "Einwanderungen" ~ "imm_int_n",
        "Interkantonale Abwanderungen" ~ "emi_nat_n",
        "Interkantonale Zuwanderungen" ~ "imm_nat_n"
      ),
      nat = dplyr::case_match(
        nat,
        "Schweiz" ~ "ch",
        "Ausland" ~ "int"
      ),
      sex = dplyr::case_when(
        sex == "Mann" ~ "m",
        sex == "Frau" ~ "f"
      ),
      age = as.numeric(stringr::str_extract(age, "\\d+"))
    )

  cli::cli_progress_step("Preparing download of rate parameters")

  # Get "rate and probability" parameters ----
  # Get meta data to determine what to download
  metadata <- BFS::bfs_get_metadata(
    number_bfs = number_fso_rates # "px-x-0104020000_109")
  )

  metadata_tidy_109 <- metadata |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = metadata |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())


  # Check if spatial units are available
  assertthat::assert_that(
    all(spatial_units
        %in%
          metadata_tidy_109$valueTexts[
            metadata_tidy_109$text ==
              "Kanton"]) ,
    msg = paste0("At least one of the requested spatial units is not available.",
                 " Check the spelling against those in the STATTAB cube ",
                 number_fso_rates, ". Inspecting the column 'valueTexts' in the",
                 " following package data may help to identify the correct",
                 "spelling: data('stattab_109_snap')"
    ))

  # Specify the elements to download
  dim1 <- metadata_tidy_109 |>
    dplyr::filter(
      text == "Kanton" & # Canton
        valueTexts %in% spatial_units
    )
  dim2 <- metadata_tidy_109 |>
    dplyr::filter(
      text == "Szenario-Variante" & # sex
        valueTexts %in% c(
          "Referenzszenario AR-00-2025", "'hohes' Szenario BR-00-2025",
          "'tiefes' Szenario CR-00-2025"
        )
    )
  dim3 <- metadata_tidy_109 |>
    dplyr::filter(
      text == stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ), # "Staatsangehörigkeit (Kategorie)" & # nationality
      valueTexts %in% c("Schweiz", "Ausland")
    )
  dim4 <- metadata_tidy_109 |>
    dplyr::filter(text == "Geschlecht" & valueTexts %in% c("Mann", "Frau")) # sex
  dim5 <- metadata_tidy_109 |>
    dplyr::filter(
      text == "Alter" & # age
        !(valueTexts %in% "Alter - Total")
    ) # exclude "Total"
  dim6 <- metadata_tidy_109 |>
    dplyr::filter(
      text == "Jahr" & # get years
        valueTexts %in% year_first:year_last
    )
  dim7 <- metadata_tidy_109 |>
    dplyr::filter(text == "Beobachtungseinheit" & # type of parameter types
                    valueTexts %in% c(
                      "Geburtenziffern",
                      "Prospektive Sterbewahrscheinlichkeiten",
                      "Auswanderungsziffern",
                      "Interkantonale Abwanderungsziffern",
                      stringi::stri_unescape_unicode("Einb\\u00fcrgerungsziffern")
                    )) # "Einbürgerungsziffern"))

  # build dimensions list object
  dimensions <- list(
    dim1$values,
    dim2$values,
    dim3$values,
    dim4$values,
    dim5$values,
    dim6$values,
    dim7$values
  )

  # add names
  names(dimensions) <- c(
    unique(dim1$code),
    unique(dim2$code),
    unique(dim3$code),
    unique(dim4$code),
    unique(dim5$code),
    unique(dim6$code),
    unique(dim7$code)
  )

  cli::cli_progress_step("Downloading rate parameters")

  # Download rate parameters
  fso_rates_raw <- BFS::bfs_get_data(
    number_bfs = number_fso_rates, # "px-x-0104020000_109"
    query = dimensions
  )

  # Check structure
  assertthat::assert_that(is.data.frame(fso_rates_raw))
  assertthat::assert_that(ncol(fso_rates_raw) == 8)

  # Column names
  expected_names_109 <- c(
    "Kanton",
    "Szenario-Variante",
    stringi::stri_unescape_unicode("Staatsangeh\\u00f6rigkeit (Kategorie)"),
    "Geschlecht",
    "Alter",
    "Jahr",
    "Beobachtungseinheit",
    stringi::stri_unescape_unicode(paste0(
      "Szenarien zur Bev\\u00f6lkerungsentwicklung der Kantone 2025-2055 - ",
      "Ziffern nach Kanton, Szenario-Variante, ",
      "Staatsangeh\\u00f6rigkeit (Kategorie), ",
      "Geschlecht und Alter")
    )
  )

  # Check columns by name
  assertthat::assert_that(all(names(fso_rates_raw)[1:8] == expected_names_109))

  # Check if column 8 includes numeric values
  assertthat::assert_that(is.numeric(fso_rates_raw[[8]]))


  # Assert that "Jahr" contains no NAs and matches required values exactly
  assertthat::assert_that(
    is.character(fso_rates_raw$Jahr),
    all(!is.na(fso_rates_raw$Jahr)),
    setequal(fso_rates_raw$Jahr, as.character(year_first:year_last))
  )

  # Bring variable names and factor levels into the format required later
  fso_rates <- fso_rates_raw |>
    dplyr::rename(
      nat = stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ), # "Staatsangehörigkeit (Kategorie)",
      sex = Geschlecht,
      age = Alter,
      year = Jahr,
      fso_parameter = Beobachtungseinheit,
      scen = "Szenario-Variante",
      # Rename column according to position rather than by referring to long name
      value = 8
    ) |>
    # change factor levels
    dplyr::mutate(
      scen = dplyr::case_match(
        scen,
        "Referenzszenario AR-00-2025" ~ "reference",
        "'hohes' Szenario BR-00-2025" ~ "high",
        "'tiefes' Szenario CR-00-2025" ~ "low"
      ),
      nat = dplyr::case_match(
        nat,
        "Schweiz" ~ "ch",
        "Ausland" ~ "int"
      ),
      sex = dplyr::case_when(
        sex == "Mann" ~ "m",
        sex == "Frau" ~ "f"
      ),
      age = as.numeric(stringr::str_extract(age, "\\d+")),
      fso_parameter = dplyr::case_match(
        fso_parameter,
        "Prospektive Sterbewahrscheinlichkeiten" ~ "mor",
        "Auswanderungsziffern" ~ "emi_int", # used to be emi
        "Interkantonale Abwanderungsziffern" ~ "emi_nat",
        # "Einbürgerungsziffern" ~ "acq",
        stringi::stri_unescape_unicode("Einb\\u00fcrgerungsziffern") ~ "acq",
        "Geburtenziffern" ~ "birthrate"
      )
    )
  # End of getting rates


  # Get share of newborns with Swiss citizenship born to internat. mothers ----

  cli::cli_progress_step("Preparing download of birth parameter")

  # Get meta data to determine what to download
  metadata_106 <- BFS::bfs_get_metadata(
    number_bfs = number_fso_births
  ) # "px-x-0104020000_106")
  metadata_tidy_106 <- metadata_106 |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = metadata_106 |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  # Check if spatial units are available
  assertthat::assert_that(
    all(spatial_units
        %in%
          metadata_tidy_106$valueTexts[
            metadata_tidy_106$text ==
              "Kanton"]) ,
    msg = paste0("At least one of the requested spatial units is not available.",
                 " Check the spelling against those in the STATTAB cube ",
                 number_fso_births, ". Inspecting the column 'valueTexts' in ",
                 "the following package data may help to determine the correct",
                 " spelling: data('stattab_106_snap')"
    ))

  # Specify the elements to download
  dim1 <- metadata_tidy_106 |>
    dplyr::filter(
      text == "Kanton" & # Canton
        valueTexts %in% spatial_units
    )

  dim2 <- metadata_tidy_106 |>
    dplyr::filter(text == "Szenario-Variante" & # scenario
                    valueTexts %in% c(
                      "Referenzszenario AR-00-2025",
                      "'hohes' Szenario BR-00-2025",
                      "'tiefes' Szenario CR-00-2025"
                    ))
  dim3 <- metadata_tidy_106 |>
    dplyr::filter(
      text == stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ) & # "Staatsangehörigkeit (Kategorie)" & # nationality
        valueTexts %in% "Ausland"
    )
  dim4 <- metadata_tidy_106 |>
    dplyr::filter(
      text == "Geschlecht" & # sex
        valueTexts %in% "Geschlecht - Total"
    )
  dim5 <- metadata_tidy_106 |>
    dplyr::filter(
      text == "Altersklasse" & # age
        valueTexts %in% "Altersklasse - Total"
    )
  dim6 <- metadata_tidy_106 |>
    dplyr::filter(
      text == "Jahr" & # get years
        valueTexts %in% year_first:year_last
    )
  dim7 <- metadata_tidy_106 |>
    dplyr::filter(
      text == "Beobachtungseinheit" & # type of parameter types
        valueTexts %in%
        c(
          "Lebendgeburten",
          stringi::stri_unescape_unicode(
            "Lebendgeburten nach Alter und Staatsangeh\\u00f6rigkeit der Mutter"
          )
        )
    ) # "Lebendgeburten nach Alter und Staatsangehörigkeit der Mutter"))

  # build dimensions list object
  dimensions <- list(
    dim1$values,
    dim2$values,
    dim3$values,
    dim4$values,
    dim5$values,
    dim6$values,
    dim7$values
  )

  # add names
  names(dimensions) <- c(
    unique(dim1$code),
    unique(dim2$code),
    unique(dim3$code),
    unique(dim4$code),
    unique(dim5$code),
    unique(dim6$code),
    unique(dim7$code)
  )

  cli::cli_progress_step("Downloading birth parameter")

  # Download rate parameters
  fso_int_mothers_raw <- BFS::bfs_get_data(
    number_bfs = number_fso_births, # "px-x-0104020000_106",
    query = dimensions
  )

  # Process data
  fso_int_mothers <- fso_int_mothers_raw |>
    # Compute share of Swiss newborns to international mothers
    tidyr::pivot_wider(
      names_from = Beobachtungseinheit,
      values_from = stringi::stri_unescape_unicode(
        paste0(
          "Szenarien zur Bev\\u00f6lkerungsentwicklung der Kantone 2025-2055",
          " - zuk\\u00fcnftige Bev\\u00f6lkerungsentwicklung nach Kanton, ",
          "Szenario-Variante, Staatsangeh\\u00f6rigkeit (Kategorie), ",
          "Geschlecht und Altersklasse"
        )
      )
    ) |>
    # use shorter, clearer names
    dplyr::rename(
      # all live births from international mothers
      live_birth_total = stringi::stri_unescape_unicode(
        "Lebendgeburten nach Alter und Staatsangeh\\u00f6rigkeit der Mutter"
      ), # "Lebendgeburten nach Alter und Staatsangehörigkeit der Mutter",
      # live births of international newborns to international mothers
      live_birth_int = Lebendgeburten
    ) |>
    dplyr::mutate(
      int_mothers = (live_birth_total - live_birth_int) / live_birth_total
    ) |>
    # Bring variable names and factor levels into the format required later
    dplyr::rename(
      scen = "Szenario-Variante",
      age = Altersklasse,
      year = Jahr
    ) |>
    # change factor levels
    dplyr::mutate(
      scen = dplyr::case_match(
        scen,
        "Referenzszenario AR-00-2025" ~ "reference",
        "'hohes' Szenario BR-00-2025" ~ "high",
        "'tiefes' Szenario CR-00-2025" ~ "low"
      )
    ) |>
    # remove unnecessary variables
    dplyr::select(Kanton, year, scen, int_mothers) |>
    dplyr::arrange(Kanton, year, scen)

  # Assert that year contains no NAs and matches required values exactly
  assertthat::assert_that(
    is.character(fso_int_mothers$year),
    all(!is.na(fso_int_mothers$year)),
    setequal(fso_int_mothers$year, as.character(year_first:year_last))
  )
  # End of newborns to int mothers

  cli::cli_progress_step("Merging and cleaning parameters")

  # Merge data frames containing numbers and rates ----
  projection_parameters <- dplyr::full_join(fso_rates, fso_numbers) |>
    tidyr::pivot_wider(names_from = fso_parameter, values_from = value) |>
    dplyr::mutate(mig_nat_n = imm_nat_n - emi_nat_n) |>
    dplyr::left_join(fso_int_mothers, by = c("Kanton", "year", "scen")) |>
    dplyr::arrange(year)

  # Clean data ----
  projection_parameters_clean <- projection_parameters |>
    dplyr::mutate(spatial_unit = Kanton) |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::select(
      nat, sex, age, start_n,
      year, scen, spatial_unit,
      birthrate, int_mothers, mor, emi_int, emi_nat, acq,
      imm_int_n, imm_nat_n, emi_nat_n, mig_nat_n,
      fso_projection_n,
      -c(Kanton, emi_n)
    )

  # Feedback if years are outside current FSO projection period----
  if (year_first < 2025 |
      year_first > 2055 |
      year_last < 2025 |
      year_last > 2055) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("`year_first` or `year_last` is outside FSO's current
                    projection period (2025-2055).")
    cli::cli_alert_info("You might want to double-check your input variables.")
  }

  return(projection_parameters_clean)
}
