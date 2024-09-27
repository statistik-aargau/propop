#' Get population data from FSO
#'
#' @description
#' Users who do not have the required population data can use this convenience
#' function to get the mandatory starting population for
#' `propop::propop()` from the Federal Statistical Office (FSO). The function
#' can also be used to obtain historical population records (e.g., for model
#' performance evaluations).
#'
#' To get population data, you must use the spelling defined in the
#' corresponding FSO table. For more details see
#' \code{vignette("prepare_data", package = "propop")}.
#'
#' Changes to the API interface may break this function. If problems occur, see
#' \code{vignette("prepare_data", package = "propop")}.
#'
#' @param number_fso character, px-x table ID for population records,
#'        defaults to `px-x-0102010000_101`.
#' @param year_first numeric, first year for which the population records are to
#'        be downloaded.
#' @param year_last numeric, last year for which the population records are to
#'        be downloaded. When downloading the starting population for the
#'        projection, this will be the same as `year_first`.
#'
#' `year_first` when requesting the starting population for `propop::propop()`
#' @param spatial_units character vector, indicating at least one spatial
#' entity for which the projection will be run. Typically a canton, districts,
#' or municipalities.
#'
#' @return A data frame. For each of the four demographic groups (female / male,
#' Swiss / foreign nationals), there are 101 age classes, resulting in a total
#' of 404 rows per requested year and spatial unit. Columns included in the
#' returned data frame:
#' \describe{
#'   \item{year}{character, year in which the population was recorded.}
#'   \item{spatial_unit}{character, indicating the spatial entities (e.g.,
#'   cantons, districts, municipalities).}
#'   \item{nat}{character, ch = Swiss, int = foreign / international.}
#'   \item{sex}{character f = female, m = male.}
#'   \item{age}{numeric, 101 one-year age classes, ranging from 0 to 100
#'   (including those older than 100).}
#'   \item{n}{numeric, number of people per year, spatial entity, and
#'   demographic group.}
#' }
#'
#' @source Federal Statistical Office:
#' <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0102010000_101/-/px-x-0102010000_101.px/>
#'
#' @autoglobal
#'
#' @export
#'
#' @examplesIf (Sys.getenv("RUN_EXPENSIVE_TESTS") == "true")
#' \dontrun{
#' get_population(
#'   number_fso = "px-x-0102010000_101",
#'   year_first = 2018,
#'   year_last = 2019,
#'   spatial_units = "- Aargau"
#' )
#' get_population(
#'   year_first = 2018,
#'   year_last = 2018,
#'   spatial_units = c("- Aargau", "......0301 Aarberg")
#' )
#' }
get_population <- function(number_fso = "px-x-0102010000_101",
                           year_first,
                           year_last,
                           spatial_units) {
  # Test input
  # convert input in years to integer, results in error if not possible
  year_first <- vctrs::vec_cast(year_first, integer())
  year_last <- vctrs::vec_cast(year_last, integer())

  # get last year (most recent possible population record)
  current_year <- (as.numeric(format(Sys.Date(), "%Y")))
  assertthat::assert_that(is.integer(year_first),
    year_first >= 2018 && year_first < current_year,
    msg = paste0(
      "`year_first` must be an integer or a numeric value without decimals
      larger than 2017 and smaller than ",
      current_year
    )
  )
  assertthat::assert_that(is.integer(year_last),
    year_last >= 2018 && year_last < current_year,
    msg = paste0(
      "`year_last` must be an integer or
                                       a numeric value without decimals
                                       larger than 2017 and smaller than ",
      current_year
    )
  )
  assertthat::assert_that(is.integer(year_first),
    is.integer(year_last), year_first <= year_last,
    msg = "year_first must be smaller than or equal to year_last."
  )
  assertthat::assert_that(is.vector(spatial_units),
    length(spatial_units) > 0,
    any(sapply(spatial_units, is.character)),
    msg = paste0(
      "The argument 'spatial_unit' must be a non-empty vector with at least",
      " one character elementof type `character`."
    )
  )

  cli::cli_progress_step("Preparing download...",
    msg_done = "Download prepared"
  )

  # rename fso to bfs for use in package bfs
  number_bfs <- number_fso

  # Get meta data and prepare query for the population data:
  metadata_pop <- BFS::bfs_get_metadata(number_bfs = number_bfs)
  metadata_pop_tidy <- metadata_pop |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = metadata_pop |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  # check what the most recent year is
  min_year <- metadata_pop_tidy |>
    dplyr::filter(text == "Jahr") |>
    distinct(values) |>
    pull() |>
    min() |>
    as.numeric()

  max_year <- metadata_pop_tidy |>
    dplyr::filter(text == "Jahr") |>
    distinct(values) |>
    pull() |>
    max() |>
    as.numeric()

  assertthat::assert_that(year_first >= min_year,
    msg = paste0(
      "No data available for `year_first` = ",
      year_first,
      ". Population data are available for ",
      min_year, "-", max_year, "."
    )
  )

  assertthat::assert_that(year_last >= min_year,
    msg = paste0(
      "No data available for `year_last` = ",
      year_last,
      ". Population data are available for ",
      min_year, "-", max_year, "."
    )
  )


  # Check if spatial units are available
  assertthat::assert_that(
    all(spatial_units
    %in%
      metadata_pop_tidy$valueTexts[
        metadata_pop_tidy$text ==
          "Kanton (-) / Bezirk (>>) / Gemeinde (......)"]) ,
    msg = paste0("At least one of the requested spatial units is not ",
    "available. Check the spelling against those in the STATTAB cube ",
    number_fso))

  # Specify the elements to download
  dim1 <- metadata_pop_tidy |>
    dplyr::filter(
      text == "Kanton (-) / Bezirk (>>) / Gemeinde (......)" & # Canton
        valueTexts %in% spatial_units
    )

  dim2 <- metadata_pop_tidy |>
    dplyr::filter(
      text == "Jahr" &
        valueTexts %in% year_first:year_last
    ) # get population in December

  dim3 <- metadata_pop_tidy |>
    dplyr::filter(
      text == stringi::stri_unescape_unicode(
        "Bev\\u00f6lkerungstyp"
      ) & # permanent
        valueTexts %in% stringi::stri_unescape_unicode(
          "St\\u00e4ndige Wohnbev\\u00f6lkerung"
        )
    )

  dim4 <- metadata_pop_tidy |>
    dplyr::filter(
      text == stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ) & # nationality
        valueTexts %in% c("Schweiz", "Ausland")
    )

  dim5 <- metadata_pop_tidy |>
    dplyr::filter(
      text == "Geschlecht" & # sex
        valueTexts %in% c("Mann", "Frau")
    )

  dim6 <- metadata_pop_tidy |>
    dplyr::filter(
      text == "Alter" & # age
        !(valueTexts %in% "Alter - Total")
    ) # exclude "Total"

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

  cli::cli_progress_step("Downloading data...",
    msg_done = "Data downloaded"
  )

  # Download population data
  fso_pop_raw <- BFS::bfs_get_data(
    number_bfs = number_bfs,
    query = dimensions
  )

  cli::cli_progress_step("Cleaning data...",
    msg_done = "Data cleaned",
    spinner = TRUE
  )

  # Bring variable names and factor levels into the format required later
  fso_pop_raw |>
    dplyr::select(-stringi::stri_unescape_unicode("Bev\\u00f6lkerungstyp")) |>
    dplyr::rename(
      year = Jahr,
      Kanton = "Kanton (-) / Bezirk (>>) / Gemeinde (......)",
      nat = stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ),
      sex = Geschlecht,
      age = Alter,
      n = stringi::stri_unescape_unicode(
        "St\\u00e4ndige und nichtst\\u00e4ndige Wohnbev\\u00f6lkerung"
      )
    ) |>
    # change factor levels
    dplyr::mutate(
      Kanton = stringr::str_remove_all(Kanton, "- "),
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
    ) |>
    dplyr::rename(spatial_unit = Kanton)
}
