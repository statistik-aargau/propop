#' Prepare empty data frame to fill in projection results
#'
#' @description
#' Create empty data frame (skeleton) containing information about demographic
#' groups, years, and spatial levels.
#'
#' @param age_groups numeric,  number of 1-year age classes (typically 101).
#' @param year_first integer, indicating the first year of the projection period.
#'    The results will also include the starting population.
#' @param year_last integer, indicating the last year of the projection period.
#' @param spatial_unit character vector, at least one region
#'
#' @return Empty data frame including all combinations of the input variables.
#'
#' @noRd

prepare_skeleton <-
  function(age_groups, year_first, year_last, spatial_unit) {
    # check input
    assertthat::assert_that(is.numeric(age_groups),
      !is.na(age_groups),
      msg = "`age_groups` must be a numeric value."
    )
    assertthat::assert_that(is.numeric(year_first),
      msg = "The argument 'year_first' must be numeric"
    )
    assertthat::assert_that(is.numeric(year_last),
      msg = "The argument 'year_last' must be numeric"
    )
    assertthat::assert_that(is.numeric(year_first),
      is.numeric(year_last), year_first <= year_last,
      msg = "year_first must be smaller than or equal to year_last"
    )
    assertthat::assert_that(is.character(spatial_unit),
      all(!is.na(spatial_unit)),
      all(nzchar(spatial_unit)),
      msg = paste0(
        "The argument 'spatial_unit' must be of type `character`",
        " and cannot include any `NA` or empty elements."
      )
    )

    # create skeleton
    expand.grid(
      age = 0:(age_groups - 1),
      sex = c("m", "f"),
      nat = c("ch", "int"),
      year = ((year_first - 1):year_last),
      spatial_unit = spatial_unit
    ) |>
      tibble::as_tibble()
  }
