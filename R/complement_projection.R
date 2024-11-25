#' Add identifiers to the raw results from the projection
#'
#' @description
#' Adds information about demographic groups and years to make the raw results
#' from `project_raw.R` more informative.
#'
#' @param skeleton empty data frame containing all combinations of demographic
#'    groups, years, and spatial levels.
#' @param projection_raw data frame showing the projection results obtained with
#'    `project_raw`.
#'
#' @return data frame containing five identifiers (age, sex, nationality, year,
#'    spatial levels). These indicate to which demographic group and year the
#'    results in the rows refer to.
#' @autoglobal
#' @noRd

complement_projection <- function(skeleton, projection_raw) {
  # check structure of input
  assertthat::assert_that(
    is.numeric(skeleton$age),
    msg = "Column 'age' must be numeric."
  )

  assertthat::assert_that(
    is.factor(skeleton$sex),
    all(levels(skeleton$sex) %in% c("m", "f")),
    msg = "Column 'sex' must be a factor with either 'm' and 'f' as level."
  )

  assertthat::assert_that(
    is.factor(skeleton$nat),
    all(levels(skeleton$nat) %in% c("ch", "int")),
    msg = "Column 'nat' must be a factor with either 'ch' or 'int' as level."
  )

  assertthat::assert_that(
    is.numeric(skeleton$year),
    all(skeleton$year >= 2018),
    all(skeleton$year <= 2061),
    !any(is.na(skeleton$year)),
    msg = "`year` must only inlcude numeric values between 2018 and 2061."
  )


  ## Include again to show problem in upcoming developer meeting
  # # Only keep n (post-hoc components are imprecise)
  # projection_raw <- projection_raw|>
  #   dplyr::select(N)

  # create skeleton
  skeleton |>
    dplyr::bind_cols(projection_raw) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower)
}
