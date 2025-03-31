#' Prepare data for evaluation
#'
#' @description
#' This functions takes benchmark data (typically population records) and
#' population projections and prepares a combined data frame to evaluate the
#' performance of the projection.
#' For more details on usage, see
#' \code{vignette("evaluate", package = "propop")}.
#'
#' @section Input data and variables:
#' Both input data frames must contain the following variables for the
#' \bold{same range of years}:
#' \describe{
#'   \item{year}{character, year in which the population was recorded.}
#'   \item{spatial_unit}{character, indicating the spatial entities (e.g.,
#'   cantons, districts, municipalities).}
#'   \item{nat}{character, ch = Swiss, int = foreign / international.}
#'   \item{sex}{character, f = female, m = male.}
#'   \item{age}{numeric, 101 one-year age classes, ranging from 0 to 100
#'   (including those older than 100).}
#'   \item{n}{numeric, number of people per year, spatial entity, and
#'   demographic group.}
#' }
#'
#' @param data_benchmark data frame containing benchmark data (e.g., actual /
#' official population records obtained with `propop::get_population()`).
#' @param n_benchmark numeric column containing the benchmark population of each
#' demographic group.
#' @param data_projected data frame containing population projections; can be
#' created with `propop::propop()`.
#' @param n_projected numeric column containing the projected size of each
#' demographic group.
#' @param age_groups character, optional argument (`"age_groups_3"`) indicating
#' if the data shall be aggregated into the predefined three age groups
#' (0-19, 20-64, over 65 years). Using aggregated groups will lead to smaller
#' projection errors than using 101 age classes. Currently only one option is
#' available for aggregating age groups. Defaults to using 101 one-year age
#' classes.
#'
#' @return Returns a data frame with the number of people from the benchmark and
#' from the projection. Each row contains a unique combination of year, spatial
#' unit, and demographic group.
#'
#' @export
#'
#' @autoglobal
#'
#' @examples
#' \dontrun{
#' combined <- prepare_evaluation(
#'   data_benchmark = output_get_population,
#'   data_projected = output_propop
#' )
#' combined_grouped <- prepare_evaluation(
#'   data_benchmark = output_get_population,
#'   data_projected = output_propop,
#'   age_groups = "age_groups_3"
#' )
#' }
prepare_evaluation <- function(
    data_benchmark,
    n_benchmark,
    data_projected,
    n_projected,
    age_groups = NULL) {

  # browser()

  # Get earliest year in data_projected
  base_year <- data_projected |>
    distinct(year) |>
    pull() |>
    min() |>
    as.numeric()

  # Convert `year` in benchmark to integer
  data_benchmark <- data_benchmark |>
    dplyr::mutate(year = as.integer(year)) |>
  # Rename columns containing population
    dplyr::rename(n_benchmark = !!sym(n_benchmark))

  data_projected <- data_projected |>
    dplyr::rename(n_projected = !!sym(n_projected))

  # Test input ----
  assertthat::assert_that(
    all(as.integer(data_benchmark$year) == data_benchmark$year),
    msg = "All years in `data_benchmark` must be integers"
  )
  assertthat::assert_that(
    all(as.integer(data_projected$year) == data_projected$year),
    msg = "All years in `data_projected` must be integers"
  )
  assertthat::assert_that(
    identical(
      range(as.integer(data_benchmark$year)),
      range(as.integer(data_projected$year))
    ),
    msg = "The ranges of years in `data_benchmark` and `data_projected` are not
    identical"
  )
  assertthat::assert_that(
    is.null(age_groups) ||
      identical(age_groups, "age_groups_3"),
    msg = "`age_groups` must be either NULL or 'age_groups_3'"
  )
  assertthat::assert_that(
    "sex" %in% names(data_benchmark),
    msg = "column `sex` is missing in data_benchmark"
  )
  assertthat::assert_that(
    "age" %in% names(data_benchmark),
    msg = "column `age` is missing in data_benchmark"
  )
  assertthat::assert_that(
    "year" %in% names(data_benchmark),
    msg = "column `year` is missing in data_benchmark"
  )
  assertthat::assert_that(
    "n_benchmark" %in% names(data_benchmark),
    msg = "column `n_benchmark` is missing in data_benchmark"
  )
  assertthat::assert_that(
    "nat" %in% names(data_projected),
    msg = "column `nat` is missing in data_projected"
  )
  assertthat::assert_that(
    "sex" %in% names(data_projected),
    msg = "column `sex` is missing in data_projected"
  )
  assertthat::assert_that(
    "age" %in% names(data_projected),
    msg = "column `age` is missing in data_projected"
  )
  assertthat::assert_that(
    "year" %in% names(data_projected),
    msg = "column `year` is missing in data_projected"
  )
  assertthat::assert_that(
    "spatial_unit" %in% names(data_projected),
    msg = "column `spatial_unit` is missing in data_projected"
  )
  assertthat::assert_that(
    "n_projected" %in% names(data_projected),
    msg = "column `n_projected` is missing in data_projected"
  )

  # Combine benchmark data and projected population ----

  ## Prepare projected data ----
  data_projected_clean <- data_projected |>
    dplyr::mutate(n_projected = round(n_projected, digits = 0)) |>
    dplyr::select(year, spatial_unit, age, sex, nat, n_projected)

  ## Combine data ----
  .data <- data_benchmark |>
    dplyr::full_join(
      data_projected_clean,
      by = any_of(c("year", "spatial_unit", "age", "sex", "nat"))
    )


  ## Summarize age groups if applicable ----
  if (isTRUE(age_groups == "age_groups_3")) {
    df <- .data |>
      mutate(
        age = case_when(
          age < 20 ~ "age_00_19",
          age >= 20 & age < 65 ~ "age_20_64",
          age >= 65 ~ "age_65_plus"
        )
      ) |>
      group_by(year, spatial_unit, age, sex, nat) |>
      summarise(
        n_benchmark = sum(n_benchmark),
        n_projected = sum(n_projected),
        .groups = "drop"
      )
  } else {
    df <- .data
  }

  # Check for missing values in output ----
  if (anyNA(df)) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns have missing values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { df |> select_if(function(x) any(is.na(x))) |> names()}}."
    ))
    cli::cli_alert_info("Missing values are likely to lead to biased evaluation
                        measures that cannot be properly interpreted.")
  }

  return(df)
}
