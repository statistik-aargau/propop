#' Compute evaluation measures
#'
#' Uses the differences between a benchmark and the results from a projection
#' to compute performance measures.
#'
#' The input is a data frame created with `propop::prepare_evaluation()`.
#' It includes a benchmark (typically the observed population records, i.e.,
#' the number of people per spatial unit, demographic group, and year) and the corresponding
#' projected number of people. The input can range from low resolution
#' (e.g., total number of people per municipality) to high resolution
#' (e.g., 101 age classes, nationality, sex).
#'
#' For more details on usage, see
#' \code{vignette("evaluate", package = "propop")}.
#'
#' @param combined data frame created with `propop::prepare_evaluation()`.
#' @param weight_groups character, optional argument indicating one or more
#' column names to obtain evaluation criteria weighted for specific groups
#' (e.g., age groups, nationality).
#'
#' @returns
#' A data frame. The following evaluation criteria can directly be interpreted
#' and used for descriptive comparisons:
#'
#'* `error` is the forecast error; it quantifies the level of under-projection
#' (negative values) and over-projection (positive values) relative to the
#' benchmark `n_bench`.
#'
#'* `perc_e` is the percentage error and expresses the under-/over-projection in
#' percent of the benchmark `n_bench`.
#'
#'* `abs_perc_e` is the absolute percentage error; it is the absolute deviation
#' in percent of the benchmark `n_bench`, thus only showing the extent of the
#' error but not the direction.
#'
#' * `w_abs_per_e` is the weighted absolute percentage error; it weighs each
#' absolute percentage error according to the population size of the focal group
#' (e.g., nationality, age group). The weighted version is useful as an
#' aggregated measure when groups vary strongly in terms of population size.
#' Only returned when the argument `weight_groups` contains at least one
#' grouping variable.
#'
#'
#' The following helper variables are used to compute aggregate measures. They
#' are only returned when weight groups are provided via the argument
#' `weight_groups`.
#'
#'
#'* `n_tot` is the total number of people (i.e., sum of the number of people in
#' all demographic groups); used to compute the weighted absolute percentage
#' error.
#'
#'* `group_tot` is the number of people in the focal group; used to compute the
#' weighted absolute percentage error.
#'
#'* `weight` is the share of the (optional) focal group (e.g., municipality type
#' / size, nationality, age group) relative to all people; used to compute the
#' weighted absolute percentage error.
#'
#' @references
#' Baker, J., et al. (2015). Sub-county population estimates using administrative records: A municipal-level case study in New Mexico. In M. N. Hoque & L. B. Potter (Eds.), Emerging techniques in applied demography (pp. 63-79). Springer, [https://doi.org/10.1007/978-94-017-8990-5_6](https://doi.org/10.1007/978-94-017-8990-5_6)
#'
#' Wilson, T. (2012). Forecast accuracy and uncertainty of Australian Bureau of Statistics state and territory population projections, International Journal of Population Research, 1, 419824, [https://doi.org/10.1155/2012/419824](https://doi.org/10.1155/2012/419824)
#'
#' Wilson, T. (2016). Evaluation of alternative cohort-component models for local area population forecasts, Population Research and Policy Review, 35, 241-261, [https://doi.org/10.1007/s11113-015-9380-y](https://doi.org/10.1007/s11113-015-9380-y)
#'
#' @export
#'
#' @autoglobal
#'
#' @examples
#' \dontrun{
#' # Get evaluation measures without weights
#' compute_measures(combined)
#' # Get evaluation measures weighted for groups
#' compute_measures(combined, weight_groups = c("age", "nat"))
#' }
compute_measures <- function(combined, weight_groups = NULL) {
  # Test input ----
  ## Presence of mandatory columns ----
  assertthat::assert_that("year" %in% names(combined),
    msg = "column `year` is missing in combined"
  )
  assertthat::assert_that("spatial_unit" %in% names(combined),
    msg = "column `spatial_unit` is missing in combined"
  )
  assertthat::assert_that("nat" %in% names(combined),
    msg = "column `nat` is missing in combined"
  )
  assertthat::assert_that("sex" %in% names(combined),
    msg = "column `sex` is missing in combined"
  )
  assertthat::assert_that("age" %in% names(combined),
    msg = "column `age` is missing in combined"
  )
  assertthat::assert_that("n_bench" %in% names(combined),
    msg = "column `n_bench` is missing in combined"
  )
  assertthat::assert_that("n_proj" %in% names(combined),
    msg = "column `n_proj` is missing in combined"
  )
  # Input of weight groups
  if (!is.null(weight_groups)) {
    assertthat::assert_that(
      length(weight_groups) > 0,
      msg = "`weight_groups` must contain at least one element."
    )
    assertthat::assert_that(
      is.character(weight_groups),
      msg = "`weight_groups` must be a character vector."
    )
  }

  df <- combined |>
    mutate(
      # Compute error
      error = n_proj - n_bench,
      # Compute percentage error
      perc_e = error / n_bench * 100,
      # Compute absolute percentage error (neglecting sign)
      abs_perc_e = abs(error) / n_bench * 100
    )

  # Add weighted measures if weight_groups is defined
  if (!is.null(weight_groups)) {
    df <- df |>
      mutate(
        # Compute total number of people
        n_tot = sum(n_bench, na.rm = TRUE)
      ) |>
      mutate(group_tot = sum(n_bench, na.rm = TRUE), .by = any_of(weight_groups)) |>
      mutate(
        # Compute size of focal unit relative to overall population size
        weight = group_tot / n_tot,
        # Compute weighted absolute percentage error
        w_abs_perc_e = abs_perc_e * weight
      )
  }

  # Feedback when there are Inf values in output ----
  if (any(sapply(df, is.infinite))) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns of the output have `Inf` values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { df |> select_if(function(x) any(is.infinite(x))) |> names()}}."
    ))
    cli::cli_alert_info(paste0("`Inf` values are probably caused by divisions ",
                        "by zero (e.g., when `n_bench` = 0). Consider using ",
                        "age groups that include more than 1 year."))
  }

  # Check for missing values in output ----
  if (anyNA(df)) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns of the output have missing values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { df |> select_if(function(x) any(is.na(x))) |> names()}}."
    ))
    cli::cli_alert_info("Missing values are likely to lead to biased evaluation
                        measures that cannot be properly interpreted.")
  }

  return(df)
}
