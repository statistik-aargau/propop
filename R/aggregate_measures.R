#' Aggregate evaluation measures
#'
#' Returns descriptive summary statistics of model accuracy and bias measures
#' across demographic groups and years. The returned statistics are particularly
#' useful for comparing the model performance for different groups or different
#' models.
#'
#' @param data data frame created with function `compute_measures`.
#' @param weight_groups character, optional argument indicating one or more
#' column names to obtain evaluation criteria weighted for specific groups
#' (e.g., age groups, nationality).
#'
#' @return
#' #' A data frame. The data frame includes the following summary measures:
#'
#' * `mpe` is the mean percentage error
#' (\bold{mpe}; or mean algebraic percentage error
#' \bold{malpe}); it is a bias indicator as it takes the \bold{direction} of the
#' error into account. Positive values indicate that the projections were,
#' overall, too high. Negative values indicate that the projections were,
#' overall, too low.
#' The closer the value is too zero, the lower the bias.
#' * `medpe` is the median (or middle value) of the percentage error
#' (\bold{medpe}). Particularly useful for small samples or skewed
#' distributions. The closer the value is too zero, the lower the bias.
#' * `mape` is the mean \bold{absolute} percentage / proportional error
#' (\bold{mape}). It considers variance (or amplitude) and can be seen as a
#' measure of precision.
#' The smaller the value, the lower is the average error.
#' * `medape` is the median (or middle value) of the \bold{absolute}
#' percentage error (\bold{medape}). Particularly useful for small samples or
#' skewed distributions. The smaller the value, the lower is the average error.
#' * `rmse` is the root mean square error; it is an indication of the robustness
#' or quality of the projection. The smaller the value, the more robust the
#' projection.
#' * `wmape` is the \bold{weighted} mean \bold{absolute} percentage
#' error (\bold{wmape}); in contrast to `mape`, this measure
#' weights each absolute percentage error according to the population size of
#' the "focal" group (e.g., nationality, age group) and thus considers domain
#' size. Put differently, errors count more in populous groups than in less
#' populous groups. This measure is particularly useful when population sizes
#' vary strongly. The closer the value, the more precise is the
#' projection.
#' * `n_measure` is the number of times a summary measure occurs (per weight
#' group if requested).
#' * `ape_under_1` is a measure to gauge the error distribution; it indicates the
#' proportion of observations that have absolute percentage errors smaller than
#' 1%.
#' * `ape_under_5` is a measure to gauge the error distribution; it indicates the
#' proportion of observations that have absolute percentage errors smaller than
#' 5%.
#'
#' @export
#'
#' @autoglobal
#'
#' @references
#' Baker, J., et al. (2015). Sub-county population estimates using administrative records: A municipal-level case study in New Mexico. In M. N. Hoque & L. B. Potter (Eds.), Emerging techniques in applied demography (pp. 63-79). Springer, [https://doi.org/10.1007/978-94-017-8990-5_6](https://doi.org/10.1007/978-94-017-8990-5_6)
#'
#' Bérard-Chagnon, J. (2015) Using tax data to estimate the number of families and households in Canada. In M. N. Hoque & L. B. Potter (Eds.), Emerging techniques in applied demography (pp. 137-153). Springer, [https://doi.org/10.1007/978-94-017-8990-5_10](https://doi.org/10.1007/978-94-017-8990-5_10)
#'
#' Reinhold M. & Thomsen, S. L. (2015) Subnational population projections by age: An evaluation of combined forecast techniques, Population Research and Policy Review, 34, 593-613, [https://doi.org/10.1007/s11113-015-9362-0](https://doi.org/10.1007/s11113-015-9362-0)
#'
#' Wilson, T. (2012). Forecast accuracy and uncertainty of Australian Bureau of Statistics state and territory population projections, International Journal of Population Research, 1, 419824, [https://doi.org/10.1155/2012/419824](https://doi.org/10.1155/2012/419824)
#'
#' Wilson, T. (2016). Evaluation of alternative cohort-component models for local area population forecasts, Population Research and Policy Review, 35, 241-261, [https://doi.org/10.1007/s11113-015-9380-y](https://doi.org/10.1007/s11113-015-9380-y)
#'
#' @importFrom stats median
aggregate_measures <- function(data, weight_groups = NULL) {
  results <- data |>
    dplyr::summarise(
      mpe = mean(pe, na.rm = TRUE),
      medpe = median(pe, na.rm = TRUE),
      mape = mean(ape, na.rm = TRUE),
      medape = median(ape, na.rm = TRUE),
      wmape = if ("w_ape" %in% colnames(data)) {
        sum(w_ape, na.rm = TRUE)
      } else {
        NA
      },
      # check calculation of rmse
      rmse = sqrt(mean((n_benchmark - n_projected)^2)),
      n_measure = n(),
      ape_under_1 = sum(ape < 1, na.rm = TRUE) / n_measure,
      ape_under_5 = sum(ape < 5, na.rm = TRUE) / n_measure,
      .by = {{ weight_groups }}
    )
  # Remove `wmape` if input doesn't include weight_groups
  if (!("weight" %in% colnames(data))) {
    results <- results |>
      dplyr::select(-wmape)
  }

  # Feedback data includes weight but no group_weight argument provided
  if ("weight" %in% colnames(data) & is.null(weight_groups)) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_alert_warning(paste0("`data` includes results for weighted ",
                                  "groups but the `weight_groups` argument ",
                                  "is empty. Did you forget to specify the ",
                                  "same groups as in `compute_measures`?")
    )
  }

  # Feedback when input contain `Inf` values
  if (any(sapply(data, is.infinite))) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns in `data` have `Inf` values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { data |> select_if(function(x) any(is.infinite(x))) |> names()}}."
    ))
    cli::cli_alert_info(paste0
                        ("`Inf` values are probably caused by divisions ",
                          "by zero (e.g., when `n_benchmark` = 0). Consider using ",
                          "larger age groups that include more than 1 year."))
  }

  # Feedback when input contains missing values
  if (anyNA(data)) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns in `data` have missing values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { data |> select_if(function(x) any(is.na(x))) |> names()}}."
    ))
    cli::cli_alert_info("Missing values may lead to inaccurate evaluation
                        measures.")
  }

  # Feedback when results contain `Inf` values
  if (any(sapply(results, is.infinite))) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns in the output have `Inf` values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { results |> select_if(function(x) any(is.infinite(x))) |>
      names()}}."
    ))
  }

  # Feedback when output contains missing values
  if (anyNA(results)) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns in the output have missing values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { results |> select_if(function(x) any(is.na(x))) |> names()}}."
    ))
  }

  return(results)

}
