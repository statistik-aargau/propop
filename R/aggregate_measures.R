#' Compute aggregate evaluation measures per model variant
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
#' * `m_perc_e` is the mean percentage error (MPE; or mean algebraic percentage
#' error MALPE); it is a bias indicator as it takes the \bold{direction} of the
#' error into account. Positive values indicate overestimation and negative
#' values indicate underestimation. The smaller the value, the lower the bias.
#' * `md_perc_e` is the median percentage error (MedPE).
#' * `m_abs_perc_e` is the mean \bold{absolute} percentage (or: proportional)
#' error (MAPE). In contrast to `wm_abs_per_e` (see below), it gives equal
#' weighting to each absolute percentage error of which it is the mean (1/n).
#' It considers variance (or amplitude) and can be seen as a measure
#' of precision. The smaller the value, the lower is the average relative error.
#' * `md_abs_perc_e` is the median \bold{absolute} percentage error (MedAPE).
#' * `wm_abs_perc_e` is the weighted mean absolute percentage error (WMAPE); it
#' weights each absolute percentage error by the population size of the focal
#' group (e.g., municipality, nationality, age group) and thus considers domain
#' size. Put differently, errors count more in populous groups than in less
#' populous groups.
#' * `rmse` is the root mean square error; it is an indication of robustness of
#' the projection. The smaller the value, the more robust the projection.
#' * `n_measure` is the number of times a summary measure occurs (per weight
#' group if requested),
#' * `p_under_1` is a measure to gauge the error distribution; it indicates the
#' proportion of observations that have absolute percentage errors smaller than
#' 1%.
#' * `p_1_to_5` is a measure to gauge the error distribution; it indicates the
#' proportion of observations that have absolute percentage errors between 1%
#' and 5%.
#' * `p_under_5` is a measure to gauge the error distribution; it indicates the
#' proportion of observations that have absolute percentage errors smaller than
#' 5%.
#' * `p_5_to_10` is a measure to gauge the error distribution; it indicates the
#' proportion of observations that have absolute percentage errors between 5%
#' and 10%.
#' * `p_over_10` is a measure to gauge the error distribution; it indicates the
#' proportion of observations that have absolute percentage errors larger than
#' 10%.
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
      m_perc_e = mean(perc_e, na.rm = TRUE),
      md_perc_e = median(perc_e, na.rm = TRUE),
      m_abs_perc_e = mean(abs_perc_e, na.rm = TRUE),
      md_abs_perc_e = median(abs_perc_e, na.rm = TRUE),
      # TODO check carefully against literature
      wm_abs_perc_e = if ("w_abs_perc_e" %in% colnames(data)) {
        sum(w_abs_perc_e, na.rm = TRUE)
      } else {
        NA
      },
      # TODO check calculation of rmse
      rmse = sqrt(mean((n_bench - n_proj)^2)),
      # TODO ensure that this is divided by n of group size
      n_measure = n(),
      p_under_1 = sum(abs_perc_e < 1) / n_measure,
      p_1_to_5 = sum(abs_perc_e >= 1 & abs_perc_e <= 5) / n_measure,
      p_under_5 = sum(abs_perc_e < 5) / n_measure,
      p_5_to_10 = sum(abs_perc_e >= 5 & abs_perc_e <= 10) / n_measure,
      p_over_10 = sum(abs_perc_e > 10) / n_measure,
      .by = {{ weight_groups }}
    )
  # Remove `wm_abs_perc_e` if input doesn't include weight_groups
  if (!("weight" %in% colnames(data))) {
    results <- results |>
      dplyr::select(-wm_abs_perc_e)
  }

  # Feedback data includes weight but no group_weight argument provided
  if ("weight" %in% colnames(data) & is.null(weight_groups)) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_alert_warning(paste0("`data` includes results for weighted ",
                                  "groups but the `weight_groups` argument ",
                                  "is empty. Did you forget to specify the ",
                                  "groups as in `compute_measures`?")
    )
  }

  # Feedback when input contain `Inf` values
  if (any(sapply(data, is.infinite))) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns in `data` have `Inf` values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { data |> select_if(function(x) any(is.infinite(x))) |> names()}}."
    ))
  }

  # Feedback when input contains missing values
  if (anyNA(data)) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns in `data` have missing values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { data |> select_if(function(x) any(is.na(x))) |> names()}}."
    ))
  }

  # Feedback when results contain `Inf` values
  if (any(sapply(results, is.infinite))) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns in the output have `Inf` values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { results |> select_if(function(x) any(is.infinite(x))) |> names()}}."
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


# # tests
# aggregate_measures(snapshot1)
# aggregate_measures(snapshot1, weight_groups = "nat")
# aggregate_measures(snapshot1, weight_groups = c("nat", "sex"))
#
# aggregate_measures(snapshot4, weight_groups = NULL)
# aggregate_measures(snapshot4, weight_groups = c("nat", "age"))
