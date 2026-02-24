#' Check if population equation components add up within one year
#'
#' Function to check if the components in the `propop::propop()` output add up.
#' Takes the population at the beginning of the year (`n_jan`), adds all
#' components (births - mor - emi_int - emi_nat + imm_int + imm_nat + acq) and
#' checks if the sum is equal to the population at the end of the year (`n_dec`).
#'
#' @param data data frame containing population projections; can be
#' created with `propop::propop()`.
#'
#' @return data frame containing summary statistics.
#' @export
#'
#' @autoglobal
#'
#' @examples
#'propop(
#'  parameters = fso_parameters,
#'  year_first = 2024,
#'  year_last = 2027,
#'  population = fso_population,
#'  subregional = FALSE,
#'  binational = TRUE
#') |>
#'  check_balance()

check_balance <- function(data){

  df_check <- data |>
    dplyr::mutate(
      pop_balance = n_jan + births - mor_n - emi_int_n - emi_nat_n + imm_int_n + imm_nat_n,
      pop_balance = if ("acq_n" %in% names(data))
        pop_balance + acq_n else pop_balance,
      pop_balance = if ("mig_sub" %in% names(data))
        pop_balance + mig_sub else pop_balance,
      diff = round(n_dec - pop_balance, 0)
    )
  summary <- df_check |>
    dplyr::summarize(
      rows = n(),
      nonzeros = sum(diff != 0, na.rm = TRUE),
      missings = sum(is.na(diff))
    )

  # Feedback
  cli::cli_h1("Result of population equation components check")
  cli::cli_alert_info("Total rows checked: {.val {summary$rows}}")
  if (summary$nonzeros != 0 || summary$missings != 0) {
    cli::cli_alert_warning(paste0(
      "Check failed:\n",
      "- Rows in which population equation differs from zero: {.val {summary$nonzeros}}\n",
      "- Rows in which equation check has missing values: {.val {summary$missings}}")
      )
  } else {
    cli::cli_alert_success(paste0(
      "Check passed: Equations in all rows add up ",
      "and there are no missing values.")
    )
  }


  invisible(summary)
}
