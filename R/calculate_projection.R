#' Calculates the projection for ages 1-100.
#'
#' @param .data data frame, population and parameters for one year.
#'
#' @return data frame, contains the staring population `n_jan`, components, and
#' the resulting projected population for the next year `n_dec`.
#' @export
#'
calculate_projection <- function(.data, subregional = FALSE) {
  # Cohort component method
  .data |>
    mutate(
      # placeholder for newborns (those will be calculated later)
      births = 0,
      # international emigration
      emi_int_n = n_jan * (emi_int * (1 - (mor / 2))),
      # emigration to other cantons
      emi_nat_n = n_jan * (emi_nat * (1 - (mor / 2))),
      # acquisition of the Swiss citizenship
      acq_n = n_jan * (acq * (1 - (mor / 2))),
      # subtract new Swiss citizens from the international population
      acq_n = ifelse(nat == "ch", dplyr::lead(acq_n, 2 * 100), - acq_n),
      # mortality (deaths)
      mor_n = n_jan - (n_jan * (1 - mor)),
      # calculate the population balance
      n_dec = case_when(
        subregional == FALSE ~ n_jan - mor_n - emi_int_n - emi_nat_n + acq_n +
          imm_int_n + imm_nat_n,
        subregional == TRUE ~ n_jan - mor_n - emi_int_n - emi_nat_n + acq_n +
          imm_int_n + imm_nat_n + mig_sub,
        .default = NA
      )
    )
}
