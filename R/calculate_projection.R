#' Calculates the projection for ages 1-100.
#'
#' @param .data data frame, population and parameters for one year.
#'
#' @return data frame, contains the staring population `n_jan`, components, and
#' the resulting projected population for the next year `n_dec`.
#' @export
#' @autoglobal
#'
calculate_projection <- function(.data, subregional = FALSE) {
  # Cohort component method
  out <- .data |>
    mutate(
      .by = spatial_unit,
      # placeholder for newborns (those will be calculated later)
      births = 0,
      # international emigration
      emi_int_n = n_jan * (emi_int * (1 - (mor / 2))),
      # emigration to other cantons
      emi_nat_n = n_jan * (emi_nat * (1 - (mor / 2))),
      # emigration to other subregional units
      emi_sub_n = n_jan * (emi_sub * (1 - (mor / 2))),
      # acquisition of the Swiss citizenship
      acq_n = n_jan * (acq * (1 - (mor / 2))),
      # subtract new Swiss citizens from the international population
      acq_n = ifelse(nat == "ch", dplyr::lead(acq_n, 2 * 100), - acq_n),
      # mortality (deaths)
      mor_n = n_jan - (n_jan * (1 - mor)),
      # calculate the population balance
      n_dec = n_jan - mor_n - emi_int_n - emi_nat_n - emi_sub_n +
          acq_n + imm_int_n + imm_nat_n
    )

  # Redistribute subregional emigration back to all subregional units as immigration
  if(subregional){
    # Get total of subregional emigration
    dat_emi_sub_n_total <- out |>
      dplyr::summarise(.by = c(year, nat, sex, age), emi_sub_n_total = sum(emi_sub_n))

    # Redistribution according to provided shares for each subregion
    out |>
      dplyr::left_join(dat_emi_sub_n_total,
                       by = dplyr::join_by(year, nat, sex, age)) |>
      dplyr::mutate(imm_sub_n = imm_sub * emi_sub_n_total,
                    n_dec = n_dec + imm_sub_n)
  } else{
    return(out)
  }
}
