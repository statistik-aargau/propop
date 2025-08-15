#' Calculates the projection for ages 1-100.
#'
#' @description Uses the cohort component method to project population development.
#'
#' @param .data data frame, population and parameters for one year.
#' @param subregional character or NULL, indicates if subregional migration
#'        patterns (e.g., movement between municipalities within a canton) are
#'        part of the projection (default `subregional = NULL`). Requires input
#'        on the level of subregions (in `parameters` and `population`).
#'        Two calculation methods are supported to distribute people between
#'        subregions: With `subregional = "net"`, the net migration between
#'        subregions is added to the population balance. Net migration numbers
#'        must be specified in a data column `mig_sub` in `parameters`.
#'        With `subregional = "rate"`, the numbers for subregional emigrants are
#'        subtracted from the population balance, then redistributed back to all
#'        subregional units as subregional immigration; `parameters` must contain
#'        the columns `emi_sub` and `imm_sub`.
#'
#' @return Returns a data frame with the starting population `n_jan`, components,
#'         and the resulting projected population for the next year `n_dec`.
#' @export
#' @autoglobal
#'
calculate_projection <- function(.data, subregional = subregional) {
  df_out <- .data |>
    mutate(
      .by = c(year, scen, spatial_unit, sex, age),
      # placeholder for newborns (those will be calculated later)
      births = 0,
      # international emigration
      emi_int_n = n_jan * (emi_int * (1 - (mor / 2))),
      # emigration to other cantons
      emi_nat_n = n_jan * (emi_nat * (1 - (mor / 2))),
      # acquisition of the Swiss citizenship
      acq_n = n_jan * (acq * (1 - (mor / 2))),
      # subtract new Swiss citizens from the international population
      acq_n = ifelse(nat == "ch", dplyr::lead(acq_n, order_by = nat), -acq_n),
      # mortality (deaths)
      mor_n = n_jan - (n_jan * (1 - mor)),
      # calculate the population balance
      n_dec = n_jan + births - mor_n - emi_int_n - emi_nat_n + acq_n + imm_int_n +
        imm_nat_n
    )

  # Subregional migration ----
  if (!is.null(subregional) && subregional == "net") {

    # Add net saldo for subregional migration
    df_out |> mutate(n_dec = n_dec + mig_sub)

  } else if (!is.null(subregional) && subregional == "rate") {

    # Redistribute subregional emigration back to all subregional units as
    # subregional immigration
    df_out <- df_out |>
      mutate(
        # emigration to other subregional units
        emi_sub_n = n_jan * (emi_sub * (1 - (mor / 2))),
        # calculate the population balance
        n_dec = n_dec - emi_sub_n
      )

    # Get total of subregional emigration
    df_emi_sub_n_total <- df_out |>
      dplyr::summarise(
        emi_sub_n_total = sum(emi_sub_n),
        .by = c(year, scen, nat, sex, age)
      )

    # Redistribution according to provided shares for each subregion
    df_out |>
      dplyr::left_join(
        df_emi_sub_n_total,
        by = dplyr::join_by(year, scen, nat, sex, age)
      ) |>
      dplyr::mutate(
        imm_sub_n = imm_sub * emi_sub_n_total,
        n_dec = n_dec + imm_sub_n
      )
  } else {

    # No subregional migration
    return(df_out)

  }
}
