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
#' @noRd
#'
calculate_projection <- function(.data, subregional = subregional) {

  # Transition of each age-cohort to the next age
  df_transition <- .data |>
    mutate(
      .by = c(year, scen, spatial_unit, sex, age),
      # placeholder for newborns
      births = 0,
      # calculate the number of deaths
      mor_n = case_when(
        # Swiss population
        nat == "ch" ~ n_jan * (mor - ((emi_int + emi_nat) * (mor / 2))),
        # international population
        nat == "int" ~ n_jan * (mor - (emi_int + emi_nat + acq) * (mor / 2)),
        .default = NA
      ),
      # international emigration
      emi_int_n = n_jan * emi_int,
      # emigration to other cantons
      emi_nat_n = n_jan * emi_nat,
      # acquisition of the Swiss citizenship
      acq_n = n_jan * acq,
      # subtract new Swiss citizens from the international population
      acq_n = ifelse(nat == "ch", dplyr::lead(acq_n, order_by = nat), -acq_n)
    )

  # Number of deaths of international people acquiring Swiss citizenship
  # (Swiss mortality rates for new Swiss citizens)
  df_mor_acq_helper <- .data |>
    filter(nat == "ch") |>
    select(any_of(c("year", "nat", "sex", "age", "spatial_unit", "scen", "mor"))) |>
    mutate(nat = "int")

  # Number of deaths after nationality change through citizenship acquisition
  df_mor_acq <- .data |>
    filter(nat == "int") |>
    select(any_of(c(
      "year", "nat", "sex", "age", "spatial_unit", "scen", "n_jan", "acq"
    ))) |>
    # add Swiss mortality rates
    left_join(
      df_mor_acq_helper,
      by = join_by(year, nat, sex, age, spatial_unit, scen)
    ) |>
    mutate(
      # calculate the number of deaths for new Swiss citizens
      mor_n_new = n_jan * (acq * (mor / 2)),
      # change nationality from international to Swiss
      nat = "ch"
    ) |>
    select(year, nat, sex, age, spatial_unit, scen, mor_n_new)

  # Add new citizens to the Swiss population
  df_transition_mor <- df_transition |>
    left_join(
      df_mor_acq,
      by = join_by(year, nat, sex, age, spatial_unit, scen)
    ) |>
    mutate(
      mor_n = case_when(nat == "ch" ~ mor_n + mor_n_new, .default = mor_n)
    ) |>
    select(any_of(c(
      "year", "nat", "sex", "age", "spatial_unit", "scen", "mor_n"
    )))

  # Combine new Swiss citizens's number of deaths with the rest of the population
  df_transition_complete <- df_transition |>
    # Replace by number of deaths which include new Swiss citizens
    select(-mor_n) |>
    left_join(
      df_transition_mor,
      by = join_by(year, nat, sex, age, spatial_unit, scen)
    )

  # Adapt mortality rate for aggregated people of age 100 and older
  df_mor_100plus <- .data |>
    select(year, nat, sex, age, spatial_unit, scen, mor) |>
    mutate(age = age - 1) |>
    bind_rows(
      .data |>
        select(year, nat, sex, age, spatial_unit, scen, mor) |>
        mutate(age = age - 1) |>
        filter(age == 99) |>
        mutate(age = 100)
    )

  df_out <- df_transition_complete |>
    select(any_of(c(
      "year", "nat", "sex", "age", "spatial_unit", "scen", "n_jan", "births",
      "mor_n", "emi_int_n", "emi_nat_n", "acq_n", "imm_int_n", "imm_nat_n",
      "n_dec", "mig_sub", "emi_sub", "imm_sub", "int_mothers", "birthrate"
    ))) |>
    left_join(
      df_mor_100plus,
      by = join_by(year, nat, sex, age, spatial_unit, scen)
    ) |>
    mutate(
      # take immigration from other cantons and countries into account
      mor_n = mor_n + (imm_nat_n * mor / 2) + (imm_int_n * mor / 2),
      # calculate the population balance for the transition
      n_dec = n_jan + births - mor_n - emi_int_n - emi_nat_n + acq_n +
        imm_int_n + imm_nat_n
    ) |>
    select(any_of(c(
      "year", "nat", "sex", "age", "spatial_unit", "scen", "n_jan",
      "mor_n", "emi_int_n", "emi_nat_n", "acq_n", "imm_int_n", "imm_nat_n",
      "n_dec", "mig_sub", "emi_sub", "imm_sub", "int_mothers", "birthrate"
    )))

  # Subregional migration ----
  if (!is.null(subregional) && subregional == "net") {
    # Add net saldo for subregional migration
    df_out |> mutate(n_dec = n_dec + mig_sub)
  } else if (!is.null(subregional) && subregional == "rate") {
    # Redistribute subregional emigration back to all subregional units as
    # subregional immigration
    df_out <- df_out |>
      left_join(
        .data |>
          select(any_of(c(
            "year", "nat", "sex", "age", "spatial_unit", "scen", "mor"
          ))),
        by = join_by(year, nat, sex, age, spatial_unit, scen)
      ) |>
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
