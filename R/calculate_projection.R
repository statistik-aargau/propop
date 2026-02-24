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

  # Define ID-columns for joins
  id_cols <- c("year", "nat", "sex", "age", "spatial_unit", "scen")

  # Step 2: Transition of each demographic cohort to the next age ----
  df_transition <- .data |>
    mutate(
      .by = c(year, scen, spatial_unit, sex, age),
      # placeholder for newborns
      births = 0,
      # international emigration
      emi_int_n = n_jan * emi_int,
      # emigration to other cantons
      emi_nat_n = n_jan * emi_nat,
      # acquisition of the Swiss citizenship
      acq_n = n_jan * acq,
      # subtract new Swiss citizens from the international population
      acq_n = ifelse(nat == "ch", dplyr::lead(acq_n, order_by = nat), -acq_n)
    )

  # Step 2: Calculate the number of deaths ----
  # Subset data for mortality rates of international people
  df_mor_int <- vctrs::vec_slice(.data, .data$nat == "int")[c(
    id_cols, "n_jan", "mor", "emi_int", "emi_nat", "acq"
  )]

  # Subset data for mortality rates of Swiss people
  df_mor_ch <- vctrs::vec_slice(.data, .data$nat == "ch")[c(
    id_cols, "n_jan", "mor", "emi_int", "emi_nat", "acq"
  )]

  # Calculate the number of deaths for international people
  int_mor <- df_mor_int |>
    mutate(mor_n_int = n_jan * (mor - (emi_int + emi_nat + acq) * (mor / 2))) |>
    select(all_of(id_cols), mor_n_int)

  # Calculate the number of deaths for people who acquired Swiss citizenship
  ch_acq_mor <- df_mor_ch[c(id_cols, "mor")] |>
    # get mortality rates of Swiss people
    mutate(nat = "int") |>
    # join international people who acquired Swiss citizenship
    left_join(df_mor_int[c(id_cols, "n_jan", "acq")], by = id_cols) |>
    # calculate the number of deaths
    mutate(mor_n_acq = n_jan * (acq * (mor / 2))) |>
    select(-c(n_jan, mor, acq))

  # Adapt nationality from international to Swiss
  ch_acq_mor$nat <- "ch"

  # Calculate the number of deaths for old and new Swiss citizens
  ch_mor <- df_mor_ch |>
    # join new Swiss citizens
    left_join(ch_acq_mor, by = id_cols) |>
    # calculate the number of deaths
    mutate(
      mor_n_ch = sum(
        # existing Swiss population
        n_jan * (mor - ((emi_int + emi_nat) * (mor / 2))) +
          # new Swiss citizens
          mor_n_acq,
        na.rm = TRUE
      ),
      .by = all_of(id_cols)
    ) |>
    select(all_of(id_cols), mor_n_ch)

  # Step 3: Adapt mortality rate for the transitioned population ----
  # Also, aggregates people of age 100 and older
  df_mor_transitioned <- .data |>
    mutate(age = age - 1) |>
    bind_rows(filter(.data, age == 100)) |>
    select(all_of(id_cols), mor)

  # Step 4: Calculate immigration and balance for the transitioned population ----
  df_out <- df_transition |>
    select(-mor) |>
    # Join previously calculated number of deaths by nationality
    left_join(ch_mor, by = id_cols) |>
    left_join( int_mor, by = id_cols) |>
    mutate(mor_n = sum(mor_n_ch, mor_n_int, na.rm = TRUE), .by = all_of(id_cols)) |>
    # Complete people of age 100 and older
    left_join(df_mor_transitioned, by = id_cols) |>
    mutate(
      # add immigration from other cantons and countries
      mor_n = mor_n + (imm_nat_n * mor / 2) + (imm_int_n * mor / 2),
      # calculate the population balance for the transition
      n_dec = n_jan + births - mor_n - emi_int_n - emi_nat_n + acq_n +
        imm_int_n + imm_nat_n
    ) |>
    # prune columns
    select(-c(mor_n_int, mor_n_ch, mor))

  # Optional Step 5: Subregional migration ----
  if (!is.null(subregional) && subregional == "net") {
    # Add net saldo for subregional migration
    df_out |> mutate(n_dec = n_dec + mig_sub)
  } else if (!is.null(subregional) && subregional == "rate") {
    # Redistribute subregional emigration back to all subregional units as
    # subregional immigration
    df_out <- df_out |>
      left_join(.data |> select(all_of(id_cols), mor), by = id_cols) |>
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
