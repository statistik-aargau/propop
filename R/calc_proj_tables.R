#' Calculates the projection.
#'
#' @param .data data frame, population and parameters for one year.
#'
#' @return data frame, contains the staring population `n_jan`, components, and
#' the resulting projected population for the next year `n_dec`.
#' @export
#'
calc_proj_tables <- function(.data) {
  .data |>
    # apply FSO method for projections
    mutate(
      # rates (resulting people are subtracted from the population) -----
      ## mortality -----
      mor_n = case_when(
        # ages 1-100
        age > 0 ~ n_jan - (n_jan * (1 - mor_rate)),
        # newborns
        age == 0 ~ births - (births * (1 - mor_rate)),
        TRUE ~ NA
      ),
      # international emigration -----
      emi_int_n = case_when(
        # ages 1-100
        age > 0 ~ n_jan * (emi_int_rate * (1 - (mor_rate / 2))),
        # newborns
        age == 0 ~ births * (emi_int_rate * (1 - (mor_rate / 2))),
        TRUE ~ NA
      ),
      # emigration to other cantons -----
      emi_nat_n = case_when(
        # ages 1-100
        age > 0 ~ n_jan * (emi_nat_rate * (1 - (mor_rate / 2))),
        # newborns
        age == 0 ~ births * (emi_nat_rate * (1 - (mor_rate / 2))),
        TRUE ~ NA
      ),
      # surviving newborns -----
      birth_balance = case_when(
        age == 0 ~ births - mor_n - emi_int_n - emi_nat_n, TRUE ~ NA
      ),
      # absolute numbers (are added to the population) -----
      ## acquisition of Swiss citizenship -----
      acq_n = case_when(
        # ages 1-100
        age > 0 ~ n_jan * (acq_rate * (1 - (mor_rate / 2))),
        # newborns
        age == 0 ~ (1 / 2) * birth_balance * (acq_rate * (1 - (mor_rate / 2))),
        TRUE ~ NA
      ),
      acq_n = ifelse(nat == "ch", dplyr::lead(acq_n, 2 * 101), -acq_n),
      ## international immigration -----
      imm_int_nn = imm_int_n * (1 - (mor_rate / 2)),
      ## immigration from other cantons
      imm_nat_nn = imm_nat_n * (1 - (mor_rate / 2)),
      # calculate the new population in December of the respective year -----
      # population balance
      n_dec = n_jan - mor_n - emi_int_n - emi_nat_n + acq_n + imm_int_n +
        imm_nat_n,
      # add newborns
      n_dec = case_when(age == 0 ~ n_dec + births, TRUE ~ n_dec)
    ) |>
    # clean the data
    select(
      year:spatial_unit, n_jan, births, mor_n, emi_int_n, emi_nat_n, imm_int_n,
      imm_nat_n, acq_n, n_dec
    )
}
