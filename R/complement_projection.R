#' Add identifiers to the raw results from the projection and arrange components
#' to build the population balance.
#'
#' @description
#' Adds information about demographic groups and years to make the raw results
#' from `project_raw.R` more informative. Arranges components of the population
#' balance.
#'
#' @param skeleton empty data frame containing all combinations of demographic
#'    groups, years, and spatial levels.
#' @param projection_raw data frame showing the projection results obtained with
#'    `project_raw`.
#' @param subregional boolean, TRUE indicates that subregional migration
#'     patterns (e.g., movement between municipalities within a canton)
#'     are part of the projection.
#'
#' @return data frame containing:
#'    * five identifiers (age, sex, optionally: nationality, year,
#'      spatial levels). These indicate to which demographic group and year the
#'      results in the rows refer to.
#'    * absolute population per demographic group and year (n).
#'    * projection components that form the population of the next year.
#'    * population balance (balance_n) of the next projected year. The formula
#'      sums up the components like so:
#'      balance_n = n + births - deaths - international emigrants -
#'      intercantonal emigrants + international immigrants +
#'      intercantonal immigrants + naturalized citizens
#'      (+ optionally: subregional migrants).
#'    * annual population change per demographic group in absolute numbers
#'      (pop_change_n) and as percentages (pop_change_perc).
#'
#' @autoglobal
#' @noRd

complement_projection <- function(skeleton, projection_raw, subregional) {
  # check structure of input
  assertthat::assert_that(
    is.numeric(skeleton$age),
    msg = "Column 'age' must be numeric."
  )

  assertthat::assert_that(
    is.factor(skeleton$sex),
    all(levels(skeleton$sex) %in% c("m", "f")),
    msg = "Column 'sex' must be a factor with either 'm' and 'f' as level."
  )

  assertthat::assert_that(
    is.factor(skeleton$nat),
    all(levels(skeleton$nat) %in% c("ch", "int")),
    msg = "Column 'nat' must be a factor with either 'ch' or 'int' as level."
  )

  assertthat::assert_that(
    is.numeric(skeleton$year),
    all(skeleton$year >= 2018),
    all(skeleton$year <= 2061),
    !any(is.na(skeleton$year)),
    msg = "`year` must only inlcude numeric values between 2018 and 2061."
  )

  # apply skeleton
  projection_result <- skeleton |>
    dplyr::bind_cols(projection_raw) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower)

  # Remove mig_sub if subregional is false
  if (subregional == FALSE) {
    projection_result <- projection_result |>
      dplyr::select(-mig_sub)
  }

  # arrange components
  projection_result |>
    # aggregate people aged 100 and older
    dplyr::mutate(
      new_age_group_100 = ifelse(age == 100, 100, NA),
      age = dplyr::case_when(age < 100 ~ age + 1, TRUE ~ age)
    ) |>
    dplyr::mutate(n = sum(n), .by = c(year, nat, sex, age)) |>
    dplyr::mutate(
      # adapt age range to 0-100 years
      age = age - 1,
      # label age group of people aged 100 years and older as age = "100"
      age = dplyr::coalesce(new_age_group_100, age),
      # age the population by one year
      n = dplyr::lag(n, 1),
      # set the starting population to zero for newborns
      n = dplyr::case_when(age == 0 ~ 0, TRUE ~ n),
      # add new Swiss citizens and subtract the same number of people from the
      # international group
      acq = ifelse(nat == "ch", dplyr::lead(acq, 2 * 101), - acq),
    ) |>
    dplyr::mutate(
      # calculate population balance
      balance_n = n + births - mor - emi_int - emi_nat + imm_int + imm_nat + acq,
      balance_n = if ("mig_sub" %in% names(projection_result))
        balance_n + mig_sub else balance_n,
      # calculate the annual change per demographic group
      ## total number of people
      pop_change_n = round(balance_n - n, 0),
      ## percentage
      pop_change_perc = round((100 / n) * pop_change_n, 1),
      # percentages for newborns are NAs
      pop_change_perc = ifelse(age == 0, NA, pop_change_perc)
    ) |>
    # clean the data
    dplyr::select(any_of(c(
      "year", "age", "sex", "nat", "n", "births", "mor", "emi_int", "emi_nat",
      "imm_int", "imm_nat", "acq", "mig_sub", "balance_n", "pop_change_n",
      "pop_change_perc"
    )))
}
