#' Add identifiers to the raw results from the projection and arrange components
#' to build the population balance.
#'
#' `r lifecycle::badge("deprecated")`
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
#'    patterns (e.g., movement between municipalities within a canton)
#'    are part of the projection.
#'
#' @return data frame containing:
#'    * five identifiers (age, sex, optionally: nationality, year,
#'      spatial levels). These indicate to which demographic group and year the
#'      results in the rows refer to.
#'    * absolute population per demographic group and year (n).
#'    * projection components that form the population of the next year.
#'    * population balance (n_1) of the next projected year. The formula
#'      sums up the components like so:
#'      n_1 = n + births - deaths - international emigrants -
#'      intercantonal emigrants + international immigrants +
#'      intercantonal immigrants + naturalized citizens
#'      (+ optionally: subregional migrants).
#'    * annual population change per demographic group in absolute numbers
#'      (delta_n) and as percentages (delta_perc).
#'
#' @autoglobal
#' @noRd

complement_projection <- function(skeleton,
                                  projection_raw,
                                  subregional) {
  # Deprecate
  lifecycle::deprecate_warn(
    "2.0.0", "complement_projection()",
    details = paste0(
      "`complement_projection()` is still operational as part of `propop_legacy()` but ",
      "won't be further maintained"
    )
  )
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
    !any(is.na(skeleton$year)),
    msg = "`year` must only inlcude numeric values."
  )

  assertthat::assert_that(
    "scen" %in% names(projection_raw),
    msg = "Column `scen` is missing in `projection_raw`."
  )

  assertthat::assert_that(
    "spatial_unit" %in% names(projection_raw),
    msg = "Column `spatial_unit` is missing in `projection_raw`."
  )

  assertthat::assert_that(
    is.factor(projection_raw$scen),
    msg = "Column `scen` in `projection_raw` must be a factor."
  )

  assertthat::assert_that(
    is.factor(projection_raw$spatial_unit),
    msg = "Column `spatial_unit` in `projection_raw` must be a factor."
  )


  # TODO ensure identical arrangement of scen & spatial_unit
  # TODO or remove these columns from skeleton
  # TODO or don't create them in the first place (just repeat skeleton as often as require)

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
    # n becomes the population of year + 1 in january (n_dec refers to the
    # projected population at the end of the year (december))
    rename(n_jan = n) |>
    # aggregate people aged 100 and older
    dplyr::mutate(
      new_age_group_100 = ifelse(age == 100, 100, NA),
      age = dplyr::case_when(age < 100 ~ age + 1, TRUE ~ age)
    ) |>
    dplyr::mutate(
      n_jan = sum(n_jan),
      .by = c(year, scen, nat, sex, age, spatial_unit)
    ) |>
    dplyr::mutate(
      # adapt age range to 0-100 years
      age = age - 1,
      # label age group of people aged 100 years and older as age = "100"
      age = dplyr::coalesce(new_age_group_100, age),
      # age the population by one year
      n_jan = dplyr::lag(n_jan, 1),
      # set the starting population to zero for newborns
      n_jan = dplyr::case_when(age == 0 ~ 0, TRUE ~ n_jan),
      # add new Swiss citizens and subtract the same number of people from the
      # international group
      acq = ifelse(nat == "ch", dplyr::lead(acq, 2 * 101), -acq),
    ) |>
    dplyr::mutate(
      # calculate population balance
      n_dec = n_jan + births - mor - emi_int - emi_nat + imm_int + imm_nat + acq,
      n_dec = if ("mig_sub" %in% names(projection_result)) {
        n_dec + mig_sub
      } else {
        n_dec
      },
      # calculate the annual change per demographic group
      ## total number of people
      delta_n = round(n_dec - n_jan, 0),
      ## percentage
      delta_perc = round((delta_n / n_jan) * 100, 3),
      # percentages for newborns are NAs
      delta_perc = ifelse(age == 0, NA, delta_perc),
      # percentages if `n_jan`and `n_dec` are both zero
      delta_perc = case_when(
        (round(n_jan, 0) == 0 & round(n_dec, 0) == 0) ~ 0,
        .default = delta_perc
      ),
      year = year + 1
    ) |>
    dplyr::filter(year < max(year)) |>
    # clean the data
    dplyr::select(any_of(c(
      "year", "scen", "spatial_unit", "age", "sex", "nat", "n_jan", "births",
      "mor_n" = "mor", "emi_int_n" = "emi_int", "emi_nat_n" = "emi_nat",
      "imm_int_n" = "imm_int", "imm_nat_n" = "imm_nat", "acq_n" = "acq",
      "mig_sub", "n_dec", "delta_n", "delta_perc"
    )))
}
