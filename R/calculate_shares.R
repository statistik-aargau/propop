#' Calculate shares for distributing immigration among subregions
#'
#' @param hist_data data frame, historical records (e.g., immigration from
#'        other cantons, countries or subregions). Required columns are: `year`,
#'        `spatial_unit`, `age` and a column that contains historical migration
#'        records aggregated by age demographic group. The columns `nat` and `sex`
#'        are optional.
#' @param imm_n character, name of the column which contains the
#'        data for aggregated historical migration records.
#' @param year_range \bold{(optional)} vector, years taken into consideration to
#'        calculate historic shares. Default uses all years present in the data.
#' @param age_group \bold{(optional)} integer, divides continuous age values into
#'        intervals for calculating shares. If the argument is not specified,
#'        the default uses 1-year age groups.
#' @param binational \bold{(optional)} boolean, `TRUE` indicates that the calculation
#'        discriminates between two groups of nationalities. `FALSE` indicates
#'        that the calculation does not distinguish between nationalities.
#' @returns
#' A data frame that includes the average share by demographic group.
#'
#' @export
#'
#' @autoglobal
#'
#' @examples
#' # Calculate shares to distribute subregional immigration among spatial units
#' calculate_shares(
#'   hist_data = ag_migration_subregional,
#'   imm_n = "imm_n",
#'   year_range = c(2022:2024),
#'   age_group = 10,
#'   binational = TRUE,
#'   two_sex = TRUE
#' )
calculate_shares <- function(
    hist_data,
    imm_n,
    year_range = NULL,
    age_group = 10,
    binational = TRUE,
    two_sex = TRUE) {

  # Test input ----
  ## Presence of mandatory columns ----
  assertthat::assert_that("year" %in% names(hist_data),
    msg = "column `year` is missing in `hist_data`."
  )
  assertthat::assert_that("spatial_unit" %in% names(hist_data),
    msg = "column `spatial_unit` is missing in `hist_data`."
  )
  assertthat::assert_that("age" %in% names(hist_data),
    msg = "column `age` is missing in `hist_data`."
  )
  assertthat::assert_that(
    imm_n %in% names(hist_data),
    msg = paste0("column `imm_n` is missing in `hist_data`.")
  )
  ## Data types and content ----
  assertthat::assert_that(
    is.numeric(hist_data$year),
    msg = paste0("Values in column `", year, "` must be numeric.")
  )
  assertthat::assert_that(
    is.numeric(hist_data$age),
    msg = paste0("Values in column `", age, "` must be numeric.")
  )

  # levels in spatial unit
  assertthat::assert_that(length(unique(as.factor(hist_data$spatial_unit))) > 1,
    msg = "Levels for spatial_units in `hist_data` must be larger than 1 level."
  )

  ## Optional arguments ----
  # Past years (default uses all years in `hist_data`)
  if (isTRUE(!is.null(year_range))){
    assertthat::assert_that(all(year_range %in% c(unique(hist_data$year))),
      msg = paste0("Vector for `year_range` does not correspond to ",
      "available years in `hist_data`."
    ))
  } else {
    year_range <- unique(hist_data$year)
  }

  # Age group contains no digits after the comma
  assertthat::assert_that(grepl("^[^.]*\\.?0*$", age_group),
    msg = paste0("Value for `age_group` must not contain any digits after the comma.")
  )
  assertthat::assert_that(age_group > 0,
    msg = paste0("Value for `age_group` must be larger than zero.")
  )

  # Nationality
  assertthat::assert_that(isTRUE(binational) | isFALSE(binational),
    msg = paste0("Value for `binational` must be either `TRUE` or `FALSE`")
  )

  if (binational == TRUE){
    assertthat::assert_that(
      "nat" %in% names(hist_data),
      msg = "Column `nat` is missing in `hist_data`."
    )
    # Factor levels
    assertthat::assert_that(
      length(unique(hist_data$nat)) == length(c("int", "ch")) &&
        all(hist_data$nat %in% c("int", "ch")),
      msg = paste0(
        "Column `nat` in `hist_data` must include the factor levels",
        " `ch` and `int`. \nMissing values (NA), other values, or only one ",
        "factor level are not allowed. ",
        "\nIf historic migration shares should not be projected for two",
        " nationalities, \nplease remove the column 'nat' from the data."
      )
    )
  } else if (binational == FALSE) {
    # Check if column `nat` is absent in `hist_data`
    assertthat::assert_that(
      !"nat" %in% names(hist_data),
      msg = paste0(
        "Argument `two_sex` is `FALSE` suggesting that the calculation \nshould",
        " not discriminate between nationalities. \nHowever, `hist_data` includes",
        " column `nat` suggesting multiple nationalities. \nPlease change argument",
        " `binational` or remove column `nat` from `hist_data`."
      )
    )
  }
  # Sex
  assertthat::assert_that(isTRUE(two_sex) | isFALSE(two_sex),
    msg = paste0("Value for `", two_sex, "` must be either `TRUE` or `FALSE`")
  )
  if (two_sex == TRUE) {
    assertthat::assert_that(
      "sex" %in% names(hist_data),
      msg = "Column `sex` is missing in `hist_data`."
    )
    # Factor levels
    assertthat::assert_that(
      length(unique(hist_data$sex)) == length(c("f", "m")) &&
        all(hist_data$sex %in% c("f", "m")),
      msg = paste0(
        "Column `sex` in `hist_data` must include the factor levels",
        " `f` and `m`. \nMissing values (NA), other values, or only one ",
        "factor level are not allowed. ",
        "\nIf historic migration shares should not be projected for two",
        " nationalities, \nplease remove the column 'sex' from the data."
      )
    )
  } else if (two_sex == FALSE) {
    # Check if column `sex` is absent in `hist_data`
    assertthat::assert_that(
      !"sex" %in% names(hist_data),
      msg = paste0(
        "Argument `two_sex` is `FALSE` suggesting that the calculation \nshould",
        " not discriminate between nationalities. \nHowever, `hist_data` includes",
        " column `sex` suggesting multiple nationalities. \nPlease change argument",
        " `two_sex` or remove column `sex` from `hist_data`."
      )
    )
  }

  # Clean data ----
  df_clean <- hist_data |>
    # filter years
    filter(year %in% year_range) |>
    # rename column with absolute migration numbers
    rename(imm_n = all_of(imm_n)) |>
    # select relevant columns
    select(any_of(c("year", "spatial_unit", "age", "nat", "sex", "imm_n"))) |>
    # convert spatial units to character
    mutate(spatial_unit = as.character(spatial_unit))

  # Column that contains historic records must be numeric
  assertthat::assert_that(
    is.numeric(df_clean$imm_n),
    msg = paste0("Values in column `", imm_n, "` must be numeric.")
  )


  # Prepare age groups ----
  if (age_group == 1) {
    df_prep <- df_clean
  } else if (age_group > 1){
    # Define breaks for age groups
    age_length = age_group
    age_from = age_length
    age_to = 101
    age_breaks = c(-Inf, seq(age_length - 1, age_to - 1, by = age_length), Inf)
    age_labels = c(paste0(
      seq(0, age_to - 1, by = age_length),
      "_",
      c(seq(age_length - 1, age_to - 1, by = age_length), 100)
    ))

    # Prepare age groups
    df_prep <- df_clean |>
      mutate(
        # aggregate 100+ year old people
        age = case_when(age > 100 ~ 100, .default = age),
        # create age groups
        age_group = cut(
          age,
          breaks = age_breaks,
          labels = age_labels,
          include.lowest = TRUE
        ),
        age_group = paste0("age_", age_group)
      )

    # Add people aged 99-100 years and older to the last age group
    age_group_98 <- as.character(df_prep$age_group[df_prep$age == 98][1])
    age_group_99 <- as.character(df_prep$age_group[df_prep$age == 99][1])
    age_group_100plus <- as.character(df_prep$age_group[df_prep$age == 100][1])

    df_prep <- df_prep |>
      mutate(
        age_group = case_when(
          (age == 100 & age_group_98 != age_group_100plus) ~ age_group_98,
          (age == 99 & age_group_98 != age_group_99) ~ age_group_98,
          .default = age_group
        ),
        age_group = str_replace(age_group, "99", "100_"))

    # Number of 1-year age groups within larger age groups
    df_age_group_n <- df_prep |>
      filter(year == year_range[1]) |>
      select(age, age_group) |>
      distinct() |>
      count(age_group, name = "age_group_n")
  }

  # Calculate mean shares ----
  df_result <- df_prep |>
    left_join(df_age_group_n, by = "age_group") |>
    mutate(
      # total number of people in 5-year age groups
      sum_imm_n = sum(imm_n),
      .by = any_of(c("spatial_unit", "nat", "sex", "age_group"))
    ) |>
    mutate(
      # calculate shares
      share = imm_n / sum_imm_n,
      # If no observations exist in a demographic group and its' larger age group,
      # values are filled with zeros to avoid NAs
      share = case_when((imm_n == 0 & sum_imm_n == 0) ~ 0, .default = share)
    ) |>
    # prune columns
    select(any_of(c(
      "year", "spatial_unit", "age", "age_group", "age_group_n", "nat", "sex",
      "imm_n", "sum_imm_n", "prop_imm", "share"
    )))

  # Ensure there are no missing values
  assertthat::assert_that(
    all(colSums(is.na(df_result)) == 0),
    msg = paste(
      "'df_result' contains missing values in columns:",
      paste(names(which(colSums(is.na(df_result)) > 0)), collapse = ", "))
  )

  return(df_result)
}
