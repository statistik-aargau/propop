#' Get mean historical subregional emigration to municipalities within the canton
#'
#' @description The function calculates the mean of past emigration to other
#'   municipalities within the canton for all municipalities and all possible
#'   combinations of age group, sex, and nationality.
#'
#' @param past_migration data frame, containing aggregated migration records.
#'        The data frame must include one row for each combination of year,
#'        spatial unit, nationality, sex, and age.
#' @param n_jan character, column containing the starting population (typically
#'        the number of people per group in January).
#' @param births character, column containing the number of births in the group
#'        of 0-year olds.
#' @param emi_n character, column containing the total number of people emigrating per
#'        spatial unit and demographic group.
#' @param spatial_unit character, column containing the spatial units.
#' @param method character, method to calculate average shares, i.e. `mean` or
#'        `median`.
#' @param year_range \bold{(optional)} vector, years taken into consideration to
#'        calculate historical shares. Default uses all years present in the data.
#' @param age_group \bold{(optional)} integer, divides continuous age values into
#'        intervals for calculating shares. If the argument is not specified,
#'        the default uses 1-year age groups.
#' @param binational \bold{(optional)} boolean, `TRUE` indicates that the calculation
#'        discriminates between two groups of nationalities. `FALSE` indicates
#'        that the calculation does not distinguish between nationalities.
#' @param two_sex \bold{(optional)} boolean, `TRUE` indicates that the calculation
#'        discriminates between two sexes. `FALSE` indicates that the calculation
#'        does not distinguish between sexes.
#'
#' @return A data frame that includes the average emigration rate.
#' @export
#' @autoglobal
#'
#' @examples
#' # Compute mean emigration rate
#'calculate_rates(
#'  past_migration = ag_migration_subregional,
#'  n_jan = n_jan,
#'  births = births,
#'  emi_n = emi_n,
#'  spatial_unit = spatial_unit,
#'  method = "mean",
#'  year_range = c(2022:2024),
#'  age_group = 5,
#'  binational = TRUE,
#'  two_sex = TRUE
#')

calculate_rates <- function(
    past_migration,
    n_jan,
    births,
    emi_n,
    spatial_unit,
    method,
    year_range = NULL,
    age_group = 1,
    binational = TRUE,
    two_sex = TRUE
) {
  # Ensure all required columns exist
  required_cols <- c(
    "year", "age", rlang::as_name(rlang::ensym(spatial_unit)),
    rlang::as_name(rlang::ensym(n_jan)), rlang::as_name(rlang::ensym(births)),
    rlang::as_name(rlang::ensym(emi_n))
  )

  missing_cols <- setdiff(required_cols, names(past_migration))
  assertthat::assert_that(length(missing_cols) == 0,
    msg = paste("Missing columns:", paste(missing_cols, collapse = ", "))
  )

  # Check if there are missing values
  # If only some years are used, NAs in other years are not problematic
  if (anyNA(past_migration)) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns of `past_migration` have missing values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { past_migration |>
      select(where(function(x) any(is.na(x)))) |>
      names()}}."
    ))
  }

  # Ensure method is either `mean` or `median`
  assertthat::assert_that(method %in% c("mean", "median"),
    msg = "method must be either `mean` or `median`."
  )

  # Check optional arguments
  # Past years (default uses all years in `past_migration`)
  if (isTRUE(!is.null(year_range))){
    assertthat::assert_that(all(year_range %in% c(unique(past_migration$year))),
      msg = paste0(
        "Vector for `year_range` does not correspond to available years in ",
        "`past_migration`."
      ))
  } else {
    year_range <- unique(past_migration$year)
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
      "nat" %in% names(past_migration),
      msg = "Column `nat` is missing in `past_migration`."
    )
    # Factor levels
    assertthat::assert_that(
      length(unique(past_migration$nat)) == length(c("int", "ch")) &&
        all(past_migration$nat %in% c("int", "ch")),
      msg = paste0(
        "Column `nat` in `past_migration` must include the factor levels",
        " `ch` and `int`. \nMissing values (NA), other values, or only one ",
        "factor level are not allowed. ",
        "\nIf historical migration shares should not be projected for two",
        " nationalities, \nplease remove the column 'nat' from the data."
      )
    )
  } else if (binational == FALSE) {
    # Check if column `nat` is absent in `past_migration`
    assertthat::assert_that(
      !"nat" %in% names(past_migration),
      msg = paste0(
        "Argument `binational` is `FALSE` suggesting that the calculation \nshould",
        " not discriminate between nationalities. \nHowever, `past_migration` includes",
        " column `nat` suggesting multiple nationalities. \nPlease change argument",
        " `binational` or remove column `nat` from `past_migration`."
      )
    )
  }

  # Sex
  assertthat::assert_that(isTRUE(two_sex) | isFALSE(two_sex),
    msg = paste0("Value for `", two_sex, "` must be either `TRUE` or `FALSE`")
  )

  if (two_sex == TRUE) {
    assertthat::assert_that(
      "sex" %in% names(past_migration),
      msg = "Column `sex` is missing in `past_migration`."
    )
    # Factor levels
    assertthat::assert_that(
      length(unique(past_migration$sex)) == length(c("f", "m")) &&
        all(past_migration$sex %in% c("f", "m")),
      msg = paste0(
        "Column `sex` in `past_migration` must include the factor levels",
        " `f` and `m`. \nMissing values (NA), other values, or only one ",
        "factor level are not allowed. ",
        "\nIf historical migration shares should not be projected for two",
        " nationalities, \nplease remove the column 'sex' from the data."
      )
    )
  } else if (two_sex == FALSE) {
    # Check if column `sex` is absent in `past_migration`
    assertthat::assert_that(
      !"sex" %in% names(past_migration),
      msg = paste0(
        "Argument `two_sex` is `FALSE` suggesting that the calculation \nshould",
        " not discriminate between levels of `sex`. \nHowever, `past_migration` includes",
        " column `sex` suggesting multiple levels of `sex`. \nPlease change argument",
        " `two_sex` or remove column `sex` from `past_migration`."
      )
    )
  }


  # Clean data ----
  df_clean <- past_migration |>
    # filter years
    filter(year %in% year_range) |>
    # select relevant columns
    select(
      year, spatial_unit, age, any_of(c("nat", "sex")), {{n_jan}}, {{births}},
      {{emi_n}}
    ) |>
    # Use number of births as starting population for 0-year olds
    mutate(n_base = if_else(age == 0, {{births}}, {{n_jan}})) |>
    rename(spatial_unit = {{spatial_unit}})


  # Prepare age groups ----
  if (age_group == 1) {
    df_prep <- df_clean |>
      mutate(age_group = as.character(age))
  } else if (age_group > 1){
    # Define breaks for age groups
    age_length = age_group
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
        age_group = stringr::str_replace(age_group, "99", "100_"))
  }

  # Make a concise string from `year_range`
  format_years <- function(year_range) {
    years <- sort(unique(year_range))

    # Find where consecutive gaps occur
    gaps <- c(0, diff(years))
    group <- cumsum(gaps != 1)

    # For each group, collapse to "start-end" or just "year"
    ranges <- tapply(years, group, function(g) {
      if (length(g) > 1) paste(min(g), max(g), sep = "-")
      else as.character(g)
    })

    paste(ranges, collapse = ", ")
  }

  year_range_string <- format_years(year_range)

  # Calculate mean rates ----
  df_rate <- df_prep |>
    mutate(
      emi_rate = case_when(
        # 1. If nobody left share should be 0.
        {{emi_n}} == 0 ~ 0,
        # 2. If nobody was in population in January and somebody left share should
        #    be 1 as "everybody" left.
        n_base == 0 ~ 1,
        # 3. In all other cases share of persons leaving is calculated as ratio
        #    between people leaving and population in January.
        .default = pmin({{emi_n}} / n_base, 1)
      )
    )

  # Columns to summarize across
  cols_summarize <- df_rate |>
    select(spatial_unit, age_group, any_of(c("sex", "nat"))) |>
    colnames()

  if (method == "mean") {
    df_rate_mean <- df_rate |>
      mutate(
        .by = c(cols_summarize),
        emi_rate = mean(emi_rate, na.rm = TRUE)
      )
  } else if (method == "median") {
    df_rate_mean <- df_rate |>
      mutate(
        .by = c(cols_summarize),
        emi_rate = median(emi_rate, na.rm = TRUE)
      )
  }

  # Finalize output ----
  df_result <- df_rate_mean |>
    dplyr::mutate(
      spatial_unit = as.character(spatial_unit),
      # age = as.character(age),
      method = paste0(method, " ", year_range_string)
    ) |>
    # prune columns
    select(
      year, spatial_unit, age, any_of(c("age_group", "nat", "sex")), {{n_jan}},
      {{births}}, n_base, {{emi_n}}, emi_rate, method
    )

  # Ensure there are no missing values
  assertthat::assert_that(
    all(colSums(is.na(df_result)) == 0),
    msg = paste(
      "'df_result' contains missing values in columns:",
      paste(names(which(colSums(is.na(df_result)) > 0)), collapse = ", "))
  )

  return(df_result)
}
