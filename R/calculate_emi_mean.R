#' Calculate average past emigration to spatial units within the region
#'
#' @description Calculates the mean of past emigration to other places
#'   within the region for all spatial units and for all combinations of
#'   age group, sex, and nationality.
#'
#' @param hist_data data frame, containing aggregated migration records.
#'        The data frame must include one row for each combination of year,
#'        spatial unit, nationality, sex, and age.
#' @param n_jan character, column containing the starting population (typically
#'        the number of people per group in January).
#' @param births character, column containing the number of births in the group
#'        of 0-year olds.
#' @param emi_n character, column containing the total number of people emigrating per
#'        spatial unit and demographic group.
#' @param spatial_unit character, column containing the spatial units.
#' @param year_n numeric, number of years to be considered for computing the mean.
#'
#' @returns
#' A data frame that includes the average emigration rate per demographic group
#' and spatial unit. `emi_rate` can be used as `emi_sub` parameter when
#' `propop::propop()` uses `subregional = "rate"`.
#'
#' @seealso
#' [propop()] for details on how to account for subregional migration using the rate method,
#' [calculate_shares()] for calculating the associated immigration share `imm_sub`.
#' @export
#'
#' @examples
#' # Compute mean emigration rate
#' calculate_emi_mean(
#' hist_data = ag_migration_subregional,
#' n_jan = n_jan,
#' births = births,
#' emi_n = emi_n,
#' spatial_unit = spatial_unit,
#' year_n = NULL
#' )

calculate_emi_mean <- function(
    hist_data,
    n_jan,
    births,
    emi_n,
    spatial_unit,
    year_n = NULL
) {


  # Ensure all required columns exist
  required_cols <- c("year", "age", "sex", "nat", rlang::as_name(rlang::ensym(spatial_unit)),
                     rlang::as_name(rlang::ensym(n_jan)), rlang::as_name(rlang::ensym(births)),
                     rlang::as_name(rlang::ensym(emi_n)))

  missing_cols <- setdiff(required_cols, names(hist_data))
  assertthat::assert_that(length(missing_cols) == 0, msg = paste("Missing columns:", paste(missing_cols, collapse = ", ")))

  # Check if there are missing values
  # If only some years are used, NAs in other years are not problematic
  if (anyNA(hist_data)) {
    cli::cli_text(cli::col_red("Warning message:"))
    cli::cli_text("The following columns of `hist_data` have missing values:")
    cli::cli_alert_warning(cli::col_magenta(
      "{.val { hist_data |>
      select(where(function(x) any(is.na(x)))) |>
      names()}}."
    ))
  }


  # If year is not indicated, use all years
  if (is.null(year_n)) {
    year_n <- max(hist_data$year) - min(hist_data$year) + 1
  }


  # Ensure plausible value for year_n
  assertthat::assert_that(year_n <= max(hist_data$year) - min(hist_data$year) + 1,
                          msg = "`year_n` cannot be larger than the available number of years")

  assertthat::assert_that(year_n >= 1,
                          msg = "`year_n` must be 1 or larger")

  year_last <- max(hist_data$year)
  emi_var <- rlang::as_label(rlang::ensym(emi_n))

  df <- hist_data |>
    filter(year %in% (year_last - year_n + 1):year_last)

  n_spatial_units <- df |> dplyr::distinct({{ spatial_unit }}) |> nrow()

  # Ensure that the dimensions are correct
  assertthat::assert_that(
    nrow(df) == year_n * n_spatial_units * 404,
    msg = "Unexpected number of rows: expected year_n * n_spatial_units * 404."
  )

  df_result <- df |>
    mutate(
      # Use number of births as starting population for 0-year olds
      n_base = if_else(age == 0, {{births}}, {{n_jan}}),
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
    ) |>
    rename(spatial_unit = {{spatial_unit}}) |>
    summarise(
      .by = c(spatial_unit, age, sex, nat),
      emi_rate = mean(emi_rate, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      !!paste0(emi_var, "_method") := paste0("mean last ", year_n, " years")
    ) |>
    dplyr::mutate(
      spatial_unit = as.character(spatial_unit),
      age = as.character(age))

  # Ensure there are no missing values
  assertthat::assert_that(
    all(colSums(is.na(df_result)) == 0),
    msg = paste(
      "'df_result' contains missing values in columns:",
      paste(names(which(colSums(is.na(df_result)) > 0)), collapse = ", "))
  )

  return(df_result)
}
