#' Calculate shares for distributing migration parameters among spatial units
#'
#' @param data_migration data frame, historical records (e.g., for immigration
#' from other cantons or countries) aggregated across demographic groups.
#' @param column_migration character, name of the column which contains the
#' data for historical migration occurrences.
#' @param age_group character, optional argument: age group used for
#' calculating shares. If the argument is not specified, the default selects
#' shares based on xyz. To use the same age clustering for all demographic units,
#' the argument can be specified with "age_group_5" or "age_group_10".
#' @returns
#' Returns the input data frame with the following new columns:
#'
#'* `age_group_5`: character, name of the 5-year age group to which the 1-year
#'  age class is assigned to.
#'* `sum_5`: numeric, total number of people in the 5-year age group.
#'* `prop_5`: numeric, proportion of the the 5-year age group total that
#'  is allocated to each 1-year age group.
#'* `age_group_10`: character, name of the 10-year age group to which the 1-year
#'  age class is assigned to.
#'* `sum_10`: numeric, total number of people in the 10-year age group.
#'* `prop_10`: numeric, proportion of the the 10-year age group total that
#'  is allocated to each 1-year age group.
#'* `use_age_group`: character, preference for 1-year, 5-year or 10-year age
#'  group. Defaults to `age_group_1` if at least one observation was recorded.
#'* `use_share`: numeric, share to be used according to `use_age_group`.
#'* `use_share_sum`: numeric, total per demographic group and across all
#'  spatial units.
#'* `share`: numeric, the spatial unit's share relative to the total (across all
#'  spatial units) of people within the same demographic group.
#'
#' @export
#'
#' @autoglobal
calculate_shares <- function(
    data_migration,
    column_migration,
    age_group = "default"
  ) {
  # Test input ----
  ## Presence of mandatory columns ----
  assertthat::assert_that("spatial_unit" %in% names(data_migration),
    msg = "column `spatial_unit` is missing in data_migration."
  )
  assertthat::assert_that("sex" %in% names(data_migration),
    msg = "column `sex` is missing in data_migration."
  )
  assertthat::assert_that("age" %in% names(data_migration),
    msg = "column `age` is missing in data_migration."
  )
  assertthat::assert_that(
    column_migration %in% names(data_migration),
    msg = paste0("column `", column_migration, "` is missing in data_migration.")
  )
  ## Data types and content ----
  assertthat::assert_that(
    is.numeric(data_migration[[column_migration]]),
    msg = paste0("Values in column `", column_migration, "` must be numeric.")
  )
  assertthat::assert_that(length(unique(data_migration$spatial_unit)) > 1,
    msg = "Only one level for spatial_unit is present in data_migration."
  )


  # Prepare age groups and summarize observations
  df1 <- data_migration |>
    # create coarser age groups
    ## steps of 5 years
    mutate(
      age_group_5 = cut(
        as.numeric(age),
        breaks = c(-Inf, seq(4, 99, by = 5), Inf),
        labels = c(paste0(seq(0, 95, by = 5), "_", seq(4, 99, by = 5)), "_100"),
        include.lowest = TRUE
      ),
      # expand the group of the people from 95 years on to include 100 year olds
      age_group_5 = case_when(
        age_group_5 == "95_99" ~ "95_100",
        age_group_5 == "_100" ~ "95_100",
        TRUE ~ age_group_5
      ),
      ## steps of 10 years
      age_group_10 = cut(
        as.numeric(age),
        breaks = c(-Inf, seq(10, 100, by = 10), Inf),
        labels = c(paste0(seq(0, 90, by = 10), "_", seq(9, 99, by = 10)), "_100"),
        right = FALSE,
        include.lowest = TRUE
      ),
      # expand the group of the people from 90 years on to include 100 year olds
      age_group_10 = case_when(
        age_group_10 == "90_99" ~ "90_100",
        age_group_10 == "_100" ~ "90_100",
        TRUE ~ age_group_10
      )
    ) |>
    # get the total and proportional total by coarser age groups
    mutate(
      # total number of people in 5-year age groups
      sum_5 = sum(!!sym(column_migration)),
      # proportional total
      prop_5 = sum_5 / 5,
      .by = any_of(c("nat", "sex", "spatial_unit", "age_group_5"))
    ) |>
    mutate(
      # total number of people in 10-year age groups
      sum_10 = sum(!!sym(column_migration)),
      # proportional total
      prop_10 = sum_10 / 10,
      .by = any_of(c("nat", "sex", "spatial_unit", "age_group_10"))
    )


  # Create dummy variables to indicate which age group should be used ----
  df2 <- df1 |>
    # firstly, grouped by 5-year age groups
    mutate(
      use_age_group = case_when(
        # if all observations in 5-year age groups are zero, use 10-year age groups
        sum(sum_5) == 0 ~ "age_group_10",
        # if there are no observations in the single age group, use the
        # proportional total of 5-year age groups
        0 %in% !!sym(column_migration) ~ "age_group_5",
        # else use single year groups
        TRUE ~ "age_group_1"
      ),
      .by = any_of(c("nat", "sex", "spatial_unit", "age_group_5"))
    ) |>
    # secondly, grouped by 10-year age groups
    mutate(
      use_age_group = case_when(
        # if the dummy variable includes "age_group_10" for any age group, use
        # the 10-year groups for all ages within those 10-year age groups.
        "age_group_10" %in% use_age_group ~ "age_group_10",
        # if neither, single, 5-year or 10-year age groups have more than zero
        # observations, still use the 10-year age group (this happens rather
        # rarely; the share will be zero in those cases)
        sum(sum_10) == 0 ~ "age_group_10",
        TRUE ~ use_age_group
      ),
      .by = any_of(c("nat", "sex", "spatial_unit", "age_group_10"))
    )

  # Check data for NAs in group choices ----
  assertthat::assert_that(isFALSE(NA %in% unique(df2$use_age_group)),
    msg = paste0(
      "One or more observations could not be assigned to either",
      "'age_group_1', 'age_group_5' or 'age_group_10'."
    )
  )

  # Calculate estimates for the number of migrations -----
  if (age_group == "default") {
  df3 <- df2 |>
    mutate(
      # get estimates depending on group choices
      n = case_when(
        use_age_group == "age_group_1" ~ !!sym(column_migration),
        use_age_group == "age_group_5" ~ prop_5,
        use_age_group == "age_group_10" ~ prop_10,
        TRUE ~ NA
      )
    )
  } else if (age_group == "age_group_5") {
    df3 <- df2 |>
      mutate(n = prop_5, use_age_group = "age_group_5")
  } else if (age_group == "age_group_10") {
    df3 <- df2 |>
      mutate(n = prop_10, use_age_group = "age_group_10")
  }

  # Calculate shares
  df4 <- df3 |>
    # get the total by demographic group
    mutate(n_sum = sum(n), .by = c(nat, sex, age)) |>
    # calculate shares
    mutate(share = n / n_sum)

  # Check data for NAs in shares ----
  assertthat::assert_that(isFALSE(NA %in% unique(df4$share)),
    msg = paste0("Shares for one or more observations are NA.")
  )

  return(df4)
}
