#' Calculate shares for distributing migration parameters to spatial units
#'
#' @param data_migration data frame, mutation data e.g. for international immigration or
#' intercantonal immigration aggregated across demographic groups.
#' @param column_migration string, name of the data column which contains the
#' data for historical migration occurrences.
#'
#' @export
#'
#' @autoglobal
calculate_shares <- function(data_migration, column_migration) {

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


  # Prepare age groups and summarize observations ....
  df1 <- data_migration |>
    # create coarser age groups (steps of 10 years)
    mutate(
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
      # total number of people in 10-year age groups
      sum_mig_10 = sum(!!sym(column_migration)),
      # proportional total
      prop_mig_10 = sum_mig_10 / 10,
      .by = any_of(c("nat", "sex", "spatial_unit", "age_group_10"))
    )


  # Create dummy variables to indicate which age group should be used ----
  df2 <- df1 |>
    mutate(
      use_group =
      # if there are observations in the single age group, use those
        ifelse(!!sym(column_migration) != 0, "single_age_group",
          # if there are no observations in the single age group, use the
          # proportional total of 10-year age groups
          ifelse(!!sym(column_migration) == 0 & prop_mig_10 > 0, "age_group_10",
            # if neither, single or 10-year age group have more than zero
            # observations, still use the 10-year age group (this happens rather
            # rarely; the share will be zero in those cases)
            "age_group_10"
          )
        )
    )

  # Check data for NAs in group choices ----
  assertthat::assert_that(isFALSE(NA %in% unique(df2$use_group)),
    msg = paste0("One or more observations could not be assigned to either",
      "'single_age_group' or 'age_group_10'.")
  )

# Calculate estimates for the number of migrations -----
  df3 <- df2 |>
    mutate(
      # get estimates depending on group choices
      estimate_mig = case_when(
        use_group == "single_age_group" ~ !!sym(column_migration),
        use_group == "age_group_10" ~ prop_mig_10,
        TRUE ~ NA
      )
    ) |>
    # get the total by demographic group
    mutate(estimate_mig_sum = sum(estimate_mig), .by = c(nat, sex, age)) |>
    # calculate shares
    mutate(share_migration = estimate_mig / estimate_mig_sum)

  # Check data for NAs in shares ----
  assertthat::assert_that(isFALSE(NA %in% unique(df3$share_migration)),
    msg = paste0("Shares for one or more observations are NA.")
  )

  return(df3)
}
