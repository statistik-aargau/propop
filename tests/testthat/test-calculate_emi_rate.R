

test_that("calculate_emi_rate snapshot - mean, age_group=5, binational, two_sex", {

  result <- calculate_emi_rate(
    past_migration = ag_migration_subregional,
    n_jan         = n_jan,
    births        = births,
    emi_n         = emi_n,
    spatial_unit  = spatial_unit,
    method        = "mean",
    year_range    = c(2024, 2025),
    age_group     = 5,
    binational    = TRUE,
    two_sex       = TRUE
  )

  expect_snapshot(print(tail(result, 100)))
})

test_that("calculate_emi_rate snapshot - median, age_group=7, 2023 & 2025, one sex", {

  result2 <- calculate_emi_rate(
    past_migration = ag_migration_subregional |>
      dplyr::select(-sex),
    n_jan         = n_jan,
    births        = births,
    emi_n         = emi_n,
    spatial_unit  = spatial_unit,
    method        = "median",
    year_range    = c(2022, 2025),
    age_group     = 7,
    binational    = TRUE,
    two_sex       = FALSE
  )

  expect_snapshot(print(tail(result2, 100)))
})


test_that("calculate_emi_rate snapshots should differ - median vs. mean", {

  result2 <- calculate_emi_rate(
    past_migration = ag_migration_subregional |>
      dplyr::select(-sex),
    n_jan         = n_jan,
    births        = births,
    emi_n         = emi_n,
    spatial_unit  = spatial_unit,
    method        = "median",
    year_range    = c(2022, 2025),
    age_group     = 7,
    binational    = TRUE,
    two_sex       = FALSE
  )

  result3 <- calculate_emi_rate(
    past_migration = ag_migration_subregional |>
      dplyr::select(-sex),
    n_jan         = n_jan,
    births        = births,
    emi_n         = emi_n,
    spatial_unit  = spatial_unit,
    method        = "mean",
    year_range    = c(2022, 2025),
    age_group     = 7,
    binational    = TRUE,
    two_sex       = FALSE
  )

  expect_failure(expect_equal(
    result2, result3)
  )

})
