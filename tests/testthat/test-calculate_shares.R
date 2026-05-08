test_that("calculate_shares snapshot1 - mean, age_group=5, binational, two_sex", {

  result <- calculate_shares(
    past_migration = ag_migration_subregional,
    imm_n         = "imm_n",
    year_range    = c(2024, 2025),
    age_group     = 5,
    binational    = TRUE,
    two_sex       = TRUE
  )

  expect_snapshot(print(tail(result, 100)))
})

test_that("calculate_shares snapshot2 - median, age_group=7, 2023 & 2025, one sex", {

  result2 <- calculate_shares(
    past_migration = ag_migration_subregional |>
      dplyr::select(-sex),
    imm_n         = "imm_n",
    year_range    = c(2022, 2025),
    age_group     = 7,
    binational    = TRUE,
    two_sex       = FALSE
  )

  expect_snapshot(print(tail(result2, 100)))
})


test_that("calculate_shares snapshots differ - median, age_group=7, 2023 & 2025, one sex", {

  result2 <- calculate_shares(
    past_migration = ag_migration_subregional |>
      dplyr::select(-sex),
    imm_n         = "imm_n",
    year_range    = c(2022, 2025),
    age_group     = 7,
    binational    = TRUE,
    two_sex       = FALSE
  )

  result3 <- calculate_shares(
    past_migration = ag_migration_subregional,
    imm_n         = "imm_n",
    year_range    = c(2022, 2025),
    age_group     = 7,
    binational    = TRUE,
    two_sex       = TRUE
  )

    expect_failure(expect_equal(
      result2, result3)
      )

})
