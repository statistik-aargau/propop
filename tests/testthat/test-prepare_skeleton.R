# Test dimensions and type of output
test_that("skeleton simple checks", {

  skip_on_ci()

  # create a skeleton for 1 scenario
  output_table <- prepare_skeleton(
    age_groups = 101,
    year_first = 2025,
    year_last = 2027,
    spatial_unit = "My region",
    scenarios = "reference"
  )

  # dimensions of output_table
  expect_equal(nrow(output_table), 1616)
  expect_false(any(is.na(output_table)))


  # create a skeleton for 2 scenarios
  output_table <- prepare_skeleton(
    age_groups = 101,
    year_first = 2025,
    year_last = 2027,
    spatial_unit = "My region",
    scenarios = c("low", "reference")
  )

  # dimensions of output_table
  expect_equal(nrow(output_table), 3232)
  expect_false(any(is.na(output_table)))

  })


test_that("skeleton snapshots", {

  skip_on_ci()

  # create a skeleton
  skeleton_snapshot <- prepare_skeleton(
    age_groups = 101,
    year_first = 2025,
    year_last = 2027,
    spatial_unit = "Canton",
    scenarios = "reference"
  )

  expect_snapshot(constructive::construct(skeleton_snapshot))
})
