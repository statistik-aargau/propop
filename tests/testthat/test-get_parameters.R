test_that("get_parameters snapshots - 1 region 2026-2050", {

  # don't run on gitlab ci
  skip_on_ci()

  # only run if: Sys.setenv(RUN_EXPENSIVE_TESTS = TRUE)
  skip_if_not(as.logical(Sys.getenv("RUN_EXPENSIVE_TESTS")))

  one_snapshot <- get_parameters(
    year_first = 2026,
    year_last = 2050,
    spatial_units = c("Aargau")
  )
  expect_snapshot(constructive::construct(one_snapshot))
})

test_that("get_parameters snapshots - 2 regions 2027-2028", {

  # don't run on gitlab ci
  skip_on_ci()

  # only run if: Sys.setenv(RUN_EXPENSIVE_TESTS = TRUE)
  skip_if_not(as.logical(Sys.getenv("RUN_EXPENSIVE_TESTS")))

  two_snapshot <- get_parameters(
    year_first = 2027,
    year_last = 2028,
    spatial_units = c("Aargau", "Zug")
  )
  expect_snapshot(constructive::construct(two_snapshot))
})
