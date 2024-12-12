options(cli.default_handler = function(...) { })

test_that("get population snapshots - 1 region 1 year", {

  # don't run on gitlab ci
  skip_on_ci()

  # only run if: Sys.setenv(RUN_EXPENSIVE_TESTS = TRUE)
  skip_if_not(as.logical(Sys.getenv("RUN_EXPENSIVE_TESTS")))
  get_population_snapshot <- get_population(
    number_fso = "px-x-0102010000_101",
    year_first = 2022,
    year_last = 2022,
    spatial_units = "......4232 Geltwil"
  )

  expect_snapshot(constructive::construct(get_population_snapshot))
})

test_that("get population snapshots - 2 regions & 2 years", {

  # don't run on gitlab ci
  skip_on_ci()

  # only run if: Sys.setenv(RUN_EXPENSIVE_TESTS = TRUE)
  skip_if_not(as.logical(Sys.getenv("RUN_EXPENSIVE_TESTS")))
  get_population_snapshot2 <- get_population(
    number_fso = "px-x-0102010000_101",
    year_first = 2022,
    year_last = 2022,
    spatial_units = c("......4232 Geltwil", "- Aargau")
  )

  expect_snapshot(constructive::construct(get_population_snapshot2))
})

test_that("error when requesting population for current year", {
  current_year <- (as.numeric(format(Sys.Date(), "%Y")))
  expect_error(
    get_population(
      number_fso = "px-x-0102010000_101",
      year_first = current_year,
      year_last = current_year,
      spatial_units = "- Aargau"
    )
  )
})
