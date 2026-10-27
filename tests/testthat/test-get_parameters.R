test_that("get_parameters snapshots - 1 region 2026-2050", {
  test_that("get_parameters() rejects invalid input", {
    expect_error(get_parameters(year_first = "abc"))
    expect_error(get_parameters(year_first = 2030, year_last = 2025))
  })

  # don't run on gitlab ci
  skip_on_ci()

  options(cli.default_handler = function(...) {})

  # only run if: Sys.setenv(RUN_EXPENSIVE_TESTS = TRUE)
  skip_if_not(as.logical(Sys.getenv("RUN_EXPENSIVE_TESTS")))

  one_snapshot <- get_parameters(
    year_first = 2025,
    year_last = 2028,
    spatial_units = "Aargau"
  )
  expect_s3_class(one_snapshot, "data.frame")
  expect_named(
    one_snapshot,
    c(
      "nat", "sex", "age", "start_n", "year", "scen", "spatial_unit",
      "birthrate", "int_mothers", "mor", "emi_int", "emi_nat", "acq",
      "imm_int_n", "imm_nat_n", "emi_nat_n", "mig_nat_n", "fso_projection_n"
    ),
    ignore.order = TRUE
  )

  expect_snapshot(dput(one_snapshot))
})

test_that("get_parameters snapshots - 2 regions 2027-2028", {
  # don't run on gitlab ci
  skip_on_ci()

  options(cli.default_handler = function(...) {})

  # only run if: Sys.setenv(RUN_EXPENSIVE_TESTS = TRUE)
  skip_if_not(as.logical(Sys.getenv("RUN_EXPENSIVE_TESTS")))

  two_snapshot <- get_parameters(
    year_first = 2038,
    year_last = 2039,
    spatial_units = c("Aargau", "Zug")
  )
  expect_snapshot(dput(two_snapshot))
})
