# Specific argument-data configurations for projections with 1 nation
# Separated from propop::propop tests for brevity & quicker error identification
# Tests with two nationalities are part of test-propop

options(cli.default_handler = function(...) { })

test_that("propop single nation projection with 1 region", {
  skip_on_ci()

  # /////////////////////////////////////////////////////
  # 1 region & 1 nationality ----------------------------
  # /////////////////////////////////////////////////////

  ## Prepare FSO parameters ----

  ### Population = CH ----
  # # Run next lines to generate the subsequent data frame
  param_sub_F_binat_F_CH <- fso_parameters |>
  dplyr::filter(year < 2026) |>
  dplyr::filter(scen == "reference") |>
    # drop non-Swiss people
    dplyr::filter(nat != "int") |>
    # remove `nat`, `acq` and `int_mothers` from `parameters`
    select(-c(nat, acq, int_mothers))

  ### Population = INT ----
  # # Run next lines to generate the subsequent data frame
  param_sub_F_binat_F_INT <-  fso_parameters |>
  dplyr::filter(year < 2026) |>
  dplyr::filter(scen == "reference") |>
    # drop Swiss people
    dplyr::filter(nat == "int") |>
    # remove `nat`, `acq` and `int_mothers` from `parameters`
    select(-c(nat, acq, int_mothers))

  ## Prepare starting population ----

  ### Population = CH ----
  # # Run next lines to generate the subsequent data frame
  # # Create starting population
  pop_sub_F_binat_F_CH <-  fso_population |>
    # drop non-Swiss people
    dplyr::filter(nat != "int") |>
    # remove `nat`  from `population`
    select(-nat)

  ### Population = INT ----
  # # Run next lines to generate the subsequent data frame
  # # Create starting population
  pop_sub_F_binat_F_INT <- fso_population |>
    # drop Swiss people
    dplyr::filter(nat == "int") |>
    # remove `nat`  from `population`
    select(-nat)


  ## Projections & tests ----

  ### Swiss nationals ----
  sub_F_binat_F_CH <- propop(
    parameters = param_sub_F_binat_F_CH |>
      dplyr::filter(scen == "reference"),
    year_first = 2025,
    year_last = 2025,
    population = pop_sub_F_binat_F_CH,
    subregional = FALSE,
    binational = FALSE
  )

  #### Tests ----

  ### Check if components add up 1
  balance_check_1 <- check_balance(sub_F_binat_F_CH)

  expect_equal(balance_check_1$nonzeros, 0,
    info =
      "The components don't add up in at least one row"
  )
  expect_equal(balance_check_1$missings, 0,
    info =
      "There are missings in at least one row"
  )

  ### Should match snapshot data
  expect_snapshot(print(print(as.data.frame(sub_F_binat_F_CH))))

  ### Should fail because function expects column `nat`
  expect_error(
    propop(
      parameters = param_sub_F_binat_F_CH |>
        dplyr::filter(scen == "reference"),
      year_first = 2025,
      year_last = 2025,
      population = pop_sub_F_binat_F_CH,
      subregional = FALSE,
      binational = TRUE
    )
  )

  ### Non-Swiss nationals ----
  sub_F_binat_F_INT <- propop(
    parameters = param_sub_F_binat_F_INT |>
      dplyr::filter(scen == "reference"),
    year_first = 2025,
    year_last = 2025,
    population = pop_sub_F_binat_F_INT,
    subregional = FALSE,
    binational = FALSE
  )

  #### Tests ----

  ### Check if components add up 2
  balance_check_2 <- check_balance(sub_F_binat_F_INT)

  expect_equal(balance_check_2$nonzeros, 0,
    info =
      "The components don't add up in at least one row"
  )
  expect_equal(balance_check_2$missings, 0,
    info =
      "There are missings in at least one row"
  )

  ### Should match snapshot data
  expect_snapshot(print(as.data.frame(sub_F_binat_F_INT)))

  ### Should fail because function expects column `nat`
  expect_error(
    propop(
      parameters = param_sub_F_binat_F_INT |>
        dplyr::filter(scen == "reference"),
      year_first = 2025,
      year_last = 2025,
      population = pop_sub_F_binat_F_INT,
      subregional = FALSE,
      binational = TRUE
    )
  )
})
