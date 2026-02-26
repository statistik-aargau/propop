# Compare projection result from propop::propop() with FSO projections

test_that("Projection output from propop matches FSO projection", {
  # don't run on gitlab ci
  skip_on_ci()

  skip("Disabled because the FSO starting population is not publicly available")

  options(cli.default_handler = function(...) { })

  # FSO projection
  data_benchmark <- fso_parameters |>
    dplyr::filter(scen == "reference") |>
    dplyr::select(year, spatial_unit, nat, age, sex, fso_projection_n)

  # propop projection
  data_projected <- propop(
    parameters = fso_parameters |>
      dplyr::filter(scen == "reference"),
    year_first = 2024,
    year_last = 2055,
    age_groups = 101,
    fert_first = 16,
    fert_last = 50,
    share_born_female = 100 / 205,
    population = startpop,
    subregional = FALSE,
    binational = TRUE
  ) |>
    dplyr::select(year, spatial_unit, nat, age, sex, n_dec)


  # Combine and pre-process the data
  combined <- prepare_evaluation(
    # only keep years from projected period
    data_benchmark = data_benchmark |> dplyr::filter(year > min(year)),
    n_benchmark = "fso_projection_n",
    data_projected = data_projected |> dplyr::filter(year > min(year)),
    n_projected = "n_dec"
  )

  evaluation <- compute_measures(combined)

  # Fail if absolute percentage error is larger than threshold
  expect_lte(
    max(abs(evaluation$error), na.rm = TRUE), 1,
    paste0(
      "The largest difference in number of ",
      "people is ",
      round(max(evaluation$error, na.rm = TRUE), digits = 0),
      ". The difference between the expected value and the observed value"
    )
  )
})
