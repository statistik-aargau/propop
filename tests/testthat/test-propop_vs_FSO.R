# Compare projection result from propop::propop() with FSO projections

test_that("Projection output from propop matches FSO projection", {

  # don't run on gitlab ci
  skip_on_ci()

  options(cli.default_handler = function(...) { })

  # FSO projection
  data_benchmark <- fso_parameters |>
    dplyr::filter(scen == "reference") |>
    dplyr::select(year, spatial_unit, nat, age, sex, fso_projection_n)

  # propop projection
  data_projected <- propop(
    parameters = fso_parameters,
    year_first = 2019,
    year_last = 2030,
    age_groups = 101,
    fert_first = 16,
    fert_last = 50,
    share_born_female = 100 / 205,
    # population records from 2018 as starting point
    population = fso_population |>
      dplyr::filter(year == 2018),
    subregional = FALSE,
    binational = TRUE
  ) |>
    dplyr::select(year, spatial_unit, nat, age, sex, n_dec)


# Combine and pre-process the data
combined <- prepare_evaluation(
  # only keep years from projected period
  data_benchmark = data_benchmark |> dplyr::filter(year > 2018),
  n_benchmark = "fso_projection_n",
  data_projected = data_projected |> dplyr::filter(year > 2018),
  n_projected = "n_dec"
)

evaluation <- compute_measures(combined)

# Fail if absolute percentage error is larger than threshold
expect_lte(max(evaluation$ape, na.rm = TRUE), 0.05,
           "The difference between the propop output and the fso projection
           exceeds the expected maximum of 0.05")

})
