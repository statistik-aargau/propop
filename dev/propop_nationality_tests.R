# Tests for running propop() with two groups of nationalities vs without
# discriminating between nationalities
# Norah Efosa (Statistik Aargau)

# Install propop from the development branch
# devtools::install_github("statistik-aargau/propop", ref = "f-single-nationality")

# Load the package
devtools::load_all()

# Run propop()
# Case 1: Two groups in column `nat` (original)
test_original <- propop(
  parameters = fso_parameters,
  year_first = 2019,
  year_last = 2022,
  population = fso_population,
  subregional = FALSE,
  binational = TRUE
)


# Case 2: No distinction between nationalities (new feature)
test_adapted <- propop(
  # remove `nat`, `acq` and `births_int_ch` from `parameters`
  parameters = fso_parameters |>
    dplyr::filter(nat != "int") |>
    select(-c(nat, acq, births_int_ch)),
  year_first = 2019,
  year_last = 2022,
  # remove `nat`  from `population`
  population = fso_population |>
    dplyr::filter(nat != "int") |>
    select(-nat),
  subregional = FALSE,
  binational = FALSE
)


# Failing tests (yes, those should fail)
# * Column `nat` exists in `parameters` but has only one factor level ("ch")
test_failure_1 <- propop(
  parameters = fso_parameters |>
    dplyr::filter(nat != "int") |>
    select(-c(acq, births_int_ch)),
  year_first = 2019,
  year_last = 2022,
  population = fso_population |>
    dplyr::filter(nat != "int") |>
    select(-nat),
  subregional = FALSE,
  binational = FALSE
)


# * Column `nat does not exist in `parameters` but in `population`
test_failure_2 <- propop(
  parameters = fso_parameters |>
    dplyr::filter(nat != "int") |>
    select(-c(nat, acq, births_int_ch)),
  year_first = 2019,
  year_last = 2022,
  population = fso_population,
  subregional = FALSE,
  binational = FALSE
)

# * Column `nat does not exist in `population` but in `parameters`
test_adapted <- propop(
  parameters = fso_parameters,
  year_first = 2019,
  year_last = 2022,
  population = fso_population |>
    dplyr::filter(nat != "int") |>
    select(-nat),
  subregional = FALSE,
  binational = FALSE
)

