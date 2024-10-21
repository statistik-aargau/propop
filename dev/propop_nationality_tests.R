# Tests for running propop() with two groups of nationalities vs without
# discriminating between nationalities
# Norah Efosa (Statistik Aargau)

# Install propop from the development branch
# devtools::install_github("statistik-aargau/propop", ref = "f-single-nationality")

# Load the package
# devtools::load_all()

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


# Binational = TRUE but the data has two nationalities where "int" is zero
test_binational_TRUE_mod <- propop(
  # remove `nat`, `acq` and `births_int_ch` from `parameters`
  parameters = fso_parameters |>
    mutate(births_int_ch = 0) |>
    mutate(across(birth_rate:mig_ch, ~if_else(nat == "int", 0, .x))),
  year_first = 2019,
  year_last = 2022,
  # remove `nat`  from `population`
  population = fso_population |>
    mutate(across(n, ~if_else(nat == "int", 0, .x))),
  subregional = FALSE,
  binational = TRUE
)


summary(arsenal::comparedf(
  test_binational_TRUE_mod |> filter(nat == "ch") |> select(-nat),
  test_adapted
))

waldo::compare(
  test_binational_TRUE_mod |> filter(nat == "ch") |> select(-nat),
  test_adapted
)


