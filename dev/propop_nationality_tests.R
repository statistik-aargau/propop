# Tests for running propop() with two groups of nationalities vs without
# discriminating between nationalities
# Norah Efosa (Statistik Aargau), 2024

# Install propop from the development branch
# devtools::install_github("statistik-aargau/propop", ref = "f-single-nationality")

# Load the package
# devtools::load_all()

# Run propop()

# Case 1: Two groups in column `nat` (original) -------------------------------
test_original <- propop(
  parameters = fso_parameters,
  year_first = 2019,
  year_last = 2022,
  population = fso_population,
  subregional = FALSE,
  binational = TRUE
)


# Case 2: No distinction between nationalities (new feature) ------------------
test_binational_FALSE <- propop(
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


# Binational = TRUE for nationalities where "int" is zero in the data ---------
test_binational_TRUE <- propop(
  # set values for nationality "int" = zero
  parameters = fso_parameters |>
    mutate(births_int_ch = 0) |>
    mutate(across(birth_rate:mig_ch, ~if_else(nat == "int", 0, .x))),
  year_first = 2019,
  year_last = 2022,
  # set values for nationality "int" = zero
  population = fso_population |>
    mutate(across(n, ~if_else(nat == "int", 0, .x))),
  subregional = FALSE,
  binational = TRUE
)


# Compare results for both approaches for a single nationality
waldo::compare(
  test_binational_FALSE,
  test_binational_TRUE |> filter(nat == "ch") |> select(-nat)
)


# Tests for subregional -------------------------------------------------------
## Get data with subregional spatial units ------------------------------------
# Sourced from propop vignette:
# https://statistik-aargau.github.io/propop/articles/run_projections.html#projection-for-multiple-subregions

# fso parameters for fictitious subregions
fso_parameters_sub <- fso_parameters |>
  # duplicating rows 5 times
  tidyr::uncount(5) |>
  # create 5 subregions
  dplyr::mutate(spatial_unit = rep(1:5, times = nrow(fso_parameters))) |>
  # divide the size of parameters with numbers by the number of regions (= 5);
  # otherwise the multiplication of lines will inflate the population size.
  dplyr::mutate(spatial_unit = as.character(spatial_unit))

# fso population for fictitious subregions
fso_population_sub <- fso_population |>
  dplyr::rename(n_tot = n) |>
  # duplicating rows 5 times
  tidyr::uncount(5) |>
  # create 5 subregions
  dplyr::mutate(spatial_unit = rep(1:5, times = nrow(fso_population))) |>
  dplyr::mutate(
    # Create fictitious n for each subregion
    n = dplyr::case_match(
      spatial_unit,
      1 ~ round(n_tot * 0.3),
      2 ~ round(n_tot * 0.25),
      3 ~ round(n_tot * 0.2),
      4 ~ round(n_tot * 0.15),
      5 ~ round(n_tot * 0.1),
      .default = NA
    ),
    .keep = "all"
  ) |>
  dplyr::mutate(spatial_unit = as.character(spatial_unit)) |>
  dplyr::select(-n_tot)


## Tests ----------------------------------------------------------------------
# Original: Two nationalities + mig_sub adding exactly up to 1 across spatial units
# subregional = TRUE
# binational = TRUE
# mig_sub adds up to 1 exactly
test_mig_sub_original <- propop(
  parameters = fso_parameters_sub |>
    # add subregional migration (adds up to 1 exactly)
    mutate(mig_sub = 0.2),
  year_first = 2019,
  year_last = 2022,
  population = fso_population_sub,
  subregional = TRUE,
  binational = TRUE
)


# Case 2: Only one nationality provided
# subregional = TRUE
# binational = FALSE
# mig_sub adds up to 1 exactly
test_mig_sub_exact <- propop(
  # remove `nat`, `acq` and `births_int_ch` from `parameters`
  parameters = fso_parameters_sub |>
    # add subregional migration (adds up to 1 exactly)
    mutate(mig_sub = 0.2) |>
    dplyr::filter(nat != "int") |>
    select(-c(nat, acq, births_int_ch)),
  year_first = 2019,
  year_last = 2022,
  # remove `nat`  from `population`
  population = fso_population_sub |>
    dplyr::filter(nat != "int") |>
    select(-nat),
  subregional = TRUE,
  binational = FALSE
)


# Two for nationalities but values for "int" are zero in the data
# subregional = TRUE
# binational = TRUE
# # mig_sub adds up to 1 exactly
# test_mig_sub_binational_TRUE <- propop(
#   # set values for nationality "int" = zero
#   parameters = fso_parameters_sub |>
#     # add subregional migration (adds up to 1 exactly)
#     mutate(mig_sub = 0.2, .before = "spatial_unit") |>
#   # set values for nationality "int" = zero
#     mutate(births_int_ch = 0) |>
#     mutate(across(birth_rate:mig_sub, ~if_else(nat == "int", 0, .x))),
#   year_first = 2019,
#   year_last = 2022,
#   # set values for nationality "int" = zero
#   population = fso_population_sub |>
#     mutate(across(n, ~if_else(nat == "int", 0, .x))),
#   subregional = TRUE,
#   binational = TRUE
# )
#
# # Compare the output
# waldo::compare(
#   test_mig_sub_exact,
#   test_mig_sub_binational_TRUE |> filter(nat != "int") |> select(-nat)
# )


# Case 2: Only one nationality provided
# subregional = TRUE
# binational = FALSE
# mig_sub diverts slighty from 1
test_mig_sub_modified <- propop(
  # remove `nat`, `acq` and `births_int_ch` from `parameters`
  parameters = fso_parameters_sub |>
    # mig_sub diverts slighty from 1 for one spatial unit
    mutate(mig_sub = ifelse(spatial_unit == "1", 0.2 - 0.000000000001522, 0.2)) |>
    dplyr::filter(nat != "int") |>
    select(-c(nat, acq, births_int_ch)),
  year_first = 2019,
  year_last = 2022,
  # remove `nat`  from `population`
  population = fso_population_sub |>
    dplyr::filter(nat != "int") |>
    select(-nat),
  subregional = TRUE,
  binational = FALSE
)

# Compare the output
waldo::compare(
  test_mig_sub_exact,
  test_mig_sub_modified
)



