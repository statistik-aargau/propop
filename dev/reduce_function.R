# Run projections with tables instead of matrices (work-in-progress)
# Norah Efosa (Statistik Aargau), 2024

# Install propop from the development branch
# devtools::install_github("statistik-aargau/propop", ref = "f-proj-tables")

# Load the package
devtools::load_all()

# Note: new dependency: `purrr`

# Run projections

system.time({
  library(purrr)

  # Prepare initial population
  init_population <- fso_population |>
    mutate(year = as.numeric(year)) |>
    # mutate(year = as.numeric(year)) |>
    # n_dec ('n_december') refers to the population at the end of the year from
    # the initial population
    rename(n_dec = n)

  # Prepare parameters
  parameters_test <- fso_parameters |>
    mutate(year = as.numeric(year)) |>
    select(
      # select and rename variables from parameters
      year, nat, sex, age, scen, spatial_unit, birthrate, int_mothers,
      mor_rate = mor, emi_int_rate = emi_int, emi_nat_rate = emi_nat,
      acq_rate = acq, imm_int_n, imm_nat_n
    )

  # Split parameters into a list to iterate across
  list_parameters <-
    # split parameters by year
    split(parameters_test, parameters_test$year) |>
    set_names(~ paste0("parameters_", .))

# Run projection with tables
  df_result <-
    # use purrr::reduce() to iterate across years (in parameters)
    purrr::reduce(
      list_parameters,
      ~ apply_parameters(
        population = ..1,
        parameters = ..2
      ),
      # initial population
      .init = init_population
    ) |>
    # clean the data
    filter(year != unique(init_population$year))
})

# Compare with propop() (using matrices)
## Run propop
system.time({
  propop_original <-
    propop::propop(
      parameters = propop::fso_parameters,
      year_first = 2019,
      year_last = 2030,
      population = propop::fso_population,
      subregional = FALSE,
      binational = TRUE
    )

})

# Compare results
comp_models = propop_original |>
  left_join(
    df_result |>
      rename_with(~ paste("new_", .x , sep = ""), .cols = n_jan:n_dec)
  ) |>
  mutate(
    diff_n_jan = round(n - new_n_jan, 3),
    diff_n_dec = round(balance_n - new_n_dec, 3)
  )


