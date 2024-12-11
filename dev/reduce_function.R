# Run projections with tables instead of matrices (work-in-progress)
# Norah Efosa (Statistik Aargau), 2024

# Install propop from the development branch
# devtools::install_github("statistik-aargau/propop", ref = "f-proj-tables")

# Load the package
# devtools::load_all()

# Note: new dependency: `purrr`

# Run projections

{
  library(purrr)

  # Prepare initial population
  init_population <- fso_population |>
    mutate(year = as.numeric(year)) |>
    # n_dec ('n_december') refers to the population at the end of the year from
    # the initial population
    rename(n_dec = n)

  # Prepare parameters
  parameters <- fso_parameters |>
    mutate(year = as.numeric(year)) |>
    select(
      # select and rename variables from parameters
      year, nat, sex, age, scen, spatial_unit, mor_rate = mor,
      emi_int_rate = emi_int, emi_nat_rate = emi_nat, acq_rate = acq,
      imm_int_n, imm_nat_n
    )

  # Split parameters into a list to iterate across
  list_parameters <-
    # split parameters by year
    split(parameters, parameters$year) |>
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
}

