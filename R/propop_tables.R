# Run projections with tables instead of matrices (work-in-progress)
# Norah Efosa (Statistik Aargau), 2024

propop_tables <- function(
    parameters,
    population,
    year_first,
    year_last,
    age_groups = 101,
    fert_first = 16,
    fert_last = 50,
    share_born_female = 100 / 205,
    subregional = FALSE,
    binational = TRUE,
    spatial_unit = "spatial_unit") {
  # browser()

  # Rename n in the initial population
  init_population <- population |>
    rename(n_dec = n)

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
