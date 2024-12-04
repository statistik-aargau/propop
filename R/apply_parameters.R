apply_parameters <- function(population, parameters) {


  pop_year <- unique(population$year)

  # after_firstiter <- setdiff(colnames(population), c(colnames(parameters), "n")

  if (isTRUE(length(pop_year) != 1)) {
  population_prev <- population |>
    filter(year == parameters$year - 1)
  } else {
    population_prev <- population
  }

  # if (isTRUE(after_firstiter)) {
    population_prev <- population_prev |>
      select(year, nat, sex, age, spatial_unit, start_n = balance_n) |>
      mutate(year = parameters$year)
  # }

    # browser()

  population_new <-
    parameters |>
    left_join(population_prev) |>
    mutate(
      balance_n = start_n * 2
    )

  population_out <- bind_rows(population, population_new)

  return(population_out)

}
