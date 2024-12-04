
library(purrr)

init_population <- fso_population |>
  mutate(year = as.numeric(year)) |>
  rename(balance_n = n)

parameters <- fso_parameters |>
  mutate(year = as.numeric(year))

list_parameters <-
  # split parameters by year
  split(parameters, parameters$year) |>
  set_names(~ paste0("parameters_", .))


df_result <-
  purrr::reduce(
    list_parameters,
    ~ apply_parameters(
        population = ..1,
        parameters = ..2
    ),
    .init = init_population
  )
