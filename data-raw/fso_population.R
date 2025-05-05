#' Population data from the Federal Statistical Office
#'
#' @description Population from the canton of Aargau.
#'
#' Update description in folder propop/R/


## Default way to create data, contingent on STATPOP availability
# fso_population <- get_population(
#   year = 2024,
#   spatial_units = c("- Aargau")
# )

# Provisional approach until STATPOP is available
# Get mean of 0-year olds at the end of the year to impute missing starting value
mean_0_year_olds <- fso_parameters |>
  dplyr::filter(year < 2030 & age == 0) |>
  summarise(n = round(mean(fso_projection_n), digits = 0),
            .by = c(nat, sex)) |>
  dplyr::mutate(year = 2024)

fso_population <- fso_parameters |>
  dplyr::filter(year == 2025 & scen == "reference") |>
  dplyr:: select(
    year, spatial_unit, nat, sex, age,
    n = start_n) |>
  dplyr::mutate(year = 2024) |>
  # Add mean of subsequent years as plausible start_n for 0-year olds
  left_join(mean_0_year_olds |>
              select(nat, sex, year, n_new = n),
            by = c("nat", "sex", "year")) |>
  mutate(
    n = if_else(age == 0, coalesce(n_new, n), n)) |>
  select(-n_new)

usethis::use_data(fso_population, overwrite = TRUE)
