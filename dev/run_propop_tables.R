# Running some tests for calculating the projections with tables (dplyr)
# February, 2025

# Install propop from the development branch
# devtools::install_github("statistik-aargau/propop", ref = "f-proj-tables2025-2055")

# Load the package
devtools::load_all()

# Get initial population ----
df_population <- get_population(
  number_fso = "px-x-0102010000_101",
  year = 2018,
  year_last = 2018,
  spatial_units = "- Aargau"
)


# Get parameters ----
df_parameters <- get_parameters(
  year_first = 2018,
  year_last = 2050,
  spatial_units = "Aargau"
) |>
  # one single scenario can be modeled at a time
  dplyr::filter(scen == "reference")


# Run propop with tables (new feature) ----
system.time({
  result_tables <- propop_tables(
    parameters = df_parameters,
    year_first = 2019,
    year_last = 2050,
    population = df_population,
    subregional = FALSE,
    binational = TRUE
  )
})


# Run propop with matrices (original) ----
system.time({
  result_matrices <-
    propop::propop(
      parameters = df_parameters,
      year_first = 2019,
      year_last = 2050,
      population = df_population,
      subregional = FALSE,
      binational = TRUE
    )
})


# Compare results ----
compare_models <- result_matrices |>
  arrange(year, nat, sex, age) |>
  left_join(
    result_tables |>
      # clean the data
      select(-scen) |>
      rename_with(~ paste("new_", .x, sep = ""), .cols = n_jan:n_dec),
    by = c("year", "spatial_unit", "nat", "sex", "age"),
    relationship = "one-to-one"
  ) |>
  # rowwise() |>
  mutate(
    abs_error = n_dec - new_n_dec, 3,
    abs_perc_error = abs((n_dec - new_n_dec) / n_dec) * 100
  )


# Discrepancies of new projected values (with tables) with original projected
# values (matrices)
## Per age and across time
### Absolute number of people
compare_models |>
  ggplot2::ggplot(
    aes(x = age, y = abs_error, color = as.factor(year))
  ) +
  ggplot2::geom_point() +
  scale_y_continuous(limits = c(-4, 4)) +
  theme_light() +
  theme(legend.position = "right")


### Absolute percentage error
compare_models |>
  ggplot2::ggplot(
    aes(x = age, y = abs_perc_error, color = as.factor(year))
  ) +
  ggplot2::geom_point() +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_light() +
  theme(legend.position = "right")


## Per nationality and sex
### Absolute number of people
compare_models |>
  summarize(abs_error = mean(abs_error), .by = c(year, sex, nat)) |>
  mutate(nat_sex = paste0(nat, "_", sex)) |>
  ggplot2::ggplot(
    aes(x = year, y = abs_error, color = as.factor(nat_sex), shape = sex)
  ) +
  ggplot2::geom_point(size = 2) +
  scale_y_continuous(limits = c(-2, 2)) +
  theme_light() +
  theme(legend.position = "right")


### Mean absolute percentage error
compare_models |>
  summarize(abs_perc_error = mean(abs_perc_error), .by = c(year, sex, nat)) |>
  mutate(nat_sex = paste0(nat, "_", sex)) |>
  ggplot2::ggplot(
    aes(x = year, y = abs_perc_error, color = as.factor(nat_sex), shape = sex)
  ) +
  ggplot2::geom_point() +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  theme_light() +
  theme(legend.position = "right")


