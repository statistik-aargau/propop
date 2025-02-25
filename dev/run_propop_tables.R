# Running some tests for calculating the projections with tables (dplyr)
# February, 2025

# Install propop from the development branch
# devtools::install_github("statistik-aargau/propop", ref = "f-proj-tables2025-2055")

# Load the package
devtools::load_all()

library(dplyr)

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
    year_last = 2025,
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
      year_last = 2025,
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
    ggplot2::aes(x = age, y = abs_error, color = as.factor(year))
  ) +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(limits = c(-4, 4)) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "right")


### Absolute percentage error
compare_models |>
  ggplot2::ggplot(
    ggplot2::aes(x = age, y = abs_perc_error, color = as.factor(year))
  ) +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(limits = c(-1, 1)) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "right")


## Per nationality and sex
### Absolute number of people
compare_models |>
  summarize(abs_error = mean(abs_error), .by = c(year, sex, nat)) |>
  mutate(nat_sex = paste0(nat, "_", sex)) |>
  ggplot2::ggplot(
    ggplot2::aes(x = year, y = abs_error, color = as.factor(nat_sex), shape = sex)
  ) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_y_continuous(limits = c(-2, 2)) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "right")


### Mean absolute percentage error
compare_models |>
  summarize(abs_perc_error = mean(abs_perc_error), .by = c(year, sex, nat)) |>
  mutate(nat_sex = paste0(nat, "_", sex)) |>
  ggplot2::ggplot(
    ggplot2::aes(x = year, y = abs_perc_error, color = as.factor(nat_sex), shape = sex)
  ) +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(limits = c(-0.2, 0.2)) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "right")


# Subregional = TRUE ----

# FSO parameters for fictitious subregions
df_parameters_subregional <- df_parameters |>
  # duplicating rows 5 times
  tidyr::uncount(5) |>
  # create 5 subregions
  dplyr::mutate(
    spatial_unit = rep(1:5, times = nrow(df_parameters)),
    spatial_unit = as.character(spatial_unit),
    mig_sub = 1
  )


# Generate 5 random "cuts" to distribute the original population;
# use range 0.1-0.5 to avoid very small or very large regions
cut_1 <- {
  set.seed(1)
  round(runif(1, min = 0.1, max = 0.5), digits = 2)
}
cut_2 <- {
  set.seed(2)
  round(runif(1, min = 0.1, max = 0.5), digits = 2)
}
cut_3 <- {
  set.seed(3)
  round(runif(1, min = 0.1, max = 0.5), digits = 2)
}
cut_4 <- {
  set.seed(4)
  round(runif(1, min = 0.1, max = 0.5), digits = 2)
}
# Create last cut so that everything adds up to 100%
cut_5 <- 1 - cut_1 - cut_2 - cut_3 - cut_4

# Generate population data for five subregions
df_population_subregional <- df_population |>
  # duplicating rows 5 times
  tidyr::uncount(5) |>
  # create 5 subregions
  dplyr::mutate(
    spatial_unit = as.character(rep(1:5, times = nrow(fso_population)))
  ) |>
  dplyr::mutate(
    # Distribute original population according to "cuts"
    n = dplyr::case_match(
      spatial_unit,
      "1" ~ round(n * cut_1),
      "2" ~ round(n * cut_2),
      "3" ~ round(n * cut_3),
      "4" ~ round(n * cut_4),
      "5" ~ round(n * cut_5),
      .default = NA
    ),
    .keep = "all"
  )


# Run propop with tables (new feature) ----
system.time({
  result_tables <- propop_tables(
    parameters = df_parameters_subregional,
    year_first = 2019,
    year_last = 2025,
    population = df_population_subregional,
    subregional = TRUE,
    binational = TRUE
  )
})


# Run propop with matrices (original) ----
system.time({
  result_matrices <-
    propop::propop(
      parameters = df_parameters_subregional,
      year_first = 2019,
      year_last = 2025,
      population = df_population_subregional,
      subregional = TRUE,
      binational = TRUE
    )
})


# Compare results ----
compare_models <- result_matrices |>
  arrange(year, nat, sex, age) |>
  left_join(
    result_tables |>
      # clean the data
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
    ggplot2::aes(x = age, y = abs_error, color = as.factor(year))
  ) +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(limits = c(0, 10)) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "right")


### Absolute percentage error
compare_models |>
  ggplot2::ggplot(
    ggplot2::aes(x = age, y = abs_perc_error, color = as.factor(year))
  ) +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(limits = c(0, 150)) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "right")


## Per nationality and sex
### Absolute number of people
compare_models |>
  summarize(abs_error = mean(abs_error), .by = c(year, sex, nat, spatial_unit)) |>
  mutate(nat_sex = paste0(nat, "_", sex)) |>
  ggplot2::ggplot(
    ggplot2::aes(x = year, y = abs_error, color = as.factor(nat_sex), shape = sex)
  ) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_y_continuous(limits = c(0, 10)) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "right")


### Mean absolute percentage error
compare_models |>
  summarize(abs_perc_error = mean(abs_perc_error), .by = c(year, sex, nat, spatial_unit)) |>
  mutate(nat_sex = paste0(nat, "_", sex)) |>
  ggplot2::ggplot(
    ggplot2::aes(x = year, y = abs_perc_error, color = as.factor(nat_sex), shape = sex)
  ) +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(limits = c(0, 10)) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "right")
