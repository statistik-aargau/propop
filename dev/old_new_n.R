# Get projections and benchmark data according to new method ----
data_benchmark <- get_population(
  number_fso = "px-x-0102010000_101",
  year_first = 2018,
  year_last = 2022,
  spatial_units = "- Aargau"
)

data_parameters <- get_parameters(
  year_first = 2019,
  year_last = 2050,
  spatial_units = "Aargau"
)

data_projected_new <- propop(
  parameters = data_parameters |>
    dplyr::filter(scen == "reference"),
  year_first = 2019,
  year_last = 2050,
  age_groups = 101,
  fert_first = 16,
  fert_last = 50,
  share_born_female = 100 / 205,
  # population records from 2018 as starting point
  population = data_benchmark |>
    dplyr::filter(year == 2018),
  subregional = FALSE,
  binational = TRUE
)



# Download and import data_projected from github (in vignette folder) ----
## DO THIS MANUALLY



# Compare old and new ----
comp_projections <-left_join(data_projected_new,
          data_projected |> rename(n_old = n),
          by = c("nat", "sex", "age", "year")) |>
  mutate(n_diff = n_old - n_1) |>
  select(year, nat, sex, age, n_old, n, n_start, n_1)#, n_diff)


# table(round(comp_projections$n_diff, digits = 0))

comp_projections |> filter((age < 3 | age > 98) & nat == "ch" & sex == "m")
