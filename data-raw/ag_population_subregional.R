#' Population data for the canton of Aargau (five subregions)
#'
#' @description Population from the canton of Aargau divided into five subregions.

# Store data from csv as package data
ag_population_subregional <-
  data.table::fread("data-raw/ag_population_subregional.csv") |>
  as_tibble()

usethis::use_data(ag_population_subregional, overwrite = TRUE)
