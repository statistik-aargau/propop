#' Subregional migration records for the canton of Aargau (five subregions)
#'
#' @description Raw migration data from the canton of Aargau divided into five subregions.

# Store data from csv as package data
ag_migration_subregional <-
  data.table::fread("data-raw/ag_migration_subregional.csv") |>
  as_tibble()

usethis::use_data(ag_migration_subregional, overwrite = TRUE)
