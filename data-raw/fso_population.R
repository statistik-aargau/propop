#' Population data from the Federal Statistical Office
#'
#' @description Population from the canton of Aargau in 2023 (most recent
#' data available at time of package release in Mai 2025).
#'
#' Update description in folder propop/R/


fso_population <- get_population(
  year = 2023,
  spatial_units = c("- Aargau")
)

usethis::use_data(fso_population, overwrite = TRUE)
