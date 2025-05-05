#' Population data from the Federal Statistical Office
#'
#' @description Population from the canton of Aargau.
#'
#' Update description in folder propop/R/


# Default way to create data, contingent on STATPOP availability
fso_population <- get_population(
  year = 2023,
  spatial_units = c("- Aargau")
)

usethis::use_data(fso_population, overwrite = TRUE)
