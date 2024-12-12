#' Population data from the Federal Statistical Office
#'
#' @description Population from the canton of Aargau in 2018.
#'
#' Update description in folder propop/R/


fso_population <- get_population(
  year_first = 2018,
  year_last = 2018,
  spatial_units = c("- Aargau")
)

usethis::use_data(fso_population, overwrite = TRUE)
