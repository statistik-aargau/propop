#' Parameters to run population projection
#'
#' @description Uses `get_parametrs` to create data frame with rates and number
#' of people parameters provided by the Federal Statistical Office (FSO)
#' to project the development of four demographic groups. The parameters are
#' from the 2020 model. The data frame only includes the reference scenario and
#' years 2019-2030.
#'
#' Update description in folder propop/R/

fso_parameters_raw <- get_parameters(
  year_first = 2019,
  year_last = 2030,
  spatial_units = c("Aargau")
)

# Prepare package data ----
fso_parameters <- fso_parameters_raw |>
  dplyr::filter(scen == "reference") |>
  dplyr::mutate(spatial_unit = "Aargau")

# Add data frames to package
usethis::use_data(fso_parameters, overwrite = TRUE)

