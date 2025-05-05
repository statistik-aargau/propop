#' Parameters to run population projection
#'
#' @description Uses `get_parametrs` to create data frame with
#' FSO parameters.
#'
#' Update description in folder propop/R/

fso_parameters_raw <- get_parameters(
  year_first = 2024,
  year_last = 2055,
  spatial_units = c("Aargau")
)

# Prepare package data ----
fso_parameters <- fso_parameters_raw |>
  dplyr::mutate(spatial_unit = "Aargau")

# Add data frames to package
usethis::use_data(fso_parameters, overwrite = TRUE)

