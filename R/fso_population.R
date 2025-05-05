#' Sample population data from the Federal Statistical Office
#'
#' @description
#' Example data frame containing the starting population required to
#' project the development of four demographic groups for a selected
#' canton (Aargau). Population data can be downloaded from the FSO using
#' the `get_population`.
#'
#' At the time of the release of {propop} v1.3 in May 2025, the most recent data
#' available via `get_population` are from 2023. To avoid a gap between the
#' starting population (2023) and the first year of projection (2025),
#' `fso_population` is currently based on the estimated starting population
#' (`start_n`) for the year 2025 from `fso_parameters` (obtained via
#' `get_parameters`). The missing number of newborns were imputed.
#'
#' @format
#' The example population records include the number of people of each
#' demographic group (nationality (2) X sex (2) X age classes (101)) for the
#' canton of Aargau in 2024.
#'
#' @name fso_population
#'
#' @inherit get_population return
#'
#' @inherit get_population source
"fso_population"
