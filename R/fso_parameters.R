#' Sample parameters to run population projection
#'
#' @description Data frame containing the rates and number of people from the
#' [Federal Statistical Office (FSO)](https://www.bfs.admin.ch/bfs/en/home.html)
#' required to project the development of four demographic groups for a selected
#' canton (Aargau). The parameters are from the
#' [model published in 2025](https://www.bfs.admin.ch/bfs/en/home/statistics/population/population-projections/national-projections.html).
#' The sample data include three scenarios for the years 2025-2055.
#'
#' @format
#' The example data include the required parameters for each demographic group
#' (nationality (2) X sex (2) X age classes (101)) for the years 2025-2055.
#'
#' @inheritSection get_parameters Demographic groups
#'
#' @inheritSection get_parameters Parameters
#'
#' @inheritSection get_parameters Details about calculated variables
#'
#' @name fso_parameters
#'
#' @inherit get_parameters source
"fso_parameters"
