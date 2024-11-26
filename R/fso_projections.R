#' Sample population projections from FSO for the canton of Aargau
#'
#' @description Data frame containing the population projections from the
#' Federal Statistical Office (FSO) for four demographic groups for the canton
#' of Aargau. The projections are from the model published in 2020. The sample
#' data only include the reference scenario and the years 2019-2030. Projections
#' may be obtained for other spatial units using the function `get_parameters`.
#'
#' @name fso_projections
#'
#' @format
#' The example projections include the projected number of people of each
#' demographic group (nationality (2) X sex (2) X age classes (101)) for the
#' years 2019-2030.
#'
#' @section Variables:
#'   * `year`: character, year of projection.
#'   * `scen`: character, only including the "reference" scenario.
#'   * `nat`: character, ch = Swiss; int = foreign / international.
#'   * `sex`: character, f = female, m = male.
#'   * `age`: numeric, 101 one-year age classes, ranging from 0 to 100
#'   (including those older than 100).
#'   * `spatial_unit`: character, indicating that the data refer to the canton
#'      Aargau.
#'   * `n_projected`: numeric, number of people per demographic group and year.
#'
#' @source Federal Statistical Office: <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_101/-/px-x-0104020000_101.px/>
"fso_projections"
