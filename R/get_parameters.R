#' Get projection parameters from FSO
#'
#' @name get_parameters
#'
#' @description
#' The function gets the mandatory projection parameters for `propop::propop()`
#' from the Federal Statistical Office (FSO). The parameters are only available
#' on the level of cantons. For smaller-scale projections, the parameters must
#' be scaled down.
#' In addition to the parameters, the function also returns the projected
#' population (i.e., number of expected people).
#' All parameters and projections are from the
#' [FSO model published in 2020](https://www.bfs.admin.ch/bfs/en/home/statistics/population/population-projections/national-projections.html).
#' The variables `births_int_ch` and `mig_ch` are not directly available from
#' the FSO. They are calculated by the function.
#'
#' To get projection parameters, you must use the spelling defined in the
#' corresponding FSO table. See
#' \code{vignette("prepare_data", package = "propop")}.
#'
#' Changes to the API interface may break this function. If problems occur,
#' we recommend following the step-by-step procedure described in
#' \code{vignette("prepare_data", package = "propop")}.
#'
#' @return A data frame with the rates and number of people from the
#' [Federal Statistical Office (FSO)](https://www.bfs.admin.ch/bfs/en/home.html)
#' required to project the population development of the requested spatial
#' entities. For each of the four demographic groups (nationality x sex),
#' there are 101 age classes, resulting in a total of 404 rows per requested
#' year and spatial unit.
#'
#' @section Demographic groups:
#' The returned data frame includes parameters for each unique combination of
#' the following demographic variables:
#'   * `nat`: ch = Swiss; int = foreign / international.
#'   * `sex`: f = female, m = male.
#'   * `age`: 101 one-year age classes, ranging from 0 to 100 (including those
#'   older than 100).
#'
#' @section Parameters:
#' The following parameters are included in the returned data frame:
#'    * `year`: character, year of projection.
#'    * `scen`: character, projection scenario.
#'    * `birth_rate`: numeric, number of children per year.
#'    * `births_int_ch`: numeric, proportion of children with Swiss nationality
#'       born to non-Swiss mothers.
#'    * `mor`: numeric, prospective mortality (probability of death).
#'    * `emi`: numeric, rate of people emigrating abroad.
#'    * `acq`: numeric, rate of acquisition of Swiss citizenship.
#'    * `imm_int`: numeric, number of people immigrating from abroad.
#'    * `mig_ch`: numeric, national / inter-cantonal net migration
#'      (number of immigrants minus number of emigrants).
#'    * `spatial_unit`: character, indicating the user requested spatial
#'      unit(s).
#'
#' @section Projected population:
#' `n_projected` is the the number of people per demographic group and year on
#' December 31 (as projected by the FSO in the 2020 model).
#'
#' @section Details about calculated variables:
#'
#' `births_int_ch` is calculated by dividing the number of live newborns with
#' Swiss citizenship born to non-Swiss mothers by the number of all live
#' newborns born to non-Swiss mothers.
#'
#' `mig_ch` is calculated as the difference between the immigration from other
#' cantons and the emigration to other cantons.
#'
#' @param number_fso_ref character, px-x table ID for number parameters
#' (reference scenario), defaults to "px-x-0104020000_101".
#'
#' @param number_fso_high character, px-x table ID for number parameters
#' (high growth scenario), defaults to "px-x-0104020000_102".
#'
#' @param number_fso_low character, px-x table ID for for number parameters
#' (low growth scenario, defaults to "px-x-0104020000_103".
#'
#' @param number_fso_rates character, px-x table ID for rate parameters,
#' defaults to "px-x-0104020000_109".
#'
#' @param number_fso_births character, px-x table ID required to compute the
#' share of Swiss newborns from non-Swiss mothers,
#' defaults to "px-x-0104020000_106".
#'
#' @param year_first numeric,  first year for which the parameters and
#' projections are to be downloaded.
#'
#' @param year_last numeric, last year for which the parameters and projections
#' are to be downloaded.
#'
#' @param spatial_units character vector, indicating at least one spatial
#' entity for which the projection will be run. Typically a canton.
#'
#' @source
#'   Data obtained from the Swiss Federal Statistical Office (FSO):
#'   - <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_101/-/px-x-0104020000_101.px/>
#'   - <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_102/-/px-x-0104020000_102.px>
#'   - <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_103/-/px-x-0104020000_103.px/>
#'   - <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_106/-/px-x-0104020000_106.px/>
#'   - <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_109/-/px-x-0104020000_109.px/>
#'
#' @autoglobal
#'
#' @export
#'
#' @examplesIf (Sys.getenv("RUN_EXPENSIVE_TESTS") == "true")
#' \dontrun{
#' one_canton <- get_parameters(
#'   year_first = 2025,
#'   year_last = 2050,
#'   spatial_units = c("Aargau")
#' )
#' two_cantons_4years <- get_parameters(
#'   year_first = 2018,
#'   year_last = 2021,
#'   spatial_units = c("Aargau", "Zug")
#' )
#' }
get_parameters <- function(number_fso_ref = "px-x-0104020000_101",
                           number_fso_high = "px-x-0104020000_102",
                           number_fso_low = "px-x-0104020000_103",
                           number_fso_rates = "px-x-0104020000_109",
                           number_fso_births = "px-x-0104020000_106",
                           year_first,
                           year_last,
                           spatial_units) {
  # Test input
  # convert input in years to integer, results in error if not possible
  year_first <- vctrs::vec_cast(year_first, integer())
  year_last <- vctrs::vec_cast(year_last, integer())

  assertthat::assert_that(is.integer(year_first),
    dplyr::between(year_first, 2018, 2050),
    msg = paste0(
      "`year_first` must be an integer or a numeric value without decimals",
      " between 2018 and 2050"
    )
  )
  assertthat::assert_that(
    is.integer(year_last), dplyr::between(year_last, 2018, 2050),
    msg = paste0(
      "`year_last` must be an integer or a numeric value without decimals",
      " between 2018 and 2050"
    )
  )
  assertthat::assert_that(is.integer(year_first),
    is.integer(year_last), year_first <= year_last,
    msg = "year_first must be smaller than or equal to year_last"
  )
  assertthat::assert_that(is.vector(spatial_units),
    length(spatial_units) > 0,
    any(sapply(spatial_units, is.character)),
    msg = paste0(
      "The argument 'spatial_unit' must be a non-empty vector with at least",
      " one character elementof type `character`."
    )
  )

  cli::cli_progress_step("Preparing number parameters")

  # Get "number of people" parameters ----
  # Prepare meta data to specify what to download
  metadata <- BFS::bfs_get_metadata(
    number_bfs = number_fso_ref # "px-x-0104020000_101")
  )
  metadata_tidy <- metadata |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = metadata |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  # Specify the elements to download
  dim1 <- metadata_tidy |>
    dplyr::filter(
      text == "Kanton" & # Canton
        valueTexts %in% spatial_units
    )

  dim2 <- metadata_tidy |>
    dplyr::filter(
      text == "Geschlecht" & # sex
        valueTexts %in% c("Mann", "Frau")
    )

  dim3 <- metadata_tidy |>
    dplyr::filter(
      text == "Alter" & # age
        !(valueTexts %in% "Alter - Total")
    ) # exclude "Total"

  dim4 <- metadata_tidy |>
    dplyr::filter(
      text == "Jahr" & # get years
        valueTexts %in% year_first:year_last
    )

  # adapt to the different structure of the "low" scenario table
  number_of_years <- metadata_tidy |>
    # in this version values run from 0 (first year) to 31 (last year)
    dplyr::filter(text == "Jahr") |>
    nrow() - 1

  dim4_103 <- metadata_tidy |>
    # in this version values run from 0 (first year) to 31 (last year)
    dplyr::filter(text == "Jahr") |>
    dplyr::mutate(values = as.character(0:number_of_years)) |>
    # filter requested years
    dplyr::filter(text == "Jahr" & valueTexts %in% year_first:year_last) # |>
  # # values in this version are saved as numeric values
  # dplyr::mutate(values = as.character(0:proj_length))

  dim5 <- metadata_tidy |>
    dplyr::filter(
      # "Staatsangehörigkeit (Kategorie)" & # nationality
      text == stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ) & valueTexts %in% c("Schweiz", "Ausland")
    )

  dim6 <- metadata_tidy |>
    dplyr::filter(text == "Beobachtungseinheit" & # type of parameter types
      valueTexts %in% c(
        "Einwanderungen",
        "Auswanderungen",
        "Interkantonale Zuwanderungen",
        "Interkantonale Abwanderungen",
        stringi::stri_unescape_unicode(
          "Bev\\u00f6lkerungsstand am 31. Dezember"
        )
      ))

  # build dimensions list object
  dimensions <- list(
    dim1$values,
    dim2$values,
    dim3$values,
    dim4$values,
    dim5$values,
    dim6$values
  )

  # add names
  names(dimensions) <- c(
    unique(dim1$code),
    unique(dim2$code),
    unique(dim3$code),
    unique(dim4$code),
    unique(dim5$code),
    unique(dim6$code)
  )

  # version for _103
  # build dimensions list object
  dimensions_103 <- list(
    dim1$values,
    dim2$values,
    dim3$values,
    dim4_103$values,
    dim5$values,
    dim6$values
  )

  # add names
  names(dimensions_103) <- c(
    unique(dim1$code),
    unique(dim2$code),
    unique(dim3$code),
    unique(dim4_103$code),
    unique(dim5$code),
    unique(dim6$code)
  )

  # download number of people parameters

  cli::cli_progress_step("Downloading number parameters (reference scenario)")

  fso_numbers_r <- BFS::bfs_get_data(
    number_bfs = number_fso_ref, # "px-x-0104020000_101",
    query = dimensions
  ) |>
    dplyr::rename(value = stringi::stri_unescape_unicode(
      paste0(
        "Szenarien zur Bev\\u00f6lkerungsentwicklung der Kantone 2020-2050,",
        " Referenzszenario AR-00-2020",
        " - zuk\\u00fcnftige Bev\\u00f6lkerungsentwicklung"
      )
    )) |>
    dplyr::mutate(scen = "reference")

  cli::cli_progress_step(
    "Downloading download number parameters (high growth scenario)"
  )

  fso_numbers_h <- BFS::bfs_get_data(
    number_bfs = number_fso_high, # "px-x-0104020000_102",
    query = dimensions
  ) |>
    dplyr::rename(value = stringi::stri_unescape_unicode(
      paste0(
        "Szenarien zur Bev\\u00f6lkerungsentwicklung der Kantone 2020-2050,",
        " \\'hohes\\' Szenario BR-00-2020",
        " - zuk\\u00fcnftige Bev\\u00f6lkerungsentwicklung"
      )
    )) |>
    dplyr::mutate(scen = "high")

  cli::cli_progress_step(
    "Downloading download number parameters (low growth scenario)"
  )

  fso_numbers_l <- BFS::bfs_get_data(
    number_bfs = number_fso_low, # "px-x-0104020000_103",
    query = dimensions_103
  ) |>
    dplyr::rename(value = stringi::stri_unescape_unicode(
      paste0(
        "Szenarien zur Bev\\u00f6lkerungsentwicklung der Kantone 2020-2050,",
        " \\'tiefes\\' Szenario CR-00-2020",
        " - zuk\\u00fcnftige Bev\\u00f6lkerungsentwicklung"
      )
    )) |>
    dplyr::mutate(scen = "low")


  cli::cli_progress_step("Cleaning number parameters")

  # combine into single data frame
  fso_numbers_raw <- dplyr::full_join(fso_numbers_r, fso_numbers_h) |>
    dplyr::full_join(fso_numbers_l)

  # Bring variable names and factor levels into the format required later
  fso_numbers <- fso_numbers_raw |>
    dplyr::rename(
      nat = stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ), # "Staatsangehörigkeit (Kategorie)",
      sex = Geschlecht,
      age = Alter,
      year = Jahr,
      fso_parameter = Beobachtungseinheit
    ) |>
    # change factor levels
    dplyr::mutate(
      fso_parameter = dplyr::case_match(
        fso_parameter,
        "Auswanderungen" ~ "emi_n",
        stringi::stri_unescape_unicode(
          "Bev\\u00f6lkerungsstand am 31. Dezember"
        ) ~ "n_projected",
        "Einwanderungen" ~ "imm_int",
        "Interkantonale Abwanderungen" ~ "interc_emi",
        "Interkantonale Zuwanderungen" ~ "interc_imm"
      ),
      nat = dplyr::case_match(
        nat,
        "Schweiz" ~ "ch",
        "Ausland" ~ "int"
      ),
      sex = dplyr::case_when(
        sex == "Mann" ~ "m",
        sex == "Frau" ~ "f"
      ),
      age = as.numeric(stringr::str_extract(age, "\\d+"))
    )

  cli::cli_progress_step("Preparing download of rate parameters")

  # Get "rate and probability" parameters ----
  # get meta data to determine what to download
  metadata <- BFS::bfs_get_metadata(
    number_bfs = number_fso_rates # "px-x-0104020000_109")
  )
  metadata_tidy <- metadata |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = metadata |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  # Specify the elements to download
  dim1 <- metadata_tidy |>
    dplyr::filter(
      text == "Kanton" & # Canton
        valueTexts %in% spatial_units
    )
  dim2 <- metadata_tidy |>
    dplyr::filter(
      text == "Szenario-Variante" & # sex
        valueTexts %in% c(
          "Referenzszenario AR-00-2020", "'hohes' Szenario BR-00-2020",
          "'tiefes' Szenario CR-00-2020"
        )
    )
  dim3 <- metadata_tidy |>
    dplyr::filter(
      text == stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ), # "Staatsangehörigkeit (Kategorie)" & # nationality
      valueTexts %in% c("Schweiz", "Ausland")
    )
  dim4 <- metadata_tidy |>
    dplyr::filter(text == "Geschlecht" & valueTexts %in% c("Mann", "Frau")) # sex
  dim5 <- metadata_tidy |>
    dplyr::filter(
      text == "Alter" & # age
        !(valueTexts %in% "Alter - Total")
    ) # exclude "Total"
  dim6 <- metadata_tidy |>
    dplyr::filter(
      text == "Jahr" & # get years
        valueTexts %in% year_first:year_last
    )
  dim7 <- metadata_tidy |>
    dplyr::filter(text == "Beobachtungseinheit" & # type of parameter types
      valueTexts %in% c(
        "Geburtenziffern",
        "Prospektive Sterbewahrscheinlichkeiten",
        "Auswanderungsziffern",
        "Interkantonale Abwanderungsziffern",
        stringi::stri_unescape_unicode("Einb\\u00fcrgerungsziffern")
      )) # "Einbürgerungsziffern"))

  # build dimensions list object
  dimensions <- list(
    dim1$values,
    dim2$values,
    dim3$values,
    dim4$values,
    dim5$values,
    dim6$values,
    dim7$values
  )

  # add names
  names(dimensions) <- c(
    unique(dim1$code),
    unique(dim2$code),
    unique(dim3$code),
    unique(dim4$code),
    unique(dim5$code),
    unique(dim6$code),
    unique(dim7$code)
  )

  cli::cli_progress_step("Downloading rate parameters")

  # Download rate parameters
  fso_rates_raw <- BFS::bfs_get_data(
    number_bfs = "px-x-0104020000_109",
    query = dimensions
  )

  # Bring variable names and factor levels into the format required later
  fso_rates <- fso_rates_raw |>
    dplyr::rename(
      nat = stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ), # "Staatsangehörigkeit (Kategorie)",
      sex = Geschlecht,
      age = Alter,
      year = Jahr,
      fso_parameter = Beobachtungseinheit,
      scen = "Szenario-Variante",
      value = stringi::stri_unescape_unicode(
        paste0(
          "Szenarien zur Bev\\u00f6lkerungsentwicklung der Kantone",
          " 2020-2050 - Ziffern"
        )
      )
    ) |>
    # change factor levels
    dplyr::mutate(
      scen = dplyr::case_match(
        scen,
        "Referenzszenario AR-00-2020" ~ "reference",
        "'hohes' Szenario BR-00-2020" ~ "high",
        "'tiefes' Szenario CR-00-2020" ~ "low"
      ),
      nat = dplyr::case_match(
        nat,
        "Schweiz" ~ "ch",
        "Ausland" ~ "int"
      ),
      sex = dplyr::case_when(
        sex == "Mann" ~ "m",
        sex == "Frau" ~ "f"
      ),
      age = as.numeric(stringr::str_extract(age, "\\d+")),
      fso_parameter = dplyr::case_match(
        fso_parameter,
        "Prospektive Sterbewahrscheinlichkeiten" ~ "mor",
        "Auswanderungsziffern" ~ "emi",
        "Interkantonale Abwanderungsziffern" ~ "intercant",
        # "Einbürgerungsziffern" ~ "acq",
        stringi::stri_unescape_unicode("Einb\\u00fcrgerungsziffern") ~ "acq",
        "Geburtenziffern" ~ "birth_rate"
      )
    )


  # Get share of newborns with Swiss citizenship born to internat. mothers ----

  cli::cli_progress_step("Preparing download of birth parameter")

  # Get meta data to determine what to download
  metadata <- BFS::bfs_get_metadata(
    number_bfs = number_fso_births
  ) # "px-x-0104020000_106")
  metadata_tidy <- metadata |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = metadata |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  # Specify the elements to download
  dim1 <- metadata_tidy |>
    dplyr::filter(
      text == "Kanton" & # Canton
        valueTexts %in% spatial_units
    )

  dim2 <- metadata_tidy |>
    dplyr::filter(text == "Szenario-Variante" & # scenario
      valueTexts %in% c(
        "Referenzszenario AR-00-2020",
        "'hohes' Szenario BR-00-2020",
        "'tiefes' Szenario CR-00-2020"
      ))
  dim3 <- metadata_tidy |>
    dplyr::filter(
      text == stringi::stri_unescape_unicode(
        "Staatsangeh\\u00f6rigkeit (Kategorie)"
      ) & # "Staatsangehörigkeit (Kategorie)" & # nationality
        valueTexts %in% "Ausland"
    )
  dim4 <- metadata_tidy |>
    dplyr::filter(
      text == "Geschlecht" & # sex
        valueTexts %in% "Geschlecht - Total"
    )
  dim5 <- metadata_tidy |>
    dplyr::filter(
      text == "Altersklasse" & # age
        valueTexts %in% "Altersklasse - Total"
    )
  dim6 <- metadata_tidy |>
    dplyr::filter(
      text == "Jahr" & # get years
        valueTexts %in% year_first:year_last
    )
  dim7 <- metadata_tidy |>
    dplyr::filter(
      text == "Beobachtungseinheit" & # type of parameter types
        valueTexts %in%
          c(
            "Lebendgeburten",
            stringi::stri_unescape_unicode(
              "Lebendgeburten nach Alter und Staatsangeh\\u00f6rigkeit der Mutter"
            )
          )
    ) # "Lebendgeburten nach Alter und Staatsangehörigkeit der Mutter"))

  # build dimensions list object
  dimensions <- list(
    dim1$values,
    dim2$values,
    dim3$values,
    dim4$values,
    dim5$values,
    dim6$values,
    dim7$values
  )

  # add names
  names(dimensions) <- c(
    unique(dim1$code),
    unique(dim2$code),
    unique(dim3$code),
    unique(dim4$code),
    unique(dim5$code),
    unique(dim6$code),
    unique(dim7$code)
  )


  cli::cli_progress_step("Downloading birth parameter")

  # Download rate parameters
  fso_births_int_ch_raw <- BFS::bfs_get_data(
    number_bfs = number_fso_births, # "px-x-0104020000_106",
    query = dimensions
  )

  # Process data
  fso_births_int_ch <- fso_births_int_ch_raw |>
    # Compute share of Swiss newborns to international mothers
    tidyr::pivot_wider(
      names_from = Beobachtungseinheit,
      values_from = stringi::stri_unescape_unicode(
        paste0(
          "Szenarien zur Bev\\u00f6lkerungsentwicklung der Kantone 2020-2050",
          " - zuk\\u00fcnftige Bev\\u00f6lkerungsentwicklung"
        )
      )
    ) |>
    # use shorter, clearer names
    dplyr::rename(
      # all live births from international mothers
      live_birth_total = stringi::stri_unescape_unicode(
        "Lebendgeburten nach Alter und Staatsangeh\\u00f6rigkeit der Mutter"
      ), # "Lebendgeburten nach Alter und Staatsangehörigkeit der Mutter",
      # live births of international newborns to international mothers
      live_birth_int = Lebendgeburten
    ) |>
    dplyr::mutate(
      births_int_ch = (live_birth_total - live_birth_int) / live_birth_total
    ) |>
    # Bring variable names and factor levels into the format required later
    dplyr::rename(
      scen = "Szenario-Variante",
      age = Altersklasse,
      year = Jahr
    ) |>
    # change factor levels
    dplyr::mutate(
      scen = dplyr::case_match(
        scen,
        "Referenzszenario AR-00-2020" ~ "reference",
        "'hohes' Szenario BR-00-2020" ~ "high",
        "'tiefes' Szenario CR-00-2020" ~ "low"
      )
    ) |>
    # remove unnecessary variables
    dplyr::select(Kanton, year, scen, births_int_ch) |>
    dplyr::arrange(Kanton, year, scen)


  cli::cli_progress_step("Merging and cleaning parameters")

  # Merge data frames containing numbers and rates ----
  projection_parameters <- dplyr::full_join(fso_rates, fso_numbers) |>
    tidyr::pivot_wider(names_from = fso_parameter, values_from = value) |>
    dplyr::mutate(mig_ch = interc_imm - interc_emi) |>
    dplyr::left_join(fso_births_int_ch, by = c("Kanton", "year", "scen")) |>
    dplyr::arrange(year)

  # Clean data ----
  projection_parameters_clean <- projection_parameters |>
    dplyr::mutate(spatial_unit = Kanton) |>
    dplyr::select(
      nat, sex, age, year, scen, birth_rate, births_int_ch, everything(),
      -c(Kanton, intercant, emi_n, interc_imm, interc_emi)
    )

  return(projection_parameters_clean)
}
