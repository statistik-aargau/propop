#' Parameters to run population projection
#'
#' @description Rates and number of people from the Federal Statistical Office
#' to project the development of four demographic groups. The parameters are
#' from the 2020 model. The data frame only includes the reference scenario and
#' years 2019-2030.
#'
#' Update description in folder propop/R/

# Get "number of people" parameters ----
# Prepare meta data to specify what to download
metadata <- BFS::bfs_get_metadata(number_bfs = "px-x-0104020000_101")
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
      valueTexts %in% c("Aargau")
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
    text == "Jahr"
  ) # get all years

# adapt to the different structure of the "low" scenario table
dim4_103 <- metadata_tidy |>
  dplyr::filter(text == "Jahr") |> # get all years
  dplyr::mutate(values = as.character(0:31))

dim5 <- metadata_tidy |>
  dplyr::filter(
    text == "Staatsangehörigkeit (Kategorie)" & # nationality
      valueTexts %in% c("Schweiz", "Ausland")
  )

dim6 <- metadata_tidy |>
  dplyr::filter(
    text == "Beobachtungseinheit" & # type of parameter types
      valueTexts %in% c(
        "Einwanderungen",
        "Auswanderungen",
        "Interkantonale Zuwanderungen",
        "Interkantonale Abwanderungen",
        "Bevölkerungsstand am 31. Dezember"
      )
  )

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
fso_numbers_r <- BFS::bfs_get_data(
  number_bfs = "px-x-0104020000_101", # reference scenario
  query = dimensions
) |>
  dplyr::rename(
    value = paste0(
      "Szenarien zur Bevölkerungsentwicklung der Kantone 2020-2050,",
      " Referenzszenario AR-00-2020 - zukünftige Bevölkerungsentwicklung"
    )
  ) |>
  dplyr::mutate(scen = "reference")

fso_numbers_h <- BFS::bfs_get_data(
  number_bfs = "px-x-0104020000_102", # high scenario
  query = dimensions
) |>
  dplyr::rename(
    value = paste0(
      "Szenarien zur Bevölkerungsentwicklung der Kantone 2020-2050,",
      " 'hohes' Szenario BR-00-2020 - zukünftige Bevölkerungsentwicklung"
    )
  ) |>
  dplyr::mutate(scen = "high")

fso_numbers_l <- BFS::bfs_get_data(
  number_bfs = "px-x-0104020000_103", # low scenario
  query = dimensions_103
) |>
  dplyr::rename(
    value = paste0(
      "Szenarien zur Bevölkerungsentwicklung der Kantone 2020-2050,",
      " 'tiefes' Szenario CR-00-2020 - zukünftige Bevölkerungsentwicklung"
    )
  ) |>
  dplyr::mutate(scen = "low")

# combine into single data frame
fso_numbers_raw <- dplyr::full_join(fso_numbers_r, fso_numbers_h) |>
  dplyr::full_join(fso_numbers_l)

# Bring variable names and factor levels into the format required later
fso_numbers <- fso_numbers_raw |>
  dplyr::rename(
    nat = "Staatsangehörigkeit (Kategorie)",
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
      "Bevölkerungsstand am 31. Dezember" ~ "n_projected",
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

# Get "rate and probability" parameters ----
# get meta data to determine what to download
metadata <- BFS::bfs_get_metadata(number_bfs = "px-x-0104020000_109")
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
      valueTexts %in% c("Aargau")
  )

dim2 <- metadata_tidy |>
  dplyr::filter(
    text == "Szenario-Variante" & # sex
      valueTexts %in% c(
        "Referenzszenario AR-00-2020",
        "'hohes' Szenario BR-00-2020",
        "'tiefes' Szenario CR-00-2020"
      )
  )
dim3 <- metadata_tidy |>
  dplyr::filter(
    text == "Staatsangehörigkeit (Kategorie)" & # nationality
      valueTexts %in% c("Schweiz", "Ausland")
  )
dim4 <- metadata_tidy |>
  dplyr::filter(
    text == "Geschlecht" & # sex
      valueTexts %in% c("Mann", "Frau")
  )
dim5 <- metadata_tidy |>
  dplyr::filter(
    text == "Alter" & # age
      !(valueTexts %in% "Alter - Total")
  ) # exclude "Total"
dim6 <- metadata_tidy |>
  dplyr::filter(text == "Jahr") # get all years

dim7 <- metadata_tidy |>
  dplyr::filter(text == "Beobachtungseinheit" & # type of parameter types
    valueTexts %in% c(
      "Geburtenziffern",
      "Prospektive Sterbewahrscheinlichkeiten",
      "Auswanderungsziffern",
      "Interkantonale Abwanderungsziffern",
      "Einbürgerungsziffern"
    ))

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

# Download rate parameters
fso_rates_raw <- BFS::bfs_get_data(
  number_bfs = "px-x-0104020000_109",
  query = dimensions
)

# Bring variable names and factor levels into the format required later
fso_rates <- fso_rates_raw |>
  dplyr::rename(
    nat = "Staatsangehörigkeit (Kategorie)",
    sex = Geschlecht,
    age = Alter,
    year = Jahr,
    fso_parameter = Beobachtungseinheit,
    scen = "Szenario-Variante",
    value =
      "Szenarien zur Bevölkerungsentwicklung der Kantone 2020-2050 - Ziffern"
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
      "Einbürgerungsziffern" ~ "acq",
      "Geburtenziffern" ~ "birth_rate"
    )
  )


# Get share of newborns with Swiss citizenship born to internat. mothers ----

# Get meta data to determine what to download
metadata <- BFS::bfs_get_metadata(number_bfs = "px-x-0104020000_106")
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
      valueTexts %in% c("Aargau")
  )

dim2 <- metadata_tidy |>
  dplyr::filter(
    text == "Szenario-Variante" & # scenario
      valueTexts %in% c(
        "Referenzszenario AR-00-2020",
        "'hohes' Szenario BR-00-2020",
        "'tiefes' Szenario CR-00-2020"
      )
  )
dim3 <- metadata_tidy |>
  dplyr::filter(
    text == "Staatsangehörigkeit (Kategorie)" & # nationality
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
  dplyr::filter(text == "Jahr") # get all years

dim7 <- metadata_tidy |>
  dplyr::filter(text == "Beobachtungseinheit" & # type of parameter types
    valueTexts %in%
      c(
        "Lebendgeburten",
        "Lebendgeburten nach Alter und Staatsangehörigkeit der Mutter"
      ))

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

# Download rate parameters
fso_births_int_ch_raw <- BFS::bfs_get_data(
  number_bfs = "px-x-0104020000_106",
  query = dimensions
)

# Process data
fso_births_int_ch <- fso_births_int_ch_raw |>
  # Compute share of Swiss newborns to international mothers
  tidyr::pivot_wider(
    names_from = Beobachtungseinheit,
    values_from = paste0(
      "Szenarien zur Bevölkerungsentwicklung der Kantone ",
      "2020-2050 - zukünftige Bevölkerungsentwicklung"
    )
  ) |>
  # use shorter, clearer names
  dplyr::rename(
    # all live births from international mothers
    live_birth_total =
      "Lebendgeburten nach Alter und Staatsangehörigkeit der Mutter",
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
  select(year, scen, births_int_ch) |>
  dplyr::arrange(year, scen)

# Merge data frames containing numbers and rates ----
fso_parameters_raw <- dplyr::full_join(fso_rates, fso_numbers) |>
  tidyr::pivot_wider(names_from = fso_parameter, values_from = value) |>
  dplyr::mutate(mig_ch = interc_imm - interc_emi) |>
  left_join(fso_births_int_ch, by = c("year", "scen")) |>
  dplyr::arrange(year)

# Prepare package data ----
fso_parameters <- fso_parameters_raw |>
  dplyr::filter(scen == "reference" & year < 2031) |>
  dplyr::mutate(spatial_unit = "Aargau") |>
  dplyr::select(
    nat, sex, age, year, scen, birth_rate, births_int_ch, everything(),
    -c(Kanton, intercant, emi_n, interc_imm, interc_emi, n_projected)
  )

# Prepare data with FSO projections
fso_projections <- fso_parameters_raw |>
  dplyr::filter(scen == "reference" & year < 2031) |>
  dplyr::mutate(spatial_unit = "Aargau") |>
  dplyr::select(year, scen, nat, sex, age, spatial_unit, n_projected)

# Add data frames to package
usethis::use_data(fso_parameters, overwrite = TRUE)
usethis::use_data(fso_projections, overwrite = TRUE)
