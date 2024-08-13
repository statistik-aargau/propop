#' Population data from the Federal Statistical Office
#'
#' @description Population from the canton of Aargau in 2018.
#'
#' Update description in folder propop/R/

# Get meta data and prepare query for the population data:
metadata_pop <- BFS::bfs_get_metadata(number_bfs = "px-x-0102010000_101")
metadata_pop_tidy <- metadata_pop |>
  dplyr::select(-valueTexts) |>
  tidyr::unnest_longer(values) |>
  dplyr::mutate(
    valueTexts = metadata_pop |>
      dplyr::select(valueTexts) |>
      tidyr::unnest_longer(valueTexts) |>
      dplyr::pull(valueTexts)
  ) |>
  dplyr::select(code, text, values, valueTexts, everything())
metadata_pop_tidy

# Specify the elements to download
dim1 <- metadata_pop_tidy |>
  dplyr::filter(
    text == "Kanton (-) / Bezirk (>>) / Gemeinde (......)" & # Canton
      valueTexts %in% c("- Aargau")
  )

dim2 <- metadata_pop_tidy |>
  dplyr::filter(
    text == "Jahr" &
      valueTexts %in% c("2018")
  ) # get population in December

dim3 <- metadata_pop_tidy |>
  dplyr::filter(
    text == "Bevölkerungstyp" & # permanent
      valueTexts %in% "Ständige Wohnbevölkerung"
  )

dim4 <- metadata_pop_tidy |>
  dplyr::filter(
    text == "Staatsangehörigkeit (Kategorie)" & # nationality
      valueTexts %in% c("Schweiz", "Ausland")
  )

dim5 <- metadata_pop_tidy |>
  dplyr::filter(
    text == "Geschlecht" & # sex
      valueTexts %in% c("Mann", "Frau")
  )

dim6 <- metadata_pop_tidy |>
  dplyr::filter(
    text == "Alter" & # age
      !(valueTexts %in% "Alter - Total")
  ) # exclude "Total"

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

# Download population data
fso_pop_raw <- BFS::bfs_get_data(
  number_bfs = "px-x-0102010000_101",
  query = dimensions
)

# Bring variable names and factor levels into the format required later
fso_population <- fso_pop_raw |>
  dplyr::select(-"Bevölkerungstyp") |>
  dplyr::rename(
    year = Jahr,
    Kanton = "Kanton (-) / Bezirk (>>) / Gemeinde (......)",
    nat = "Staatsangehörigkeit (Kategorie)",
    sex = Geschlecht,
    age = Alter,
    n = "Ständige und nichtständige Wohnbevölkerung"
  ) |>
  # change factor levels
  dplyr::mutate(
    Kanton = stringr::str_remove_all(Kanton, "- "),
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
  ) |>
  dplyr::rename(spatial_unit = Kanton)

usethis::use_data(fso_population, overwrite = TRUE)
