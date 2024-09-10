stattab_101_snap <-  BFS::bfs_get_metadata(number_bfs = "px-x-0102010000_101")
stattab_101_snap <- stattab_101_snap |>
  dplyr::select(-valueTexts) |>
  tidyr::unnest_longer(values) |>
  dplyr::mutate(
    valueTexts = stattab_101_snap |>
      dplyr::select(valueTexts) |>
      tidyr::unnest_longer(valueTexts) |>
      dplyr::pull(valueTexts)
  ) |>
  dplyr::select(code, text, values, valueTexts, everything())
