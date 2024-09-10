# px-x-0104020000_101
stattab_101_snap <-  BFS::bfs_get_metadata(
  number_bfs = "px-x-0104020000_101")
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

# px-x-0104020000_102
stattab_102_snap <-  BFS::bfs_get_metadata(
  number_bfs = "px-x-0104020000_102")
stattab_102_snap <- stattab_102_snap |>
  dplyr::select(-valueTexts) |>
  tidyr::unnest_longer(values) |>
  dplyr::mutate(
    valueTexts = stattab_102_snap |>
      dplyr::select(valueTexts) |>
      tidyr::unnest_longer(valueTexts) |>
      dplyr::pull(valueTexts)
  ) |>
  dplyr::select(code, text, values, valueTexts, everything())

# px-x-0104020000_103
stattab_103_snap <-  BFS::bfs_get_metadata(
  number_bfs = "px-x-0104020000_103")
stattab_103_snap <- stattab_103_snap |>
  dplyr::select(-valueTexts) |>
  tidyr::unnest_longer(values) |>
  dplyr::mutate(
    valueTexts = stattab_103_snap |>
      dplyr::select(valueTexts) |>
      tidyr::unnest_longer(valueTexts) |>
      dplyr::pull(valueTexts)
  ) |>
  dplyr::select(code, text, values, valueTexts, everything())

# px-x-0104020000_106

stattab_106_snap <- BFS::bfs_get_metadata(
  number_bfs = "px-x-0104020000_106")
stattab_106_snap <- stattab_106_snap |>
  dplyr::select(-valueTexts) |>
  tidyr::unnest_longer(values) |>
  dplyr::mutate(
    valueTexts = stattab_106_snap |>
      dplyr::select(valueTexts) |>
      tidyr::unnest_longer(valueTexts) |>
      dplyr::pull(valueTexts)
  ) |>
  dplyr::select(code, text, values, valueTexts, everything())


# px-x-0104020000_109
stattab_109_snap <- BFS::bfs_get_metadata(
  number_bfs = "px-x-0104020000_109")
stattab_109_snap <- stattab_109_snap |>
  dplyr::select(-valueTexts) |>
  tidyr::unnest_longer(values) |>
  dplyr::mutate(
    valueTexts = stattab_109_snap |>
      dplyr::select(valueTexts) |>
      tidyr::unnest_longer(valueTexts) |>
      dplyr::pull(valueTexts)
  ) |>
  dplyr::select(code, text, values, valueTexts, everything())

# px-x-0102010000_101
stattab_pop_snap <- BFS::bfs_get_metadata(
  number_bfs = "px-x-0102010000_101")
stattab_pop_snap <- stattab_pop_snap |>
  dplyr::select(-valueTexts) |>
  tidyr::unnest_longer(values) |>
  dplyr::mutate(
    valueTexts = stattab_pop_snap |>
      dplyr::select(valueTexts) |>
      tidyr::unnest_longer(valueTexts) |>
      dplyr::pull(valueTexts)
  ) |>
  dplyr::select(code, text, values, valueTexts, everything())


