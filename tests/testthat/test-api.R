test_that("snapshot table px-x-0102010000_101", {

  # don't run on gitlab ci
  skip_on_ci()

  stattab_101 <-  BFS::bfs_get_metadata(number_bfs = "px-x-0102010000_101")
  stattab_101 <- stattab_101|>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = stattab_101 |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  expect_identical(stattab_101, stattab_101_snap)

})

