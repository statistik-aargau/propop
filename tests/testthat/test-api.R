test_that("snapshot table px-x-0104020000_101", {

  # don't run on gitlab ci
  skip_on_ci()

  stattab_101 <-  BFS::bfs_get_metadata(
    number_bfs = "px-x-0104020000_101")
  stattab_101 <- stattab_101 |>
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

test_that("snapshot table px-x-0104020000_102", {

  # don't run on gitlab ci
  skip_on_ci()

  stattab_102 <-  BFS::bfs_get_metadata(
    number_bfs = "px-x-0104020000_102")
  stattab_102 <- stattab_102 |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = stattab_102 |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  expect_identical(stattab_102, stattab_102_snap)

})



test_that("snapshot table px-x-0104020000_103", {

  # don't run on gitlab ci
  skip_on_ci()

  stattab_103 <-  BFS::bfs_get_metadata(
    number_bfs = "px-x-0104020000_103")
  stattab_103 <- stattab_103 |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = stattab_103 |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  expect_identical(stattab_103, stattab_103_snap)

})


test_that("snapshot table px-x-0104020000_106", {

  # don't run on gitlab ci
  skip_on_ci()

  stattab_106 <-  BFS::bfs_get_metadata(
    number_bfs = "px-x-0104020000_106")
  stattab_106 <- stattab_106 |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = stattab_106 |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  expect_identical(stattab_106, stattab_106_snap)

})


test_that("snapshot table px-x-0104020000_109", {

  # don't run on gitlab ci
  skip_on_ci()

  stattab_109 <-  BFS::bfs_get_metadata(
    number_bfs = "px-x-0104020000_109")
  stattab_109 <- stattab_109 |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = stattab_109 |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  expect_identical(stattab_109, stattab_109_snap)

})


test_that("snapshot table px-x-0102010000_101", {

  # don't run on gitlab ci
  skip_on_ci()

  stattab_pop <-  BFS::bfs_get_metadata(
    number_bfs = "px-x-0102010000_101")
  stattab_pop <- stattab_pop |>
    dplyr::select(-valueTexts) |>
    tidyr::unnest_longer(values) |>
    dplyr::mutate(
      valueTexts = stattab_pop |>
        dplyr::select(valueTexts) |>
        tidyr::unnest_longer(valueTexts) |>
        dplyr::pull(valueTexts)
    ) |>
    dplyr::select(code, text, values, valueTexts, everything())

  expect_identical(stattab_pop, stattab_pop_snap)

})
