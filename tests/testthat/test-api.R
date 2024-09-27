test_that("snapshot table px-x-0104020000_101", {

  # run on gitlab ci
  # skip_on_ci()

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

  # load(test_path("_snaps", "stattab_101_snap.rda"))
  stattab_101_snap <- propop::stattab_101_snap
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

  # load(test_path("_snaps", "stattab_102_snap.rda"))
  stattab_102_snap <- propop::stattab_102_snap
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

  # load(test_path("_snaps", "stattab_103_snap.rda"))
  stattab_103_snap <- propop::stattab_103_snap
  expect_identical(stattab_103, stattab_103_snap)

})


test_that("snapshot table px-x-0104020000_106", {

  # run on gitlab ci
  # skip_on_ci()

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

  # load(test_path("_snaps", "stattab_106_snap.rda"))
  stattab_106_snap <- propop::stattab_106_snap
  expect_identical(stattab_106, stattab_106_snap)

})


test_that("snapshot table px-x-0104020000_109", {

  # run on gitlab ci
  # skip_on_ci()

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

  # load(test_path("_snaps", "stattab_109_snap.rda"))
  stattab_109_snap <- propop::stattab_109_snap
  expect_identical(stattab_109, stattab_109_snap)

})


test_that("snapshot table px-x-0102010000_101", {

  # run on gitlab ci
  # skip_on_ci()

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

  # load(test_path("_snaps", "stattab_pop_snap.rda"))
  stattab_pop_snap <- propop::stattab_pop_snap
  expect_identical(stattab_pop, stattab_pop_snap)

})
