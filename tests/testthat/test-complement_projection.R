test_that("snapshot test complement_projection()", {

  skip_on_ci()

  # create skeleton with variable labels and levels
  # constructive::construct(head(skeleton, 10))
  empty_df <-
    tibble::tibble(
      age = 0:9,
      sex = factor(rep("m", 10L), levels = c("m", "f")),
      nat = factor(rep("ch", 10L), levels = c("ch", "int")),
      year = rep(2018L, 10L),
      spatial_levels = factor(rep("Aargau", 10L)),
    )

  # create df with raw projection results
  # constructive::construct(head(projection_raw, 10))
  raw_df <-
    data.frame(
      N = c(2506, 2589, 2568, 2635, 2710, 2608, 2569, 2611, 2634, 2512)
    )

  # run complement_projection
  output_table <- complement_projection(empty_df, raw_df)


  # run tests
  expect_snapshot(constructive::construct(output_table))
})
