test_that("create_birth_matrix", {

  expect_snapshot(constructive::construct(
    create_birth_matrix(
      fert_first = 16,
      fert_last = 50,
      fert_length = 50 - 16 + 1,
      n_age_class = 101,
      share_born_female = 0.49,
      birthrate_ch = rep(0.02, (50 - 16 + 1)),
      birthrate_int = rep(0.025, (50 - 16 + 1)),
      int_mothers = 0.7
    )
  ))

  result <-
    create_birth_matrix(
      fert_first = 16,
      fert_last = 50,
      fert_length = 50 - 16 + 1,
      n_age_class = 101,
      share_born_female = 0.49,
      birthrate_ch = rep(0.02, (50 - 16 + 1)),
      birthrate_int = rep(0.025, (50 - 16 + 1)),
      int_mothers = 0.7
    )

  # Test matrix dimensions
  expect_equal(dim(result)[1], dim(result)[2])

  # Test that the matrix is sparse
  expect_true(is(result, "sparseMatrix"))
})
