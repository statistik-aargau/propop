test_that("create_transition_matrix", {
  skip_on_ci()

  expect_snapshot(constructive::construct(
    create_transition_matrix(
      n_age_class = 3,
      mor_ch_f = 0.1,
      mor_ch_m = 0.2,
      mor_int_f = 0.3,
      mor_int_m = 0.4,
      emi_int_ch_f = 0.5,
      emi_int_ch_m = 0.6,
      emi_int_int_f = 0.7,
      emi_int_int_m = 0.8,
      emi_nat_ch_f = 0.1,
      emi_nat_ch_m = 0.2,
      emi_nat_int_f = 0.4,
      emi_nat_int_m = 0.3,
      acq_int_f = 0.9,
      acq_int_m = 0.1
    )
  ))

  # Test function
  result <- create_transition_matrix(
    n_age_class = 3,
    mor_ch_f = 0.1,
    mor_ch_m = 0.2,
    mor_int_f = 0.3,
    mor_int_m = 0.4,
    emi_int_ch_f = 0.5,
    emi_int_ch_m = 0.6,
    emi_int_int_f = 0.7,
    emi_int_int_m = 0.8,
    emi_nat_ch_f = 0.1,
    emi_nat_ch_m = 0.2,
    emi_nat_int_f = 0.4,
    emi_nat_int_m = 0.3,
    acq_int_f = 0.9,
    acq_int_m = 0.1
  )

  # Test matrix dimensions
  expect_equal(dim(result)[1], dim(result)[2])

  # Test that the matrix is sparse
  expect_true(is(result, "sparseMatrix"))
})
