test_that("create_fertility_matrix", {
  skip_on_ci()

  expect_snapshot(constructive::construct(
    create_fertility_matrix(
      fert_first = 16,
      fert_last = 50,
      fert_length = 35,
      n_age_class = 101,
      share_born_female = 0.49,
      birthrate_ch = 0.015,
      birthrate_int = 0.02,
      int_mothers = 0.7,
      mor_ch_f_0 = 0.003,
      mor_ch_m_0 = 0.004,
      mor_int_f_0 = 0.005,
      mor_int_m_0 = 0.006,
      emi_int_ch_f_0 = 0.001,
      emi_int_ch_m_0 = 0.002,
      emi_int_int_f_0 = 0.003,
      emi_int_int_m_0 = 0.004,
      emi_nat_ch_f = 0.001,
      emi_nat_ch_m = 0.002,
      emi_nat_int_f = 0.004,
      emi_nat_int_m = 0.0023,
      acq_int_f_0 = 0.01,
      acq_int_m_0 = 0.01
    )
  ))

  result <- create_fertility_matrix(
    fert_first = 16,
    fert_last = 50,
    fert_length = 35,
    n_age_class = 101,
    share_born_female = 0.49,
    birthrate_ch = 0.015,
    birthrate_int = 0.02,
    int_mothers = 0.7,
    mor_ch_f_0 = 0.003,
    mor_ch_m_0 = 0.004,
    mor_int_f_0 = 0.005,
    mor_int_m_0 = 0.006,
    emi_int_ch_f_0 = 0.001,
    emi_int_ch_m_0 = 0.002,
    emi_int_int_f_0 = 0.003,
    emi_int_int_m_0 = 0.004,
    emi_nat_ch_f = 0.001,
    emi_nat_ch_m = 0.002,
    emi_nat_int_f = 0.004,
    emi_nat_int_m = 0.0023,
    acq_int_f_0 = 0.01,
    acq_int_m_0 = 0.01
  )

  # Test matrix dimensions
  expect_equal(dim(result)[1], dim(result)[2])

  # Test that the matrix is sparse
  expect_true(is(result, "sparseMatrix"))
})
