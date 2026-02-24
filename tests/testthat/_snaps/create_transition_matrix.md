# create_transition_matrix

    Code
      constructive::construct(create_transition_matrix(n_age_class = 3, mor_ch_f = 0.1,
        mor_ch_m = 0.2, mor_int_f = 0.3, mor_int_m = 0.4, emi_int_ch_f = 0.5,
        emi_int_ch_m = 0.6, emi_int_int_f = 0.7, emi_int_int_m = 0.8, emi_nat_ch_f = 0.1,
        emi_nat_ch_m = 0.2, emi_nat_int_f = 0.4, emi_nat_int_m = 0.3, acq_int_f = 0.9,
        acq_int_m = 0.1))
    Condition
      Warning:
      `create_transition_matrix()` was deprecated in propop 2.0.0.
      i `create_transition_matrix()` is still operational as part of `propop_legacy()` but won't be further maintained
    Output
      new(
        "dgCMatrix" |>
          structure(package = "Matrix"),
        i = c(1L, 2L, 2L, 4L, 5L, 5L, 1L, 7L, 2L, 8L, 2L, 8L, 4L, 10L, 5L, 11L, 5L, 11L),
        p = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 8L, 10L, 12L, 14L, 16L, 18L),
        Dim = c(12L, 12L),
        Dimnames = list(NULL, NULL),
        x = c(
          0.07999999999999996, 0.09000000000000001, 0.33000000000000007,
          0.07999999999999996, 0.09000000000000001, 0.33000000000000007, 0.855,
          0.07999999999999996, -0.36, 0.09000000000000001, -1, 0.33000000000000007,
          0.855, 0.855, -0.36, -0.36, -1, -1
        ),
        factors = list()
      )

