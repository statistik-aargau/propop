# create_mortality_matrix

    Code
      constructive::construct(create_mortality_matrix(n_age_class = 3, mor_ch_f = 0.1,
        mor_ch_m = 0.2, mor_int_f = 0.3, mor_int_m = 0.4, emi_ch_f = 0.5, emi_ch_m = 0.6,
        emi_int_f = 0.7, emi_int_m = 0.8, acq_int_f = 0.9, acq_int_m = 0.1))
    Output
      new(
        "dgCMatrix" |>
          structure(package = "Matrix"),
        i = c(1L, 2L, 2L, 4L, 5L, 5L, 1L, 7L, 2L, 8L, 2L, 8L, 4L, 10L, 5L, 11L, 5L, 11L),
        p = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 8L, 10L, 12L, 14L, 16L, 18L),
        Dim = c(12L, 12L),
        Dimnames = list(NULL, NULL),
        x = c(
          0.14, 0.010000000000000002, 0.07500000000000001, 0.14, 0.010000000000000002,
          0.07500000000000001, 0.045000000000000005, 0.14, 0.22, 0.010000000000000002,
          0.06, 0.07500000000000001, 0.045000000000000005, 0.045000000000000005, 0.22,
          0.22, 0.06, 0.06
        ),
        factors = list()
      )
