#' Create transition matrix (helper function for project_raw)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @description The transition matrix is used to calculate age-specific
#' survival rates and population fluxes through emigration and acquisition
#' of the Swiss citizenship.
#'
#' @param n_age_class numeric, number of age groups (e.g., 101).
#' @param mor_ch_f numeric, mortality of Swiss females.
#' @param mor_ch_m numeric, mortality of Swiss males.
#' @param mor_int_f numeric, mortality of foreign females.
#' @param mor_int_m numeric, mortality of foreign males.
#' @param emi_int_ch_f numeric, international emigration of Swiss females.
#' @param emi_int_ch_m numeric, international emigration of Swiss males.
#' @param emi_int_int_f numeric, international emigration of foreign females.
#' @param emi_int_int_m numeric, international emigration of foreign males.
#' @param emi_nat_ch_f numeric, emigration to other cantons of Swiss females.
#' @param emi_nat_ch_m numeric, emigration to other cantons of Swiss males.
#' @param emi_nat_int_f numeric, emigration to other cantons of foreign females.
#' @param emi_nat_int_m numeric, emigration to other cantons of foreign males.
#' @param acq_int_f numeric, acquisition of Swiss citizenship by foreign females.
#' @param acq_int_m numeric, acquisition of Swiss citizenship of foreign males.
#'
#' @noRd

create_transition_matrix <-
  function(n_age_class,
           mor_ch_f,
           mor_ch_m,
           mor_int_f,
           mor_int_m,
           emi_int_ch_f,
           emi_int_ch_m,
           emi_int_int_f,
           emi_int_int_m,
           emi_nat_ch_f,
           emi_nat_int_f,
           emi_nat_ch_m,
           emi_nat_int_m,
           acq_int_f,
           acq_int_m) {
    # Deprecate
    lifecycle::deprecate_warn(
      "2.0.0", "create_transition_matrix()",
      details = paste0(
        "`create_transition_matrix()` is still operational as part of `propop_legacy()` but ",
        "won't be further maintained"
      )
    )
    # Set indices -------------------------------------------------------------
    index_row <-
      c(
        2:n_age_class,
        n_age_class,
        2:n_age_class,
        n_age_class,
        2:n_age_class + n_age_class,
        n_age_class + n_age_class,
        2:n_age_class + n_age_class,
        n_age_class + n_age_class,
        2:n_age_class + (n_age_class * 2),
        n_age_class + (n_age_class * 2),
        2:n_age_class + (n_age_class * 3),
        n_age_class + (n_age_class * 3)
      )

    index_column <-
      c(
        1:n_age_class,
        1:n_age_class + (n_age_class * 2),
        1:n_age_class + n_age_class,
        1:n_age_class + (n_age_class * 3),
        1:n_age_class + (n_age_class * 2),
        1:n_age_class + (n_age_class * 3)
      )

    assertthat::assert_that(
      !any(
        sapply(
          list(index_row, index_column),
          function(x) is.null(x) || length(x) == 0
        )
      ),
      msg = paste0(
        "Transition matrix contains NAs within indices: `index_row`,",
        " `index_column`."
      )
    )

    assertthat::assert_that(
      !is.null(n_age_class),
      msg =
        "Transition matrix contains NAs within this parameter: `n_age_class`."
    )
    # Create vectors ----------------------------------------------------------
    # Swiss male to Swiss male
    ch_m_ch_m <- (1 - mor_ch_m) - ((emi_int_ch_m + emi_nat_ch_m) * (1 - mor_ch_m / 2))

    # Foreign male to Swiss male
    int_m_ch_m <- acq_int_m * (1 - mor_ch_m / 2)

    # Swiss female to Swiss female
    ch_f_ch_f <- (1 - mor_ch_f) - ((emi_int_ch_f + emi_nat_ch_f) * (1 - mor_ch_f / 2))

    # Foreign female to Swiss female
    int_f_ch_f <- acq_int_f * (1 - mor_ch_f / 2)

    # Foreign male to foreign male
    int_m_int_m <-
      (1 - mor_int_m) - ((emi_int_int_m + acq_int_m + emi_nat_int_m) * (1 - mor_int_m / 2))

    # Foreign female to foreign female
    int_f_int_f <-
      (1 - mor_int_f) - ((emi_int_int_f + acq_int_f + emi_nat_int_f) * (1 - mor_int_f / 2))

    assertthat::assert_that(
      !any(
        sapply(
          list(
            ch_m_ch_m, int_m_ch_m, ch_f_ch_f, int_f_ch_f, int_m_int_m,
            int_f_int_f
          ),
          function(x) is.null(x) || length(x) == 0
        )
      ),
      msg = "Transition matrix contains NAs within these vectors: `ch_m_ch_m`,
      `int_m_ch_m`, `ch_f_ch_f`, `int_f_ch_f`, `int_m_int_m`, `int_f_int_f`"
    )

    # Create matrix -----------------------------------------------------------
    Matrix::sparseMatrix(
      i = index_row,
      j = index_column,
      x = c(
        ch_m_ch_m, int_m_ch_m, ch_f_ch_f, int_f_ch_f, int_m_int_m, int_f_int_f
      ),
      dims = c(2 * 2 * n_age_class, 2 * 2 * n_age_class),
      check = TRUE
    )
  }
