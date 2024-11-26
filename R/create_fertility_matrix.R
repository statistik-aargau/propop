#' Create fertility matrix (helper function for project_raw)
#'
#' @description The fertility matrix is used to calculate the age-specific
#' number of offspring for females within the fertility range given by
#' `fert_length`.
#'
#' @param fert_first numeric, first year/age of fertility.
#' @param fert_last numeric, last year/age of fertility.
#' @param fert_length numeric, fertility length.
#' @param n_age_class numeric, number of age groups (e.g., 101).
#' @param share_born_female numeric,share of female newborns
#' relative to all newborns.
#' @param birthrate_ch numeric,birth rate among Swiss women.
#' @param birthrate_int numeric,birth rate among foreign women.
#' @param int_mothers numeric, proportion of children with Swiss nationality
#' born to non-Swiss mothers.
#' @param mor_ch_f_0 numeric, mortality of Swiss females aged zero years.
#' @param mor_ch_m_0 numeric, mortality of Swiss males aged zero years.
#' @param mor_int_f_0 numeric, mortality of foreign females aged zero years.
#' @param mor_int_m_0 numeric, mortality of foreign males aged zero years.
#' @param emi_int_ch_f_0 numeric, emigration of Swiss females aged zero years.
#' @param emi_int_ch_m_0 numeric, emigration of Swiss males aged zero years.
#' @param emi_int_f_0 numeric, emigration of foreign females aged zero years.
#' @param emi_int_m_0 numeric, emigration of foreign males aged zero years.
#' @param acq_int_f_0 numeric, acquisition of Swiss citizenship by foreign
#' females aged zero years.
#' @param acq_int_m_0 numeric, acquisition of Swiss citizenship of foreign
#' males aged zero years.
#'
#' @noRd

create_fertility_matrix <-
  function(fert_first,
           fert_last,
           fert_length,
           n_age_class,
           share_born_female,
           birthrate_ch,
           birthrate_int,
           int_mothers,
           mor_ch_f_0,
           mor_ch_m_0,
           mor_int_f_0,
           mor_int_m_0,
           emi_int_ch_f_0,
           emi_int_ch_m_0,
           emi_int_int_f_0,
           emi_int_int_m_0,
           acq_int_f_0,
           acq_int_m_0) {
    # Set indices -------------------------------------------------------------
    index_row <-
      c(
        rep(1, fert_length),
        rep(1, fert_length),
        rep(1, fert_length) + n_age_class,
        rep(1, fert_length) + n_age_class,
        rep(1, fert_length) + (n_age_class * 2),
        rep(1, fert_length) + (n_age_class * 3)
      )

    index_column <-
      c(
        fert_first:fert_last + 1 + n_age_class,
        fert_first:fert_last + 1 + (n_age_class * 3),
        fert_first:fert_last + 1 + n_age_class,
        fert_first:fert_last + 1 + (n_age_class * 3),
        fert_first:fert_last + 1 + (n_age_class * 3),
        fert_first:fert_last + 1 + (n_age_class * 3)
      )

    share_male <- 1 - share_born_female

    assertthat::assert_that(
      !any(
        sapply(
          list(index_row, index_column),
          function(x) is.null(x) || length(x) == 0
        )
      ),
      msg = paste0(
        "Fertility matrix contains NAs within indices: `index_row`,",
        " `index_column`."
      )
    )

    assertthat::assert_that(
      !any(
        sapply(
          list(n_age_class, share_male),
          function(x) is.null(x) || length(x) == 0
        )
      ),
      msg = paste0(
        "Fertility matrix contains NAs within these parameters:",
        " `n_age_class`, `share_male`."
      )
    )
    # Create vectors ----------------------------------------------------------
    # Swiss females giving birth to Swiss males
    ch_f_ch_m <-
      share_male * birthrate_ch *
        ((1 - mor_ch_m_0) - emi_int_ch_m_0 * (1 - 2 / 3 * mor_ch_m_0))

    # Foreign females giving birth to Swiss males
    int_f_ch_m <-
      share_male * birthrate_int * int_mothers *
      ((1 - mor_ch_m_0) - emi_int_ch_m_0 * (1 - 2 / 3 * mor_ch_m_0)) +
      share_male * birthrate_int * (1 - int_mothers) *
        (acq_int_m_0 * (1 - 2 / 3 * mor_ch_m_0))

    # Swiss females giving birth to Swiss females
    ch_f_ch_f <-
      share_born_female * birthrate_ch *
        ((1 - mor_ch_f_0) - (emi_int_ch_f_0) * (1 - 2 / 3 * mor_ch_f_0))

    # Foreign females giving birth to Swiss females
    int_f_ch_f <-
      share_born_female * birthrate_int * int_mothers *
      (1 - mor_ch_f_0 - emi_int_ch_f_0 * (1 - 2 / 3 * mor_ch_f_0)) +
      share_born_female * birthrate_int * (1 - int_mothers) *
        (acq_int_f_0 * (1 - 2 / 3 * mor_ch_f_0))

    # Foreign females giving birth to foreign males
    int_f_int_m <-
      share_male * birthrate_int * (1 - int_mothers) *
        ((1 - mor_int_m_0) - (emi_int_int_m_0 + acq_int_m_0) *
          (1 - 2 / 3 * mor_int_m_0))

    # Foreign females giving birth to foreign females
    int_f_int_f <-
      share_born_female * birthrate_int * (1 - int_mothers) *
        ((1 - mor_int_f_0) - (emi_int_int_f_0 + acq_int_f_0) *
          (1 - 2 / 3 * mor_int_f_0))

    assertthat::assert_that(
      !any(
        sapply(
          list(
            ch_f_ch_m, int_f_ch_m, ch_f_ch_f, int_f_ch_f, int_f_int_m,
            int_f_int_f
          ),
          function(x) is.null(x) || length(x) == 0
        )
      ),
      msg = "Fertility matrix contains NAs within these vectors: `ch_f_ch_m`,
      `int_f_ch_m`, `ch_f_ch_f`, `int_f_ch_f`, `int_f_int_m`, `int_f_int_f`"
    )

    # Create matrix -----------------------------------------------------------
    Matrix::sparseMatrix(
      i = index_row,
      j = index_column,
      x = c(
        ch_f_ch_m, int_f_ch_m, ch_f_ch_f, int_f_ch_f, int_f_int_m, int_f_int_f
      ),
      dims = c(2 * 2 * n_age_class, 2 * 2 * n_age_class),
      check = TRUE
    )
  }
