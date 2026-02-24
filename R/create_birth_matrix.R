#' Create birth matrix (helper function for project_raw)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @description The birth matrix is used to calculate age-specific annual births
#' by demographic group. The result is integrated into the output data frame
#' named as column `Births`.
#'
#' @param fert_first numeric, start of the fertile age of females.
#' @param fert_last numeric, end of the fertile age of females.
#' @param fert_length numeric, duration of female fertility.
#' @param n_age_class numeric, number of age groups (e.g., 101).
#' @param share_born_female numeric, share of female newborns
#' relative to all newborns.
#' @param birthrate_ch numeric,birth rate among Swiss women.
#' @param birthrate_int numeric,birth rate among foreign women.
#' @param int_mothers numeric, proportion of children with Swiss nationality
#' born to non-Swiss mothers.
#'
#' @noRd

create_birth_matrix <-
  function(fert_first,
           fert_last,
           fert_length,
           n_age_class,
           share_born_female,
           birthrate_ch,
           birthrate_int,
           int_mothers) {
    # Deprecate
    lifecycle::deprecate_warn(
      "2.0.0", "create_birth_matrix()",
      details = paste0(
        "`create_birth_matrix()` is still operational as part of `propop_legacy()` but ",
        "won't be further maintained"
      )
    )
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


    # Calculate shares --------------------------------------------------------
    # share males
    share_born_male <- 1 - share_born_female
    # share of international mothers' newborns with foreign citizenship
    births_int_int <- 1 - int_mothers

    assertthat::assert_that(
      !any(
        sapply(
          list(index_row, index_column),
          function(x) is.null(x) || length(x) == 0
        )
      ),
      msg = paste0(
        "Birth matrix contains NAs within indices: `index_row`,",
        " `index_column`."
      )
    )

    assertthat::assert_that(
      !any(
        sapply(
          list(n_age_class, share_born_male, int_mothers),
          function(x) is.null(x) || length(x) == 0
        )
      ),
      msg = paste0(
        "Birth matrix contains NAs within these parameters:",
        " `n_age_class`, `share_born_male`, `births_int_ch`."
      )
    )

    # Create vectors ----------------------------------------------------------
    # Swiss females giving birth to Swiss males
    ch_f_ch_m <- share_born_male * birthrate_ch

    # Foreign females giving birth to Swiss males
    int_f_ch_m <- share_born_male * birthrate_int * int_mothers

    # Swiss females giving birth to Swiss females
    ch_f_ch_f <- share_born_female * birthrate_ch

    # Foreign females giving birth to Swiss females
    int_f_ch_f <- share_born_female * birthrate_int * int_mothers

    # Foreign females giving birth to foreign males
    int_f_int_m <- share_born_male * birthrate_int * births_int_int

    # Foreign females giving birth to foreign females
    int_f_int_f <- share_born_female * birthrate_int * births_int_int

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
      msg = "Birth matrix contains NAs within these vectors: `ch_f_ch_m`,
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
