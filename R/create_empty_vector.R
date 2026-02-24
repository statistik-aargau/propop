#' Create empty vector (helper function for project_raw)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @description Used to create placeholder vector.
#'
#' @param empty_val 0/NA, empty values (0 for children aged zero years /
#' NA for people aged 1-100)
#' @param proj_length integer, projection length (start_year - end_year)
#' @param length_pop_vec integer, length of the population vector
#' (age groups * nat * sex)
#'
#' @noRd
create_empty_vector <-
  function(empty_val,
           proj_length,
           length_pop_vec) {
    # Deprecate
    lifecycle::deprecate_warn(
      "2.0.0", "create_empty_vector()",
      details = paste0(
        "`create_empty_vector()` is still operational as part of `propop_legacy()` but ",
        "won't be further maintained"
      )
    )
    rep(empty_val, (proj_length + 1) * length_pop_vec)
  }
