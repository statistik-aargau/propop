# Dummy data in which the population equation balances ----
balanced <- tibble::tibble(
  year      = c(2025, 2025, 2026),
  n_jan     = c(1000, 500, 1010),
  births    = c(10, 5, 11),
  mor_n     = c(8, 4, 9),
  emi_int_n = c(3, 1, 3),
  emi_nat_n = c(5, 2, 5),
  imm_int_n = c(6, 3, 6),
  imm_nat_n = c(10, 4, 10)
) |>
  dplyr::mutate(
    n_dec = n_jan + births - mor_n - emi_int_n - emi_nat_n + imm_int_n +
      imm_nat_n
  )

# Balanced data ----
test_that("check_balance() passes when all components add up", {
  res <- suppressMessages(check_balance(balanced))
  expect_equal(res$rows, 3)
  expect_equal(res$nonzeros, 0)
  expect_equal(res$missings, 0)
  result <- check_balance(balanced)
  expect_snapshot(print(as.data.frame(result)))
})

# Unbalanced data ----
test_that("check_balance() detects rows that do not add up", {
  # Dummy data with an unbalanced population equation
  unbalanced <- balanced |>
    dplyr::mutate(
      n_dec = dplyr::case_when(
        dplyr::row_number() == 2 ~ n_dec + 10,
        .default = n_dec
      )
    )

  res <- suppressMessages(check_balance(unbalanced))
  expect_equal(res$rows, 3)
  expect_equal(res$nonzeros, 1)
  expect_equal(res$missings, 0)

  msgs <- paste(testthat::capture_messages(check_balance(unbalanced)), collapse = "")
  expect_match(msgs, "Check failed")
  expect_no_match(msgs, "Check passed")
})

# Missing values ----
test_that("check_balance() reports missing values separately", {
  # Dummy data with a missing value
  missing <- balanced |>
    dplyr::mutate(
      births = dplyr::case_when(
        dplyr::row_number() == 3 ~ NA_real_,
        .default = births
      )
    )

  res <- suppressMessages(check_balance(missing))
  expect_equal(res$rows, 3)
  expect_equal(res$missings, 1)
  expect_equal(res$nonzeros, 0)

  msgs <- paste(testthat::capture_messages(check_balance(missing)), collapse = "")
  expect_match(msgs, "Check failed")
})

# Rounding ----
test_that("check_balance() tolerates differences that round to zero", {
  # Dummy data with value rounding up to zero difference
  unrounded <- balanced |>
    dplyr::mutate(
      n_dec = dplyr::case_when(
        dplyr::row_number() == 1 ~ n_dec + 0.4,
        .default = n_dec
      )
    )

  res <- suppressMessages(check_balance(unrounded))
  expect_equal(res$nonzeros, 0)

  # Testing dummy data with value not rounding up to zero difference
  res <- suppressMessages(check_balance(
    unrounded |>
      dplyr::mutate(
        n_dec = dplyr::case_when(
          dplyr::row_number() == 2 ~ n_dec + 0.7,
          .default = n_dec
        )
      )
  ))
  expect_equal(res$nonzeros, 1)
})

# Optional columns ----
test_that("check_balance() includes `acq_n` when present", {
  # Dummy data with acquisitions
  with_acq <- balanced |>
    mutate(acq_n = 2)

  # `n_dec` does not yet include `acq_n`, so the check should fail
  res <- suppressMessages(check_balance(with_acq))
  expect_equal(res$nonzeros, 3)

  # once `n_dec` includes acquisitions, it should pass
  with_acq <- with_acq |>
    mutate(n_dec = n_dec + acq_n)
  res <- suppressMessages(check_balance(with_acq))
  expect_equal(res$nonzeros, 0)
})

test_that("check_balance() includes `mig_sub` when present", {
  # Dummy data with mig_sub
  with_mig_sub <- balanced |>
    mutate(mig_sub = 2)

  # `n_dec` does not yet include `mig_sub`, so the check should fail
  res <- suppressMessages(check_balance(with_mig_sub))
  expect_equal(res$nonzeros, 3)

  # once `n_dec` includes `mig_sub`, it should pass
  with_mig_sub <- with_mig_sub |>
    mutate(n_dec = n_dec + mig_sub)
  res <- suppressMessages(check_balance(with_mig_sub))
  expect_equal(res$nonzeros, 0)
})

test_that("check_balance() includes `emi_sub_n` and `imm_sub_n` when present", {
  # Both, `emi_sub_n` and `imm_sub_n` must be present in the data
  # Dummy data with only `emi_sub_n`
  with_emi_sub_n <- balanced |>
    mutate(emi_sub_n = 5)
  expect_error(check_balance(with_emi_sub_n))

  # Dummy data with only `imm_sub_n`
  with_imm_sub_n <- balanced |>
    mutate(imm_sub_n = 5)
  expect_error(check_balance(with_imm_sub_n))

  # Dummy data with `emi_sub_n` and `imm_sub_n`
  with_imm_sub <- balanced |>
    mutate(emi_sub_n = 3, imm_sub_n = 5)

  # `n_dec` does not yet include `emi_sub_n` and `imm_sub_n`, so the check
  # should fail
  res <- suppressMessages(check_balance(with_imm_sub))
  expect_equal(res$nonzeros, 3)

  # once `n_dec` includes `emi_sub_n` and `imm_sub_n`, it should pass
  with_imm_sub <- with_imm_sub |>
    mutate(n_dec = n_dec - emi_sub_n + imm_sub_n)
  res <- suppressMessages(check_balance(with_imm_sub))
  expect_equal(res$nonzeros, 0)
})

test_that("check_balance() handles optional columns together", {
  # `acq_n` and `mig_sub` together
  balanced_combined1 <- balanced |>
    mutate(
      acq_n = 2,
      mig_sub = 3,
      n_dec = n_jan + births - mor_n - emi_int_n - emi_nat_n + imm_int_n +
        imm_nat_n + acq_n + mig_sub
    )
  res <- suppressMessages(check_balance(balanced_combined1))
  expect_equal(res$nonzeros, 0)
  expect_equal(res$missings, 0)

  # `acq_n`, `emi_sub_n` and `imm_sub_n` together
  balanced_combined2 <- balanced |>
    mutate(
      acq_n = 2,
      emi_sub_n = 3,
      imm_sub_n = 5,
      n_dec = n_jan + births - mor_n - emi_int_n - emi_nat_n + imm_int_n +
        imm_nat_n + acq_n - emi_sub_n +imm_sub_n
    )
  res <- suppressMessages(check_balance(balanced_combined2))
  expect_equal(res$nonzeros, 0)
  expect_equal(res$missings, 0)
})

# Invalid input ----
test_that("check_balance() errors when required columns are missing", {
  expect_error(suppressMessages(
    check_balance(balanced |> dplyr::select(-births))
  ))
})

# Integration with propop() ----
test_that("check_balance() passes on propop() output", {
  skip_on_cran()

  out <- propop(
    parameters = fso_parameters,
    year_first = 2026,
    year_last = 2027,
    population = fso_population,
    subregional = FALSE,
    binational = TRUE
  )

  res <- suppressMessages(check_balance(out))
  expect_gt(res$rows, 0)
  expect_equal(res$nonzeros, 0)
  expect_equal(res$missings, 0)
})
