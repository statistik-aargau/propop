test_that("create_empty_vector", {
  expect_equal(
    create_empty_vector(0, 3, 2),
    rep(0, 8)
  )
})
