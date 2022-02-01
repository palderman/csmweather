test_that("test lin_interp()", {
  expect_identical(lin_interp(c(1, 2, NA, 4, NA, 6)), as.double(1:6))
})
