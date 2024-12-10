
test_that("general test case", {

  # vector with no NA
  input <- c(1, 2, 3, 1, 1, 0, 3)
  expect_equal(min_custom(input), min(input))

  # vector with one NA
  input <- c(1, 2, 3, 1, 1, NA, 3)
  expect_equal(min_custom(input, na.rm=TRUE), min(input, na.rm=TRUE))

  # vector with only NAs
  input <- c(NA, NA, NA, NA)
  expect_equal(min_custom(input, na.rm=TRUE), NA)
})
