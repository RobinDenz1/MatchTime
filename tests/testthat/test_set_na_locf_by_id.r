
test_that("general test case", {

  data <- data.table(ID=c(1, 1, 1, 2, 2, 2, 2, 3, 4, 4),
                     A=c(NA, 1, NA, 2, NA, 13, 3, 4, 10, 10))

  # regular way
  data[, filled_regular := na_locf(A), by=ID]

  # custom technique
  set_na_locf_by_id(data, "A")

  expect_equal(data$A, data$filled_regular)
})
