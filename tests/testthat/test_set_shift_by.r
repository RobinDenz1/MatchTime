
test_that("using type='lag', no fill", {

  data <- data.table(some_id=c(1, 1, 1, 2, 2, 4, 7, 7, 7),
                     some_column=c(10, 12, 3, 5, 6, 33, 1, 2, 3))

  # regular syntax
  data[, shifted_regular := shift(some_column, type="lag", n=1), by=some_id]

  # custom function
  set_shift_by(data=data, col_in="some_column", col_out="shifted_custom",
               by="some_id", type="lag")

  expect_equal(data$shifted_regular, data$shifted_custom)
})

test_that("using type='lag', with fill", {

  data <- data.table(some_id=c(1, 1, 1, 2, 2, 4, 7, 7, 7),
                     some_column=c(10, 12, 3, 5, 6, 33, 1, 2, 3))

  # regular syntax
  data[, shifted_regular := shift(some_column, type="lag", n=1, fill=10),
       by=some_id]

  # custom function
  set_shift_by(data=data, col_in="some_column", col_out="shifted_custom",
               by="some_id", type="lag", fill=10)

  expect_equal(data$shifted_regular, data$shifted_custom)
})

test_that("using type='lead', no fill", {

  data <- data.table(some_id=c(1, 1, 1, 2, 2, 4, 7, 7, 7),
                     some_column=c(10, 12, 3, 5, 6, 33, 1, 2, 3))

  # regular syntax
  data[, shifted_regular := shift(some_column, type="lead", n=1),
       by=some_id]

  # custom function
  set_shift_by(data=data, col_in="some_column", col_out="shifted_custom",
               by="some_id", type="lead")

  expect_equal(data$shifted_regular, data$shifted_custom)
})

test_that("using type='lead', with fill", {

  data <- data.table(some_id=c(1, 1, 1, 2, 2, 4, 7, 7, 7),
                     some_column=c(10, 12, 3, 5, 6, 33, 1, 2, 3))

  # regular syntax
  data[, shifted_regular := shift(some_column, type="lead", n=1, fill=-0.2),
       by=some_id]

  # custom function
  set_shift_by(data=data, col_in="some_column", col_out="shifted_custom",
               by="some_id", type="lead", fill=-0.2)

  expect_equal(data$shifted_regular, data$shifted_custom)
})
