
test_that("general test case", {

  data <- data.table(id=c(1, 1, 1, 2, 2, 3),
                     start=c(0, 28, 66, 25, 343, 10),
                     stop=c(24, 66, 143, 245, 1233, 3214),
                     A=c(10, 122, 3434, 223, 233, 0.46))

  expected <- data.table(id=c(1, 1, 1, 1, 2, 2, 2, 3),
                         start=c(0, 24, 28, 66, 25, 245, 343, 10),
                         stop=c(24, 28, 66, 143, 245, 343, 1233, 3214),
                         .in_data=c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
                                    TRUE, TRUE),
                         A=c(10.00, NA, 122.00, 3434.00, 223.00, NA, 233.00,
                             0.46))
  setkey(expected, id, start)

  out <- fill_gaps_start_stop(data, id="id")
  expect_equal(out, expected)

  # works with non data.table input
  data <- as.data.frame(data)

  out <- fill_gaps_start_stop(data, id="id")
  expect_equal(out, expected)
})

test_that("no intervals added because already full", {

  data <- data.table(id=c(1, 1, 2),
                     start=c(0, 120, 345),
                     stop=c(120, 324, 784),
                     value=c(0.1, 2.4, 0.8))
  setkey(data, id, start)

  out <- fill_gaps_start_stop(data, id="id", missing_indicator=FALSE)
  expect_equal(out, data)
})

test_that("using first_time and last_time", {

  data <- data.table(id=c(1, 1, 2),
                     start=c(0, 120, 345),
                     stop=c(120, 324, 784),
                     value=c(0.1, 2.4, 0.8))

  expected <- data.table(id=c(1, 1, 1, 2, 2, 2),
                         start=c(2, 120, 324, 2, 345, 784),
                         stop=c(120, 324, 1000, 345, 784, 1000),
                         .in_data=c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
                         value=c(0.1, 2.4, NA, NA, 0.8, NA))
  setkey(expected, id, start)

  out <- fill_gaps_start_stop(data, id="id", first_time=2, last_time=1000)
  expect_equal(out, expected)
})

test_that("using different names for start / stop", {

  data <- data.table(id=c(1, 1, 1, 2, 2, 3),
                     begin=c(0, 28, 66, 25, 343, 10),
                     end=c(24, 66, 143, 245, 1233, 3214),
                     A=c(10, 122, 3434, 223, 233, 0.46))

  expected <- data.table(id=c(1, 1, 1, 1, 2, 2, 2, 3),
                         begin=c(0, 24, 28, 66, 25, 245, 343, 10),
                         end=c(24, 28, 66, 143, 245, 343, 1233, 3214),
                         .in_data=c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
                                    TRUE, TRUE),
                         A=c(10.00, NA, 122.00, 3434.00, 223.00, NA, 233.00,
                             0.46))
  setkey(expected, id, begin)

  out <- fill_gaps_start_stop(data, id="id", start="begin", stop="end")

  expect_equal(out, expected)
})
