
data <- data.table(id=c(1, 1, 1, 1, 1, 2, 2, 2),
                   start=c(0, 10, 25, 812, 1092, 90, 9023, 10000),
                   stop=c(10, 25, 812, 1092, 34334, 8021, 9823, 220022),
                   some_col=c(1, 2, 3, 4, 5, 6, 7, 8))

test_that("general test case", {

  expected <- data.table(id=c(1, 1, 1, 2, 2, 2),
                         start=c(28, 812, 1092, 90, 9023, 10000),
                         stop=c(812, 1092, 15000, 8021, 9823, 15000),
                         some_col=c(3, 4, 5, 6, 7, 8))

  out <- subset_start_stop(data, first_time=28, last_time=15000)

  expect_equal(out, expected)
})

test_that("general test case, no truncation", {

  expected <- data.table(id=c(1, 1, 1, 2, 2, 2),
                         start=c(25, 812, 1092, 90, 9023, 10000),
                         stop=c(812, 1092, 34334, 8021, 9823, 220022),
                         some_col=c(3, 4, 5, 6, 7, 8))

  out <- subset_start_stop(data, first_time=28, last_time=15000,
                           truncate=FALSE)

  expect_equal(out, expected)
})

test_that("using only first_time", {

  expected <- data.table(id=c(1, 1, 1, 2, 2, 2),
                         start=c(28, 812, 1092, 90, 9023, 10000),
                         stop=c(812, 1092, 34334, 8021, 9823, 220022),
                         some_col=c(3, 4, 5, 6, 7, 8))

  out <- subset_start_stop(data, first_time=28)

  expect_equal(out, expected)
})

test_that("using only last_time", {

  expected <- data.table(id=1, start=c(0, 10, 25),
                         stop=c(10, 25, 28), some_col=c(1, 2, 3))

  out <- subset_start_stop(data, last_time=28)

  expect_equal(out, expected)
})

test_that("some individuals may be removed entirely", {

  expected <- data.table(id=2, start=130000, stop=150000, some_col=8)

  out <- subset_start_stop(data, first_time=130000, last_time=150000)

  expect_equal(out, expected)
})

test_that("start / stop having different names", {

  expected <- data.table(id=c(1, 1, 1, 2, 2, 2),
                         beginning=c(28, 812, 1092, 90, 9023, 10000),
                         ending=c(812, 1092, 15000, 8021, 9823, 15000),
                         some_col=c(3, 4, 5, 6, 7, 8))

  setnames(data, old=c("start", "stop"), new=c("beginning", "ending"))
  out <- subset_start_stop(data, first_time=28, 15000,
                           start="beginning", stop="ending")

  expect_equal(out, expected)
})
