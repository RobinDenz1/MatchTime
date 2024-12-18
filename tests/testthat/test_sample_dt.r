
set.seed(131243)
data <- data.table(A=stats::rnorm(n=200),
                   strata=sample.int(n=4, size=200, replace=TRUE),
                   id=seq_len(200))

test_that("general test case", {

  set.seed(21341432)
  out <- sample_dt(data, n=100, replace=FALSE, if_lt_n="stop")

  expect_true(nrow(out)==100)
})

test_that("edge case n = 0", {
  set.seed(21341432)
  out <- sample_dt(data, n=0, replace=FALSE, if_lt_n="stop")

  expect_true(is.data.table(out) && nrow(out)==0)
})

test_that("n < nrow(data) with replace=FALSE", {
  set.seed(21341432)

  # with if_lt_n = "stop"
  expect_error(sample_dt(data, n=201, replace=FALSE, if_lt_n="stop"),
               paste0("Cannot sample 201 rows from a data.table with ",
                      "only 200 rows if replace=FALSE."))

  # with if_lt_n = "warn"
  expect_warning(out <- sample_dt(data, n=201, replace=FALSE, if_lt_n="warn"),
                 paste0("Could only sample 200 rows from 'data' instead of ",
                        "201 rows."))

  # with if_lt_n = "nothing"
  out <- sample_dt(data, n=201, replace=FALSE, if_lt_n="nothing")
  expect_true(is.data.table(out) && nrow(out)==200)
})

test_that("n < nrow(data) with replace=FALSE", {
  set.seed(21341432)

  # with if_lt_n = "stop"
  out <- sample_dt(data, n=201, replace=TRUE, if_lt_n="stop")
  expect_true(is.data.table(out) && nrow(out)==201)

  # with if_lt_n = "warn"
  out <- sample_dt(data, n=201, replace=TRUE, if_lt_n="warn")
  expect_true(is.data.table(out) && nrow(out)==201)

  # with if_lt_n = "nothing"
  out <- sample_dt(data, n=201, replace=TRUE, if_lt_n="nothing")
  expect_true(is.data.table(out) && nrow(out)==201)
})

test_that("n > nrow(data), using replace but also using max_replace", {

  set.seed(21341432)
  out <- sample_dt(data, n=300, replace=TRUE, if_lt_n="stop",
                   max_replace=2)

  expect_true(nrow(out)==300)
  expect_true(max(as.vector(table(out$id)))==2)
})

test_that("n > nrow(data), using replace but also using max_replace, errors", {

  set.seed(21341432)

  # with if_lt_n = "stop"
  expect_error(sample_dt(data, n=401, replace=TRUE, if_lt_n="stop",
                         max_replace=2))

  # with if_lt_n = "warn"
  expect_warning(sample_dt(data, n=401, replace=TRUE, if_lt_n="warn",
                           max_replace=2))

  # with if_lt_n = "nothing"
  out <- sample_dt(data, n=412, replace=TRUE, if_lt_n="nothing",
                   max_replace=2)
  expect_true(is.data.table(out) && nrow(out)==400)
  expect_true(max(as.vector(table(out$id)))==2)
})
