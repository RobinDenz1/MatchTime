
set.seed(131243)
data <- data.table(A=stats::rnorm(n=200),
                   strata=sample.int(n=4, size=200, replace=TRUE))

test_that("integer strata", {
  set.seed(2341)
  n <- c(10, 20, 30)
  names(n) <- c("1", "3", "2")
  out <- stratified_sample(data, n=n, strata="strata", replace=FALSE)

  expect_true(nrow(out[strata=="1"])==10)
  expect_true(nrow(out[strata=="2"])==30)
  expect_true(nrow(out[strata=="3"])==20)
})

test_that("factor strata", {
  set.seed(2341)

  n <- c(10, 20, 30)
  names(n) <- c("1", "3", "2")

  data[, strata := factor(strata)]
  out <- stratified_sample(data, n=n, strata="strata", replace=FALSE)

  expect_true(nrow(out[strata=="1"])==10)
  expect_true(nrow(out[strata=="2"])==30)
  expect_true(nrow(out[strata=="3"])==20)
})

test_that("character strata", {
  set.seed(2341)

  n <- c(10, 20, 30)
  names(n) <- c("1", "3", "2")

  data[, strata := as.character(strata)]
  out <- stratified_sample(data, n=n, strata="strata", replace=FALSE)

  expect_true(nrow(out[strata=="1"])==10)
  expect_true(nrow(out[strata=="2"])==30)
  expect_true(nrow(out[strata=="3"])==20)
})

test_that("data not a data.table", {
  set.seed(2341)

  n <- c(10, 20, 30)
  names(n) <- c("1", "3", "2")

  data <- as.data.frame(data)

  out <- stratified_sample(data, n=n, strata="strata", replace=FALSE)

  expect_true(nrow(out[strata=="1"])==10)
  expect_true(nrow(out[strata=="2"])==30)
  expect_true(nrow(out[strata=="3"])==20)
})

test_that("strata not in data.table", {

  set.seed(2341)

  n <- c(10, 20, 30)
  names(n) <- c("1", "3", "10")

  # with if_lt_n = "stop"
  expect_error(stratified_sample(data, n=n, strata="strata", replace=FALSE,
                                 if_lt_n="stop"),
               paste0("Cannot sample 30 rows from strata '10' in 'data' ",
                      "because there are no rows in these strata."))

  # with if_lt_n = "warn"
  expect_warning(out <- stratified_sample(data, n=n, strata="strata",
                                          replace=FALSE, if_lt_n="warn"),
                 paste0("Ignoring strata: 10 because there are no rows with ",
                        "such strata in 'data'."))

  # with if_lt_n = "nothing
  out <- stratified_sample(data, n=n, strata="strata",
                           replace=FALSE, if_lt_n="nothing")
  expect_true(nrow(out)==30)
})
