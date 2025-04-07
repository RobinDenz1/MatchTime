
d_single <- readRDS(system.file("testdata",
                                "single_n1000.Rds",
                                package="MatchTime"))
d_single[, stop := stop + 1]

set.seed(1346)
fake_obj <- match_time(formula=vacc ~ mac,
                       data=d_single,
                       id=".id",
                       inclusion="inclusion",
                       match_method="none")

test_that("print.match_time, defaults", {
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_time, fast_exact method", {
  fake_obj$info$match_method <- "fast_exact"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_time, matchit based method", {
  fake_obj$info$match_method <- "nearest"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_time, replace_over_t", {
  fake_obj$info$replace_over_t <- TRUE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_over_t <- FALSE
})

test_that("print.match_time, replace_at_t", {
  fake_obj$info$replace_at_t <- TRUE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_at_t <- FALSE
})

test_that("print.match_time, replace_over_t & replace_at_t", {
  fake_obj$info$replace_over_t <- TRUE
  fake_obj$info$replace_at_t <- TRUE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_over_t <- FALSE
  fake_obj$info$replace_at_t <- FALSE
})

test_that("print.match_time, replace_cases=FALSE", {
  fake_obj$info$replace_cases <- FALSE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_cases <- TRUE
})

test_that("print.match_time, one covariate", {
  fake_obj$info$match_vars <- "mac"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_time, multiple covariates", {
  fake_obj$info$match_vars <- c("mac", "A", "B", "C")
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_time, method='psm'", {
  fake_obj$info$method <- "psm"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_time, method='pgm'", {
  fake_obj$info$method <- "pgm"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_time, method='dsm'", {
  fake_obj$info$method <- "dsm"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_time, method='greedy'", {
  fake_obj$info$method <- "greedy"
  expect_snapshot_output(print(fake_obj))
})

test_that("checking all matchit() methods", {

  fake_obj$info$match_method <- "exact"
  expect_snapshot_output(print(fake_obj))

  fake_obj$info$match_method <- "cem"
  expect_snapshot_output(print(fake_obj))

  fake_obj$info$match_method <- "optimal"
  expect_snapshot_output(print(fake_obj))

  fake_obj$info$match_method <- "full"
  expect_snapshot_output(print(fake_obj))

  fake_obj$info$match_method <- "quick"
  expect_snapshot_output(print(fake_obj))

  fake_obj$info$match_method <- "genetic"
  expect_snapshot_output(print(fake_obj))

  fake_obj$info$match_method <- "cardinality"
  expect_snapshot_output(print(fake_obj))
})

test_that("not matched on any covariates", {
  fake_obj$info$match_vars <- NULL
  expect_snapshot_output(print(fake_obj))
})

test_that("separate test for get_matchit_method_str()", {
  expect_equal(get_matchit_method_str("exact"),
               "exact matching")
  expect_equal(get_matchit_method_str("cem"),
               "coarsened exact matching")
  expect_equal(get_matchit_method_str("nearest"),
               "nearest neighbor matching")
  expect_equal(get_matchit_method_str("optimal"),
               "optimal pair matching")
  expect_equal(get_matchit_method_str("full"),
               "optimal full matching")
  expect_equal(get_matchit_method_str("quick"),
               "generalized full matching")
  expect_equal(get_matchit_method_str("genetic"),
               "genetic matching")
  expect_equal(get_matchit_method_str("cardinality"),
               "cardinality matching")
})
