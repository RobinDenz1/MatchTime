
d_single <- readRDS(system.file("testdata",
                                "single_n1000.Rds",
                                package="MatchTD"))
d_single[, stop := stop + 1]

set.seed(1346)
fake_obj <- match_td(formula=vacc ~ mac,
                     data=d_single,
                     id=".id",
                     inclusion="inclusion",
                     match_method="none")

test_that("print.match_td, defaults", {
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_td, fast_exact method", {
  fake_obj$info$match_method <- "fast_exact"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_td, matchit based method", {
  fake_obj$info$match_method <- "nearest"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_td, replace_over_t", {
  fake_obj$info$replace_over_t <- TRUE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_over_t <- FALSE
})

test_that("print.match_td, replace_at_t", {
  fake_obj$info$replace_at_t <- TRUE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_at_t <- FALSE
})

test_that("print.match_td, replace_over_t & replace_at_t", {
  fake_obj$info$replace_over_t <- TRUE
  fake_obj$info$replace_at_t <- TRUE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_over_t <- FALSE
  fake_obj$info$replace_at_t <- FALSE
})

test_that("print.match_td, replace_cases=FALSE", {
  fake_obj$info$replace_cases <- FALSE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_cases <- TRUE
})

test_that("print.match_td, one covariate", {
  fake_obj$info$match_vars <- "mac"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.match_td, multiple covariates", {
  fake_obj$info$match_vars <- c("mac", "A", "B", "C")
  expect_snapshot_output(print(fake_obj))
})
