
fake_obj <- list(info=list(replace_over_t=FALSE,
                           replace_at_t=FALSE,
                           replace_cases=TRUE,
                           estimand="ATT",
                           ratio=1,
                           match_method="none",
                           match_vars=NULL,
                           n_orig=1000,
                           n_matched=802))
class(fake_obj) <- "MatchTD"

test_that("print.MatchTD, defaults", {
  expect_snapshot_output(print(fake_obj))
})

test_that("print.MatchTD, fast_exact method", {
  fake_obj$info$match_method <- "fast_exact"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.MatchTD, matchit based method", {
  fake_obj$info$match_method <- "nearest"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.MatchTD, replace_over_t", {
  fake_obj$info$replace_over_t <- TRUE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_over_t <- FALSE
})

test_that("print.MatchTD, replace_at_t", {
  fake_obj$info$replace_at_t <- TRUE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_at_t <- FALSE
})

test_that("print.MatchTD, replace_over_t & replace_at_t", {
  fake_obj$info$replace_over_t <- TRUE
  fake_obj$info$replace_at_t <- TRUE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_over_t <- FALSE
  fake_obj$info$replace_at_t <- FALSE
})

test_that("print.MatchTD, replace_cases=FALSE", {
  fake_obj$info$replace_cases <- FALSE
  expect_snapshot_output(print(fake_obj))
  fake_obj$info$replace_cases <- TRUE
})

test_that("print.MatchTD, one covariate", {
  fake_obj$info$match_vars <- "mac"
  expect_snapshot_output(print(fake_obj))
})

test_that("print.MatchTD, multiple covariates", {
  fake_obj$info$match_vars <- c("mac", "A", "B", "C")
  expect_snapshot_output(print(fake_obj))
})
