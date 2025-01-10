
data("heart", package="survival")

test_that("general", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="dsm", match_method="nearest",
                      replace_over_t=TRUE, ratio=2, event="event",
                      matchit_args=list(distance="mahalanobis"))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(138, 69))
  expect_true(inherits(m.obj$prog_model, "coxph"))
  expect_true(inherits(m.obj$ps_model, "coxph"))
})

test_that("using formula_prog / remove_prog", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="dsm", match_method="nearest",
                      replace_over_t=TRUE, event="event",
                      matchit_args=list(distance="mahalanobis"),
                      formula_prog= ~ surgery + age + I(age^2),
                      remove_prog=TRUE)
  expect_equal(as.vector(table(m.obj$data$.treat)), c(69, 69))
  expect_true(inherits(m.obj$prog_model, "coxph"))
  expect_equal(as.character(m.obj$prog_model$formula)[3],
               "surgery + age + I(age^2)")
  expect_true(!".prog_score" %in% colnames(m.obj$data))
})

test_that("using formula_ps / remove_ps", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="dsm", match_method="nearest",
                      replace_over_t=TRUE, event="event",
                      matchit_args=list(distance="mahalanobis"),
                      formula_ps= ~ surgery + age + I(age^2),
                      remove_ps=TRUE)
  expect_equal(as.vector(table(m.obj$data$.treat)), c(69, 69))
  expect_true(inherits(m.obj$ps_model, "coxph"))
  expect_equal(as.character(m.obj$ps_model$formula)[3],
               "surgery + age + I(age^2)")
  expect_true(!".ps_score" %in% colnames(m.obj$data))
})

test_that("using both formula_prog and formula_ps", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="dsm", match_method="nearest",
                      replace_over_t=TRUE, event="event",
                      matchit_args=list(distance="mahalanobis"),
                      formula_prog= ~ surgery + age + I(age^2),
                      formula_ps= ~ age + I(age^2) + I(age^3))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(69, 69))
  expect_equal(as.character(m.obj$prog_model$formula)[3],
               "surgery + age + I(age^2)")
  expect_equal(as.character(m.obj$ps_model$formula)[3],
               "age + I(age^2) + I(age^3)")
})

test_that("different prog_type / ps_type", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="dsm", match_method="nearest", event="event",
                      replace_over_t=TRUE, ratio=2, prog_type="lp",
                      ps_type="lp",
                      matchit_args=list(distance="mahalanobis"))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(138, 69))
  expect_equal(round(mean(m.obj$data$.prog_score), 3), -0.092)
  expect_equal(round(mean(m.obj$data$.ps_score), 3), -0.063)
})
