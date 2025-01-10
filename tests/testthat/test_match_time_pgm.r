
data("heart", package="survival")

test_that("general", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="pgm", match_method="nearest",
                      replace_over_t=TRUE, ratio=2, event="event",
                      matchit_args=list(distance="mahalanobis"))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(138, 69))
  expect_true(inherits(m.obj$prog_model, "coxph"))
})

test_that("using formula_prog / remove_prog", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="pgm", match_method="nearest",
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

test_that("different prog_type", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="pgm", match_method="nearest", event="event",
                      replace_over_t=TRUE, ratio=2, prog_type="lp",
                      matchit_args=list(distance="mahalanobis"))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(138, 69))
  expect_equal(round(mean(m.obj$data$.prog_score), 3), -0.095)
})

test_that("usual prog_type + standardizing", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="pgm", match_method="nearest", event="event",
                      replace_over_t=TRUE, ratio=1, prog_type="p",
                      standardize_prog=TRUE,
                      matchit_args=list(distance="mahalanobis"))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(69, 69))
  expect_equal(min(m.obj$data$.prog_score), 0)
  expect_equal(max(m.obj$data$.prog_score), 1)
})

test_that("different prog_type + standardizing", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="pgm", match_method="nearest", event="event",
                      replace_over_t=TRUE, ratio=2, prog_type="lp",
                      standardize_prog=TRUE,
                      matchit_args=list(distance="mahalanobis"))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(138, 69))
  expect_equal(min(m.obj$data$.prog_score), 0)
  expect_equal(max(m.obj$data$.prog_score), 1)
})

test_that("other basehaz_interpol", {

  set.seed(1234)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="pgm", match_method="nearest", event="event",
                      replace_over_t=TRUE, ratio=1, prog_type="p",
                      matchit_args=list(distance="mahalanobis"),
                      basehaz_interpol="linear")
  expect_equal(as.vector(table(m.obj$data$.treat)), c(69, 69))
})
