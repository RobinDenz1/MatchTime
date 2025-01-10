
data("heart", package="survival")

test_that("general", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="psm", match_method="nearest",
                      replace_over_t=TRUE, ratio=2,
                      matchit_args=list(distance="mahalanobis"))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(138, 69))
  expect_true(inherits(m.obj$ps_model, "coxph"))
})

test_that("using formula_ps / remove_ps", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="psm", match_method="nearest",
                      replace_over_t=TRUE,
                      matchit_args=list(distance="mahalanobis"),
                      formula_ps= ~ surgery + age + I(age^2),
                      remove_ps=TRUE)
  expect_equal(as.vector(table(m.obj$data$.treat)), c(69, 69))
  expect_true(inherits(m.obj$ps_model, "coxph"))
  expect_equal(as.character(m.obj$ps_model$formula)[3],
               "surgery + age + I(age^2)")
  expect_true(!".ps_score" %in% colnames(m.obj$data))
})

test_that("different ps_type", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="psm", match_method="nearest",
                      replace_over_t=TRUE, ratio=2, ps_type="lp",
                      matchit_args=list(distance="mahalanobis"))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(138, 69))
  expect_equal(round(mean(m.obj$data$.ps_score), 3), -0.063)
})

test_that("usual ps_type + standardizing", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="psm", match_method="nearest",
                      replace_over_t=TRUE, ratio=1, ps_type="ps",
                      standardize_ps=TRUE,
                      matchit_args=list(distance="mahalanobis"))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(69, 69))
  expect_equal(min(m.obj$data$.ps_score), 0)
  expect_equal(max(m.obj$data$.ps_score), 1)
})

test_that("different ps_type + standardizing", {

  set.seed(123)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="psm", match_method="nearest",
                      replace_over_t=TRUE, ratio=2, ps_type="lp",
                      standardize_ps=TRUE,
                      matchit_args=list(distance="mahalanobis"))
  expect_equal(as.vector(table(m.obj$data$.treat)), c(138, 69))
  expect_equal(min(m.obj$data$.ps_score), 0)
  expect_equal(max(m.obj$data$.ps_score), 1)
})

test_that("other basehaz_interpol", {

  set.seed(1234)

  m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                      method="psm", match_method="nearest",
                      replace_over_t=TRUE, ratio=1, ps_type="ps",
                      matchit_args=list(distance="mahalanobis"),
                      basehaz_interpol="linear")
  expect_equal(as.vector(table(m.obj$data$.treat)), c(69, 69))
})
