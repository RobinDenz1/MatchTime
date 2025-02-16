
data("heart", package="survival")

test_that("general", {

  set.seed(123)

  m.obj <- match_time(transplant ~ 1, data=heart, id="id",
                      method="greedy")
  expect_equal(as.vector(table(m.obj$data$.treat)), c(1953, 69))
})

test_that("most arguments of match_time() are ignored", {

  set.seed(123)
  m.obj1 <- match_time(transplant ~ 1, data=heart, id="id",
                       method="greedy")

  set.seed(123)
  m.obj2 <- match_time(transplant ~ 1, data=heart, id="id",
                       method="greedy", replace_at_t=TRUE, replace_over_t=TRUE,
                       ratio=10, match_method="nearest",
                       matchit_args=list(distance="mahalanobis"),
                       save_matchit=TRUE)

  m.obj1$info <- NULL
  m.obj1$call <- NULL
  m.obj1$matchit_objects <- NULL

  m.obj2$info <- NULL
  m.obj2$call <- NULL
  m.obj2$matchit_objects <- NULL

  expect_equal(m.obj1, m.obj2)
})
