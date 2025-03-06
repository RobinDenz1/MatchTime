
set.seed(1234)

data("heart", package="survival")

# add some random variables that will be used as inclusion criteria
# (in reality you would already have them)
heart$A <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.95, 0.05))
heart$B <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.999, 0.001))
heart$C <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.6, 0.4))

# perform some time-dependent matching with inclusion criteria
out <- match_time(transplant ~ surgery, data=heart, id="id",
                  match_method="fast_exact", method="brsm",
                  replace_over_t=TRUE, ratio=1,
                  inclusion=c("A", "B", "C"))

test_that("general test", {
  expect_snapshot_output(plot_flowchart(out))
})

test_that("using digits", {
  expect_snapshot_output(plot_flowchart(out, digits=3))
})

test_that("normal n = X", {
  expect_snapshot_output(plot_flowchart(out, n_fontface="normal"))
})

test_that("bold n = X", {
  expect_snapshot_output(plot_flowchart(out, n_fontface="bold"))
})

test_that("do not remove 0 lines", {
  expect_snapshot_output(plot_flowchart(out, remove_0_lines=FALSE))
})
