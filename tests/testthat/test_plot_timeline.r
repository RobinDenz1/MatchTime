
# example data
data("heart", package="survival")
d_event <- times_from_start_stop(heart, id="id", name="event", type="event")
heart <- heart[, c("id", "start", "stop", "age", "surgery", "transplant")]

set.seed(41231)

out <- suppressWarnings(
  match_time(transplant ~ age + surgery, data=heart, id="id",
                match_method="nearest",
                replace_over_t=TRUE, replace_at_t=FALSE)
)

out <- add_outcome(out, d_event, time="time", censor_at_treat=TRUE,
                   censor_pairs=TRUE)

test_that("defaults", {
  expect_snapshot_output(plot_timeline(out, warn=FALSE))
})

test_that("using a few ids", {
  expect_snapshot_output(plot_timeline(out, include=c(1, 2, 3, 4, 5),
                                       id_type="id"))
})

test_that("using a few .id_pair", {
  expect_snapshot_output(plot_timeline(out, include=c(1, 2, 3),
                                       id_type=".id_pair"))
})

test_that("using a few .id_mew", {
  expect_snapshot_output(plot_timeline(out, include=c(1, 2, 3, 4, 10),
                                       id_type=".id_new"))
})
