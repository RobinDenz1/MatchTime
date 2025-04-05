
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

test_that("wrong time_name", {
  expect_error(plot_timeline(out, time_name=1),
    paste0("'time_name' must specify an event-time in 'x' specified ",
           "using the 'outcomes' argument in match_time() or ",
           "added using add_outcome()."),
    fixed=TRUE)
})

test_that("wrong status_name", {
  expect_error(plot_timeline(out, status_name=1),
               paste0("'status_name' must specify an event-status in",
                      " 'x' specified ",
                      "using the 'outcomes' argument in match_time() or ",
                      "added using add_outcome()."),
               fixed=TRUE)
})

test_that("warning with missing include", {
  expect_warning(plot_timeline(out),
                 paste0("'include' is not specified. The entire dataset ",
                        "will be plotted, which might be difficult to ",
                        "display correctly. Set 'warn=FALSE' to ",
                        "silence this warning."))
})

test_that("cannot use .id_pair with some match_methods", {
  out2 <- copy(out)
  out2$data$.id_pair <- NULL
  expect_error(plot_timeline(out2, id_type=".id_pair", include=c(1, 2)),
               paste0("Cannot use id_type='.id_pair' if a 'match_method' ",
                      "was used in the original match_time() call that",
                      " does not create an '.id_pair' column."),
               fixed=TRUE)
})

test_that("wrong id_type", {
  expect_error(plot_timeline(out, id_type="custom", include=1),
               "'id_type' must be one of x$id, '.id_new' or '.id_pair'.",
               fixed=TRUE)
})

test_that("multiple events", {
  out2 <- copy(out)
  out2$info$added_event_times <- c(out2$info$added_event_times, "second")
  expect_error(plot_timeline(out2, warn=FALSE),
               paste0("Arguments 'time_name' and 'status_name' must be ",
                      "specified when more than one event is present in 'x'."))
})

test_that("no events to use", {
  out2 <- copy(out)
  out2$info$added_event_times <- c()
  expect_error(plot_timeline(out2, warn=FALSE),
               paste0("Timelines can only be plotted if at least one event",
                      " was specified in the 'outcomes' argument of ",
                      "match_time() or added to the match_time object ",
                      "using the add_outcome() function."), fixed=TRUE)
})
