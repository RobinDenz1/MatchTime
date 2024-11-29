
test_that("general test case", {
  data <- data.table(id=c(1, 1, 1, 1, 2, 2, 3, 3, 4, 5, 5, 5, 5, 5),
                     start=c(1, 15, 37, 40, 1, 35, 1, 80, 1,
                             1, 15, 37, 40, 88),
                     stop=c(14, 36, 39, 110, 34, 134, 79, 300, 512,
                            14, 36, 39, 87, 127),
                     event=c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                             TRUE, FALSE, FALSE, FALSE, TRUE, TRUE,
                             FALSE, TRUE))
  data[, stop := stop + 1]

  # if "event" refers to a time-varying variable, take the time at which
  # it starts being TRUE
  out <- times_from_start_stop(data=data, name="event", id="id", type="var",
                                time_name=".time")
  expected <- data.table(id=c(1, 1, 2, 3, 5, 5),
                         .time=c(15, 40, 35, 1, 15, 88))
  setkeyv(expected, c("id", ".time"))
  expect_equal(out, expected)

  # if "event" refers to an actual event, if should take the "stop" value
  # (because intervals always end at these values)
  out <- times_from_start_stop(data=data, name="event", id="id", type="event",
                               time_name=".time")
  expected <- data.table(id=c(1, 1, 2, 3, 5, 5, 5),
                         .time=c(37, 111, 135, 80, 37, 40, 128))
  setkeyv(expected, c("id", ".time"))
  expect_equal(out, expected)
})

test_that("no events", {
  data <- data.table(id=c(1, 1, 2, 3, 4),
                     start=c(1, 55, 1, 1, 1),
                     stop=c(54, 135, 425, 2345, 56),
                     event=c(FALSE, FALSE, FALSE, FALSE, FALSE),
                     event2=c(FALSE, TRUE, FALSE, TRUE, FALSE))

  out <- times_from_start_stop(data=data, name="event", id="id", type="var")
  expect_true(is.data.table(out) && nrow(out)==0)
})

test_that("from examples", {

  # define some example start-stop data
  data <- data.table(.id=c(1, 1, 1, 1, 1, 2, 2, 2),
                     start=c(0, 10, 25, 812, 1092, 90, 9023, 10000),
                     stop=c(10, 25, 812, 1092, 34334, 8021, 9823, 220022),
                     exposure=c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
                                TRUE))

  # treating it as an exposure
  expected1 <- data.table(.id=c(1, 1, 2), time=c(0, 812, 10000))
  setkey(expected1, .id, time)
  out1 <- times_from_start_stop(data, id=".id", name="exposure", type="var")
  expect_equal(out1, expected1)

  # treating it as an event
  expected2 <- data.table(.id=c(1, 1, 1, 2), time=c(10, 25, 1092, 220022))
  setkey(expected2, .id, time)
  out2 <- times_from_start_stop(data, id=".id", name="exposure", type="event")
  expect_equal(out2, expected2)
})
