
# create input object, resembling output of match_time()
data <- data.table(id=c(1, 2, 3, 4, 5, 2),
                   .id_new=c(1, 2, 3, 4, 5, 6),
                   .id_pair=c(1, 1, 2, 2, 3, 3),
                   .treat_time=c(28, 28, 2, 2, 4, 4),
                   .treat=c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE),
                   .next_treat_time=c(NA, NA, 17, NA, NA, 28),
                   A=c("A", "B", "A", "B", "B", "A"))
d_longest <- data.table(id=c(1, 2, 3, 4, 5),
                        .max_t=c(101, 204, 1011, 541, 891))
obj <- list(data=data, id="id", time="time", d_longest=d_longest)
class(obj) <- "match_time"

# event times
d_event <- data.table(id=c(1, 2, 3, 4, 4),
                      time=c(98, 48, 15, 401, 438))

test_that("censor_at_treat=FALSE", {

  expected <- data.table(id=c(1, 2, 2, 3, 4, 5),
                         .id_new=c(1, 2, 6, 3, 4, 5),
                         .id_pair=c(1, 1, 3, 2, 2, 3),
                         .treat_time=c(28, 28, 4, 2, 2, 4),
                         .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                         .next_treat_time=c(NA, NA, 28, 17, NA, NA),
                         A=c("A", "B", "A", "A", "B", "B"),
                         .status=c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
                         .event_time=c(70, 20, 44, 13, 399, 887))
  setkey(expected, id)

  out <- add_outcome(x=obj, data=d_event, censor_at_treat=FALSE,
                     censor_pairs=FALSE)$data

  expect_equal(out, expected)
})

test_that("events after the maximum follow-up time are censored", {

  d_event2 <- data.table(id=c(1, 2, 3, 4, 4, 5),
                         time=c(98, 48, 15, 401, 438, 900))

  expected <- data.table(id=c(1, 2, 2, 3, 4, 5),
                         .id_new=c(1, 2, 6, 3, 4, 5),
                         .id_pair=c(1, 1, 3, 2, 2, 3),
                         .treat_time=c(28, 28, 4, 2, 2, 4),
                         .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                         .next_treat_time=c(NA, NA, 28, 17, NA, NA),
                         A=c("A", "B", "A", "A", "B", "B"),
                         .status=c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
                         .event_time=c(70, 20, 44, 13, 399, 887))
  setkey(expected, id)

  out <- add_outcome(x=obj, data=d_event, censor_at_treat=FALSE,
                     censor_pairs=FALSE)$data

  expect_equal(out, expected)
})

test_that("censor_at_treat=TRUE without censor_pairs", {

  expected <- data.table(id=c(1, 2, 2, 3, 4, 5),
                         .id_new=c(1, 2, 6, 3, 4, 5),
                         .id_pair=c(1, 1, 3, 2, 2, 3),
                         .treat_time=c(28, 28, 4, 2, 2, 4),
                         .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                         .next_treat_time=c(NA, NA, 28, 17, NA, NA),
                         A=c("A", "B", "A", "A", "B", "B"),
                         .status=c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
                         .event_time=c(70, 20, 24, 13, 399, 887))
  setkey(expected, id)

  out <- add_outcome(x=obj, data=as.data.frame(d_event), censor_at_treat=TRUE,
                     censor_pairs=FALSE)$data

  expect_equal(out, expected)
})

test_that("censor_at_treat=TRUE with censor_pairs", {

  # in this example, only id = 5 gets censored, which was already censored
  # (but the .event_time change)
  # id = 4 does not get censored, even though its pair (id = 3) would have been
  # censored at t = 15, but since he has an event on that same day is not
  # considered censored
  expected <- data.table(id=c(1, 2, 2, 3, 4, 5),
                         .id_new=c(1, 2, 6, 3, 4, 5),
                         .id_pair=c(1, 1, 3, 2, 2, 3),
                         .treat_time=c(28, 28, 4, 2, 2, 4),
                         .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                         .next_treat_time=c(NA, NA, 28, 17, NA, NA),
                         A=c("A", "B", "A", "A", "B", "B"),
                         .status=c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
                         .event_time=c(70, 20, 24, 13, 399, 24))
  setkey(expected, id)

  out <- add_outcome(x=obj, data=d_event, censor_at_treat=TRUE,
                     censor_pairs=TRUE)$data
  expect_equal(out, expected)

  # here we run this again, changing the input slightly:
  # - id = 3 is now actually censored before the event (thus id = 4 as well)
  # - id = 5 has an event that requires the status changing
  # create input object, resembling output of match_time()
  data2 <- data.table(id=c(1, 2, 3, 4, 5, 2),
                     .id_new=c(1, 2, 3, 4, 5, 6),
                     .id_pair=c(1, 1, 2, 2, 3, 3),
                     .treat_time=c(28, 28, 2, 2, 4, 4),
                     .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                     .next_treat_time=c(NA, NA, 17, NA, NA, 28),
                     A=c("A", "B", "A", "B", "B", "A"))
  d_longest2 <- data.table(id=c(1, 2, 3, 4, 5),
                          .max_t=c(101, 204, 1011, 541, 891))
  obj2 <- list(data=data2, id="id", time="time", d_longest=d_longest2)
  class(obj2) <- "match_time"

  # event times
  d_event2 <- data.table(id=c(1, 2, 3, 4, 4, 5),
                         time=c(98, 48, 16, 401, 438, 90))

  expected2 <- data.table(id=c(1, 2, 2, 3, 4, 5),
                          .id_new=c(1, 2, 6, 3, 4, 5),
                          .id_pair=c(1, 1, 3, 2, 2, 3),
                          .treat_time=c(28, 28, 4, 2, 2, 4),
                          .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                          .next_treat_time=c(NA, NA, 28, 17, NA, NA),
                          A=c("A", "B", "A", "A", "B", "B"),
                          .status=c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
                          .event_time=c(70, 20, 24, 13, 13, 24))
  setkey(expected, id)

  out2 <- add_outcome(x=obj2, data=d_event2, censor_at_treat=TRUE,
                      censor_pairs=TRUE)$data
  expect_equal(out, expected)
})

test_that("works with different names", {

  expected <- data.table(id=c(1, 2, 2, 3, 4, 5),
                         .id_new=c(1, 2, 6, 3, 4, 5),
                         .id_pair=c(1, 1, 3, 2, 2, 3),
                         .treat_time=c(28, 28, 4, 2, 2, 4),
                         .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                         .next_treat_time=c(NA, NA, 28, 17, NA, NA),
                         A=c("A", "B", "A", "A", "B", "B"),
                         status=c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
                         time_to_event=c(70, 20, 24, 13, 399, 887))
  setkey(expected, id)

  d_event3 <- copy(d_event)
  setnames(d_event3, old=c("id", "time"), new=c("ID_Pers", "some_time"))

  out <- add_outcome(x=obj, data=d_event3, censor_at_treat=TRUE,
                     censor_pairs=FALSE, status_name="status",
                     event_time_name="time_to_event",
                     id="ID_Pers", time="some_time")$data

  expect_equal(out, expected)
})

test_that("censor_at_treat=TRUE without censor_pairs, date input", {

  obj$data[, .treat_time := as.Date(.treat_time, origin=as.Date("01-01-2020"))]
  obj$data[, .next_treat_time := as.Date(.next_treat_time,
                                         origin=as.Date("01-01-2020"))]
  obj$d_longest[, .max_t := as.Date(.max_t, origin=as.Date("01-01-2020"))]
  d_event[, time := as.Date(time, origin=as.Date("01-01-2020"))]

  expected <- data.table(id=c(1, 2, 2, 3, 4, 5),
                         .id_new=c(1, 2, 6, 3, 4, 5),
                         .id_pair=c(1, 1, 3, 2, 2, 3),
                         .treat_time=c(28, 28, 4, 2, 2, 4),
                         .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                         .next_treat_time=c(NA, NA, 28, 17, NA, NA),
                         A=c("A", "B", "A", "A", "B", "B"),
                         .status=c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
                         .event_time=c(70, 20, 24, 13, 399, 887))
  setkey(expected, id)
  expected[, .treat_time := as.Date(.treat_time, origin=as.Date("01-01-2020"))]
  expected[, .next_treat_time := as.Date(.next_treat_time,
                                         origin=as.Date("01-01-2020"))]

  out <- add_outcome(x=obj, data=d_event, censor_at_treat=TRUE,
                     censor_pairs=FALSE)$data

  expect_equal(out, expected)
})

test_that("censor_at_treat=TRUE with censor_pairs, date input", {

  expected <- data.table(id=c(1, 2, 2, 3, 4, 5),
                         .id_new=c(1, 2, 6, 3, 4, 5),
                         .id_pair=c(1, 1, 3, 2, 2, 3),
                         .treat_time=c(28, 28, 4, 2, 2, 4),
                         .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                         .next_treat_time=c(NA, NA, 28, 17, NA, NA),
                         A=c("A", "B", "A", "A", "B", "B"),
                         .status=c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
                         .event_time=c(70, 20, 24, 13, 399, 24))
  setkey(expected, id)

  expected[, .treat_time := as.Date(.treat_time, origin=as.Date("01-01-2020"))]
  expected[, .next_treat_time := as.Date(.next_treat_time,
                                         origin=as.Date("01-01-2020"))]

  out <- add_outcome(x=obj, data=d_event, censor_at_treat=TRUE,
                     censor_pairs=TRUE)$data
  expect_equal(out, expected)
})
