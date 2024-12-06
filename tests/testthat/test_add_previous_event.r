
# create input object, resembling output of match_td()
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
class(obj) <- "match_td"

# event times
d_event <- data.table(id=c(1, 2, 3, 4, 4),
                      time=c(18, 3, 15, 2, 438))

test_that("with include_same_t=FALSE", {

  expected <- data.table(id=c(1, 2, 2, 3, 4, 5),
                         .id_new=c(1, 2, 6, 3, 4, 5),
                         .id_pair=c(1, 1, 3, 2, 2, 3),
                         .treat_time=c(28, 28, 4, 2, 2, 4),
                         .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                         .next_treat_time=c(NA, NA, 28, 17, NA, NA),
                         A=c("A", "B", "A", "A", "B", "B"),
                         .prev_event=c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))
  setkey(expected, id)

  # with a duration of 20
  out <- add_previous_event(x=obj, data=d_event, duration=20,
                            include_same_t=FALSE)$data
  expect_equal(out, expected)

  # upping the duration to 400
  out2 <- add_previous_event(x=obj, data=d_event, duration=400,
                             include_same_t=FALSE)$data
  expected$.prev_event <- c(rep(TRUE, 3), rep(FALSE, 3))
  expect_equal(out2, expected)
})

test_that("with include_same_t=TRUE", {

  expected <- data.table(id=c(1, 2, 2, 3, 4, 5),
                         .id_new=c(1, 2, 6, 3, 4, 5),
                         .id_pair=c(1, 1, 3, 2, 2, 3),
                         .treat_time=c(28, 28, 4, 2, 2, 4),
                         .treat=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                         .next_treat_time=c(NA, NA, 28, 17, NA, NA),
                         A=c("A", "B", "A", "A", "B", "B"),
                         .prev_event=c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
  setkey(expected, id)

  # with a duration of 20
  out <- add_previous_event(x=obj, data=d_event, duration=20,
                            include_same_t=TRUE)$data
  expect_equal(out, expected)

  # upping the duration to 400
  out2 <- add_previous_event(x=obj, data=d_event, duration=400,
                             include_same_t=TRUE)$data
  expected$.prev_event <- c(rep(TRUE, 3), FALSE, TRUE, FALSE)
  expect_equal(out2, expected)
})
