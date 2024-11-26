
test_that("general test case", {

  long <- data.table(.id=rep(seq_len(10), each=5),
                     .simulation_time=rep(seq_len(5), 10),
                     A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                             TRUE),
                     B=FALSE)
  setkey(long, .id, .simulation_time)

  expected <- data.table(.id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10),
                         start=c(rep(1, 9), 4, 1, 4),
                         stop=c(rep(6, 8), 4, 6, 4, 6),
                         A=c(rep(FALSE, 9), TRUE, FALSE, TRUE),
                         B=FALSE)

  out_dat <- long2start_stop(data=long, id=".id", time=".simulation_time",
                             varying=c("A", "B"))

  expect_equal(out_dat, expected)
})

test_that("shuffled input", {

  long <- data.table(.id=rep(seq_len(10), each=5),
                     .simulation_time=rep(seq_len(5), 10),
                     A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                         TRUE),
                     B=FALSE)
  long <- long[sample(nrow(long), replace=FALSE),]

  expected <- data.table(.id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10),
                         start=c(rep(1, 9), 4, 1, 4),
                         stop=c(rep(6, 8), 4, 6, 4, 6),
                         A=c(rep(FALSE, 9), TRUE, FALSE, TRUE),
                         B=FALSE)

  out_dat <- long2start_stop(data=long, id=".id", time=".simulation_time",
                             varying=c("A", "B"))

  expect_equal(out_dat, expected)
})

test_that("variable change right at the end", {

  long <- data.table(.id=rep(seq_len(10), each=5),
                     .simulation_time=rep(seq_len(5), 10),
                     A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                         TRUE),
                     B=c(rep(FALSE, 49), TRUE))
  setkey(long, .id, .simulation_time)

  expected <- data.table(.id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10, 10),
                         start=c(rep(1, 9), 4, 1, 4, 5),
                         stop=c(rep(6, 8), 4, 6, 4, 5, 6),
                         A=c(rep(FALSE, 9), TRUE, FALSE, TRUE, TRUE),
                         B=c(rep(FALSE, 12), TRUE))

  out_dat <- long2start_stop(data=long, id=".id", time=".simulation_time",
                             varying=c("A", "B"))

  expect_equal(out_dat, expected)
})

test_that("non-logical time-varying variables", {

  long <- data.table(.id=rep(seq_len(10), each=5),
                     .simulation_time=rep(seq_len(5), 10),
                     A=c(rep(13.2, 43), 14.75, 14.75, rep(18, 3), 27.1,
                         53),
                     B=14.3)
  setkey(long, .id, .simulation_time)

  expected <- data.table(.id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10, 10),
                         start=c(rep(1, 9), 4, 1, 4, 5),
                         stop=c(rep(6, 8), 4, 6, 4, 5, 6),
                         A=c(rep(13.2, 9), 14.75, 18, 27.1, 53),
                         B=14.3)

  out_dat <- long2start_stop(data=long, id=".id", time=".simulation_time",
                             varying=c("A", "B"))

  expect_equal(out_dat, expected)
})

test_that("using internal / changed variable names", {

  long <- data.table(id=rep(seq_len(10), each=5),
                     time=rep(seq_len(5), 10),
                     A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                         TRUE),
                     B=FALSE)
  setkey(long, id, time)

  expected <- data.table(id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10),
                         begin=c(rep(1, 9), 4, 1, 4),
                         end=c(rep(6, 8), 4, 6, 4, 6),
                         A=c(rep(FALSE, 9), TRUE, FALSE, TRUE),
                         B=FALSE)

  out_dat <- long2start_stop(data=long, id="id", time="time",
                             varying=c("A", "B"), start_name="begin",
                             stop_name="end")

  expect_equal(out_dat, expected)
})

test_that("different number/starts of intervals over ids", {

  long <- data.table(id=c(1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3),
                     time=c(10, 11, 12, 13, 0, 1, 1, 2, 3, 4, 5, 6),
                     var1=c("a", "b", "b", "c", "a", "a", "a", "a", "f",
                            "f", "f", "f"),
                     var2=FALSE)
  setkey(long, id, time)

  expected <- data.table(id=c(1, 1, 1, 2, 3, 3),
                         start=c(10, 11, 13, 0, 1, 3),
                         stop=c(11, 13, 14, 2, 3, 7),
                         var1=c("a", "b", "c", "a", "a", "f"),
                         var2=FALSE)

  out_dat <- long2start_stop(data=long, id="id", time="time",
                             varying=c("var1", "var2"))

  expect_equal(out_dat, expected)
})

test_that("wrong data", {
  expect_error(long2start_stop(data="1", id=".id", time=".time"))
})
