
test_that("general test case", {

  data <- data.table(id=c(1, 1, 1, 2, 2, 3),
                     start=c(0, 14, 26, 0, 18, 0),
                     stop=c(14, 26, 30, 18, 32, 51),
                     A=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                     B=c(1L, 1L, 2L, 3L, 5L, 6L),
                     C=c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
                     D=c("A", "B", "C", "D", "E", "F"))

  out <- start_stop2long(data, id="id")

  expect_true(nrow(out)==113)
  expect_equal(colnames(out), c("id", "A", "B", "C", "D", "time"))
  expect_equal(out$time, c(seq_len(30)-1, seq_len(32)-1, seq_len(51)-1))

  out <- start_stop2long(data, id="id", include_last_t=TRUE)
  expect_true(nrow(out)==116)
  expect_equal(colnames(out), c("id", "A", "B", "C", "D", "time"))
  expect_equal(out$time, c(seq_len(31)-1, seq_len(33)-1, seq_len(52)-1))
})

test_that("using different names", {

  data <- data.table(identifier=c(1, 1, 1, 2, 2, 3),
                     beginning=c(0, 14, 26, 0, 18, 0),
                     ending=c(14, 26, 30, 18, 32, 51),
                     A=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                     B=c(1L, 1L, 2L, 3L, 5L, 6L),
                     C=c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
                     D=c("A", "B", "C", "D", "E", "F"))

  out <- start_stop2long(data, id="identifier", start="beginning",
                         stop="ending", time_name="time_variable")

  expect_true(nrow(out)==113)
  expect_equal(colnames(out), c("identifier", "A", "B", "C", "D",
                                "time_variable"))
  expect_equal(out$time, c(seq_len(30)-1, seq_len(32)-1, seq_len(51)-1))
})
