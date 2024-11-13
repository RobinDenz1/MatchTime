
test_that("general test case", {
  data <- data.table(id=c(1, 1, 1, 1, 2, 2, 3, 3, 4, 5, 5, 5, 5, 5),
                     start=c(1, 15, 37, 40, 1, 35, 1, 80, 1,
                             1, 15, 37, 40, 88),
                     stop=c(14, 36, 39, 110, 34, 134, 79, 300, 512,
                            14, 36, 39, 87, 127),
                     event=c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                             TRUE, FALSE, FALSE, FALSE, TRUE, TRUE,
                             FALSE, TRUE))

  out <- times_from_start_stop(data=data, name="event", id="id")
  expected <- data.table(id=c(1, 1, 2, 3, 5, 5),
                         .time=c(15, 40, 35, 1, 15, 88))
  setkeyv(expected, c("id", ".time"))
  expect_equal(out, expected)
})

test_that("no events", {
  data <- data.table(id=c(1, 1, 2, 3, 4),
                     start=c(1, 55, 1, 1, 1),
                     stop=c(54, 135, 425, 2345, 56),
                     event=c(FALSE, FALSE, FALSE, FALSE, FALSE),
                     event2=c(FALSE, TRUE, FALSE, TRUE, FALSE))

  out <- times_from_start_stop(data=data, name="event", id="id")
  expect_true(is.data.table(out) && nrow(out)==0)
})
