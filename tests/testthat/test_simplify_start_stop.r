
test_that("general test cases", {

  data1 <- data.table(id=1,
                      start=c(1, 20, 35, 120, 923, 1022, 2000, 3011),
                      stop=c(20, 35, 120, 923, 1022, 2000, 3011, 3013),
                      A=c(0, 0, 0, 1, 1, 0, 0, 0),
                      B=c(1, 0, 0, 1, 0, 0, 0, 0),
                      C=c(11, 0.2, 17.8, 2.1, 9.0001, 1.2, 33, 22))
  data2 <- data.table(id=2,
                      start=c(1, 20, 35, 120, 923),
                      stop=c(20, 35, 120, 923, 1022),
                      A=c(0, 0, 1, 1, 1),
                      B=c(1, 0, 0, 1, 0),
                      C=c(11, 0.2, 17.8, 2.1, 9.0001)+1)
  data <- rbind(data1, data2)
  setkey(data, id, start)

  # nothing changes when every row is unique
  out1 <- simplify_start_stop(data, id="id")
  expect_equal(data, out1)

  # three rows are removed when excluding C
  out2 <- simplify_start_stop(data, id="id", cols=c("A", "B"))
  expect_true(nrow(out2)==nrow(data)-3)
  expect_true(out2$start[2]==20 & out2$stop[2]==120)

  # using different names
  setnames(data, old=c("id", "start", "stop"), new=c("ID_Pers", "begin", "end"))
  out3 <- simplify_start_stop(data, id="ID_Pers", start="begin", stop="end",
                              cols=c("A", "B"))
  setnames(out3, new=c("id", "start", "stop"), old=c("ID_Pers", "begin", "end"))
  setnames(data, new=c("id", "start", "stop"), old=c("ID_Pers", "begin", "end"))
  expect_equal(out2, out3)

  # keep C as a column
  out4 <- simplify_start_stop(data, id="id", cols=c("A", "B"),
                              remove_other_cols=FALSE)
  expect_equal(colnames(data), colnames(out4))
})

test_that("with no change at the beginning", {

  data1 <- data.table(id=1,
                      start=c(1, 20, 35, 120, 923, 1022, 2000, 3011),
                      stop=c(20, 35, 120, 923, 1022, 2000, 3011, 3013),
                      A=c(0, 0, 0, 1, 1, 0, 0, 0),
                      B=c(0, 0, 1, 1, 0, 0, 0, 0),
                      C=c(11, 0.2, 17.8, 2.1, 9.0001, 1.2, 33, 22))
  data2 <- data.table(id=2,
                      start=c(1, 20, 35, 120, 923),
                      stop=c(20, 35, 120, 923, 1022),
                      A=c(0, 0, 1, 1, 1),
                      B=c(1, 0, 0, 1, 0),
                      C=c(11, 0.2, 17.8, 2.1, 9.0001)+1)
  data <- rbind(data1, data2)
  setkey(data, id, start)

  out <- simplify_start_stop(data, id="id", cols=c("A", "B"))

  # three rows are removed when excluding C
  expect_true(nrow(out)==nrow(data)-3)
  expect_true(out$start[1]==1 & out$stop[1]==35)
  expect_true(out$start[2]==35 & out$stop[2]==120)
})

test_that("works if there are no changes at all in individual", {

  data <- data.table(id=c(1, 1, 1, 1, 2, 2),
                     start=c(0, 100, 220, 1001, 29, 901),
                     stop=c(100, 220, 1001, 2344, 901, 1022),
                     var1=c(0, 0, 0, 0, 1, 2),
                     var2=c(1.1, 1.2, 1.2, 1.3, 1.4, 1.5))

  expected <- data.table(id=c(1, 2, 2),
                         start=c(0, 29, 901),
                         stop=c(2344, 901, 1022),
                         var1=c(0, 1, 2))
  setkey(expected, id, start)

  out <- simplify_start_stop(data, id="id", cols="var1",
                             remove_other_cols=TRUE)
  expect_equal(out, expected)
})
