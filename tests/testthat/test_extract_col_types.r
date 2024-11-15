
test_that("general test case", {

  data <- data.table(ID=c(1, 1, 1, 1, 2, 2, 2, 3, 4),
                     A=c(TRUE, FALSE, FALSE, TRUE, TRUE,
                         FALSE, FALSE, TRUE, FALSE),
                     B=c("0", "1", "1", "1", "0", "1", "0", "0", "1"),
                     C=c(0.000023, 23.1234, 123.1234, 9.333,
                         3.1234, 4.4234, 6.534, 0.942435, 423),
                     D=c(4L, 324L, 3L, 44L, 45L, 2L, 4L, 2L, 9L),
                     E=c("A", "B", "C", "D", "e", "f", "g", "h", "ij"),
                     Date_col=as.Date(round(runif(n=9, min=10, max=100)),
                                      origin=as.Date("1992-12-30")))
  data[, posix_col := as.POSIXct(data$Date_col)]
  data[, custom_class := runif(n=9, min=100, max=3242)]
  class(data[["custom_class"]]) <- c("some random thing", "and another one")

  expected <- list(ID="numeric",
                   A="logical", B="character", C="numeric", D="integer",
                   E="character", Date_col="Date",
                   posix_col=c("POSIXct", "POSIXt"),
                   custom_class=c("some random thing", "and another one"))

  out <- extract_col_types(data=data, cnames=colnames(data))

  expect_equal(out, expected)
})
