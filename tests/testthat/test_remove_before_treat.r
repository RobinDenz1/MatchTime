
test_that("general test case", {

  input <- data.table(.id=c(1, 1, 1, 3, 3, 4),
                      start=c(1, 20, 351, 1, 23, 1),
                      stop=c(19, 350, 356, 22, 321, 365),
                      .time=c(110, 110, 110, 24, 24, NA))
  expected <- data.table(.id=c(1, 1, 3, 4),
                         start=c(110, 351, 24, 1),
                         stop=c(350, 356, 321, 365))

  out <- remove_before_treat(data=input, time=".time", overlap=FALSE)
  expect_equal(out, expected)
})
