
d_multi <- readRDS(system.file("testdata",
                               "multi_n1000.Rds",
                               package="MatchTD"))

test_that("general test case", {

  input <- data.table(.id=c(1, 1, 1, 3, 3, 4),
                      start=c(1, 20, 351, 1, 23, 1),
                      stop=c(19, 350, 356, 22, 321, 365),
                      .time=c(110, 110, 110, 24, 24, NA))
  expected <- data.table(.id=c(1, 1, 3, 3, 4),
                         start=c(1, 20, 1, 23, 1),
                         stop=c(19, 110, 22, 24, 365))

  out <- remove_after_treat(data=input, time=".time", overlap=FALSE)
  expect_equal(out, expected)
})
