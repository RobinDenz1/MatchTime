
d1 <- data.table(ID=c(1, 1, 1, 2, 2, 3),
                 A=c(0, 1, 1, 2, 2, 4))

d2 <- data.table(ID=c(5, 5, 5, 2, 2, 3, 18),
                 B=c(0, 1, 1, 2, 2, 4, 7))

d3 <- data.table(ID=c(1, 1, 1, 2, 2, 3),
                 C=c(0, 1, 1, 2, 2, 4))

test_that("two data.tables in dlist", {

  dlist <- list(d1, d2)
  expected <- list(only_in_x=1, only_in_y=c(5, 18))

  out <- get_unique_x_y_ids(dlist, id="ID")
  expect_equal(out, expected)
})

test_that("three data.tables in dlist", {

  dlist <- list(d1, d2, d3)
  expected <- list(only_in_x=numeric(0), only_in_y=c(5, 18))

  out <- get_unique_x_y_ids(dlist, id="ID")
  expect_equal(out, expected)
})
