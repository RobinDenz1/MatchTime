
test_that("general test case", {

  data <- data.table(ID=c(1, 1, 1, 1, 2, 2, 2, 3, 4),
                     A=c("TRUE", "FALSE", "FALSE", "TRUE", "TRUE",
                         "FALSE", "FALSE", "TRUE", "FALSE"),
                     B=c("0", "1", "1", "1", "0", "1", "0", "0", "1"),
                     C=c("0.000023", "23.1234", "123.1234", "9.333",
                         "3.1234", "4.4234", "6.534", "0.942435", "423"),
                     D=c("4", "324", "3", "44", "45", "2", "4", "2", "9"),
                     E=c("A", "B", "C", "D", "e", "f", "g", "h", "ij"))

  col_types <- list(A="logical", B="logical", C="numeric", D="integer",
                    E="character")

  set_col_classes(data=data, col_types=col_types)

  expect_true(inherits(data$A, "logical"))
  expect_true(inherits(data$B, "logical"))
  expect_true(inherits(data$C, "numeric"))
  expect_true(inherits(data$D, "integer"))
  expect_true(inherits(data$E, "character"))
})
