
test_that("general test case", {

  data("heart")

  set.seed(1234)

  out <- suppressWarnings(
    match_td(transplant ~ age + surgery, data=heart, id="id",
                  match_method="nearest", if_no_match="warn",
                  replace_over_t=FALSE)
  )

  out2 <- remove_unmatched(out)

  expect_equal(nrow(out2$data), 102)
  expect_equal(unique(as.vector(table(out2$data$.id_pair))), 2)
  expect_equal(as.vector(table(out2$data$.treat)), c(51, 51))
})

test_that("with ratio > 1", {

  data("heart")

  set.seed(1234)

  out <- suppressWarnings(
    match_td(transplant ~ age + surgery, data=heart, id="id",
             match_method="nearest", if_no_match="warn",
             replace_over_t=FALSE, ratio=2)
  )

  # removing all cases that got n
  out2 <- remove_unmatched(out)
  expect_equal(nrow(out2$data), 105)
  expect_equal(unique(as.vector(table(out2$data$.id_pair))), 3)
  expect_equal(as.vector(table(out2$data$.treat)), c(70, 35))
})
