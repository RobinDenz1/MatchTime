
test_that("general test case", {

  data("heart", package="survival")

  set.seed(1234)

  out <- suppressWarnings(
    match_time(transplant ~ age + surgery, data=heart, id="id",
                  match_method="nearest", replace_over_t=FALSE)
  )

  data <- get_match_data(out)

  expect_equal(nrow(data), 102)
  expect_equal(unique(as.vector(table(data$.id_pair))), 2)
  expect_equal(as.vector(table(data$.treat)), c(51, 51))

  # no removing, equal to input
  data <- get_match_data(out, remove_unmatched=FALSE)
  expect_equal(data, out$data)
})

test_that("with ratio > 1", {

  data("heart", package="survival")

  set.seed(1234)

  out <- suppressWarnings(
    match_time(transplant ~ age + surgery, data=heart, id="id",
             match_method="nearest",
             replace_over_t=FALSE, ratio=2)
  )

  # removing all cases that got n
  data <- get_match_data(out)
  expect_equal(nrow(data), 105)
  expect_equal(unique(as.vector(table(data$.id_pair))), 3)
  expect_equal(as.vector(table(data$.treat)), c(70, 35))

  # no removing, equal to input
  data <- get_match_data(out, remove_unmatched=FALSE)
  expect_equal(data, out$data)

  # removing with n_required specified
  data <- get_match_data(out, remove_unmatched=TRUE, n_required=1)
  expect_true(is.data.table(data))

  # warning if n_required gets ignored
  out$data[, .id_pair := NULL]
  expect_warning(get_match_data(out, remove_unmatched=TRUE, n_required=1),
                 paste0("Argument 'n_required' ignored, because there are ",
                        "no pairs to which this argument could be applied."))
})
