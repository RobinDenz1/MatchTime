
test_that("general tests", {
  expect_warning(warnifnotm(FALSE, "one part"), "one part")
  expect_warning(warnifnotm(FALSE, "this", "has", "multiple", "parts"),
                 "this has multiple parts")
  expect_no_warning(warnifnotm(TRUE, "something"))
})
