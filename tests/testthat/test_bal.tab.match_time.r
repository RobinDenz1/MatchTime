
d_single <- readRDS(system.file("testdata",
                                "single_n1000.Rds",
                                package="MatchTime"))
d_single[, stop := stop + 1]

test_that("general test case", {

  set.seed(1346234)
  obj <- match_time(formula=vacc ~ mac,
                  data=d_single,
                  id=".id",
                  inclusion="inclusion",
                  match_method="fast_exact")

  # without unmatched
  out <- bal.tab(obj)
  expect_equal(round(out$Balance$Diff.Un, 3), c(0.24, 0.00, 0.00))

  # with unmatched
  out <- bal.tab(obj, remove_unmatched=FALSE)
  expect_equal(round(out$Balance$Diff.Un, 3), c(0.24, 0.00, 0.00))
})
