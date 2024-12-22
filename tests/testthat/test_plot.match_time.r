
d_single <- readRDS(system.file("testdata",
                                "single_n1000.Rds",
                                package="MatchTime"))
d_single[, stop := stop + 1]

set.seed(1346)
out <- match_time(formula=vacc ~ mac,
                data=d_single,
                id=".id",
                inclusion="inclusion",
                match_method="fast_exact")

test_that("general test case", {
  expect_snapshot_output(plot(out))
})

test_that("using only cum. no. controls and cases", {
  expect_snapshot_output(plot(out, include=c("controls", "cases")))
})

test_that("using some customizations", {
  expect_snapshot_output(plot(out,
                              linealpha=0.5,
                              linewidth=2,
                              linetype="dashed",
                              xlab="X",
                              ylab="Y",
                              gg_theme=ggplot2::theme_classic()))
})
