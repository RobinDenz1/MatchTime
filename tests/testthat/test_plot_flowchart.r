
set.seed(1234)

data("heart", package="survival")

# add some random variables that will be used as inclusion criteria
# (in reality you would already have them)
heart$A <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.95, 0.05))
heart$B <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.999, 0.001))
heart$C <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.6, 0.4))

# perform some time-dependent matching with inclusion criteria
out <- match_time(transplant ~ surgery, data=heart, id="id",
                  match_method="fast_exact", method="brsm",
                  replace_over_t=TRUE, ratio=1,
                  inclusion=c("A", "B", "C"))

test_that("general test", {
  expect_snapshot_output(plot_flowchart(out))
})

test_that("using digits", {
  expect_snapshot_output(plot_flowchart(out, digits=3))
})

test_that("normal n = X", {
  expect_snapshot_output(plot_flowchart(out, n_fontface="normal"))
})

test_that("bold n = X", {
  expect_snapshot_output(plot_flowchart(out, n_fontface="bold"))
})

test_that("bolditalic n = X", {
  expect_snapshot_output(plot_flowchart(out, n_fontface="bolditalic"))
})

test_that("do not remove 0 lines", {
  expect_snapshot_output(plot_flowchart(out, remove_0_lines=FALSE,
                                        remove_0_boxes=TRUE))
})

test_that("changing inclusion text", {
  incl_text <- list(A=" did not die",
                    B=" did not fufil B",
                    C=" had something")
  expect_snapshot_output(plot_flowchart(out, inclusion_text=incl_text))
})

test_that("turn off percentages in inclusion criteria", {
  expect_snapshot_output(plot_flowchart(out, perc_inclusion=FALSE))
})

test_that("turn on percentages in totals", {
  expect_snapshot_output(plot_flowchart(out, perc_inclusion_total=TRUE))
})

test_that("turn on percentages for other numbers", {
  expect_snapshot_output(plot_flowchart(out, perc_other=TRUE))
})

test_that("using custom number formatting", {
  format_custom <- function(x) {
    paste0(x, " num")
  }
  expect_snapshot_output(plot_flowchart(out, number_format=format_custom))
})

test_that("change main style", {
  expect_snapshot_output(plot_flowchart(out, box_main_style="n_first"))
})

test_that("changing a main box style argument", {
  expect_snapshot_output(plot_flowchart(out, box_main_width=0.2))
})

test_that("changing a secondary box style argument", {
  expect_snapshot_output(plot_flowchart(out, box_sec_width=0.2))
})

test_that("changing a main line style argument", {
  expect_snapshot_output(plot_flowchart(out, line_main_colour="red"))
})

test_that("changing a secondary line style argument", {
  expect_snapshot_output(plot_flowchart(out, line_sec_linetype="solid"))
})

test_that("turning off arrows", {
  expect_snapshot_output(plot_flowchart(out, arrow=FALSE))
})

test_that("changing arrow style", {
  expect_snapshot_output(
    plot_flowchart(out,
                  arrow_angle=50,
                  arrow_type="open",
                  arrow_size=ggplot2::unit(0.5, "inches"),
                  arrow_vjust=1)
  )
})

test_that("changing plot limits", {
  expect_snapshot_output(plot_flowchart(out, xlim=c(-21, 20),
                                        ylim=c(-10, 11)))
})

test_that("replacing standard text in main boxes", {
  expect_snapshot_output(
    plot_flowchart(out, box_main_text=list(box1="lol",
                                           box2l="Potential vacc",
                                           box4r="Final Controls"))
  )
})

test_that("wrong name supplied to box_main_text", {
  expect_error({
    plot_flowchart(out, box_main_text=list(box12="lol",
                                           box2l="Potential vacc",
                                           box4r="Final Controls"))
  }, paste0("'box_main_text' should be a named list of characters, ",
            "where the names can only be one or more of 'box1', 'box2l', ",
            "'box2r', 'box3l', 'box3r', 'box4l', 'box4r', not: box12"),
  fixed=TRUE)
})

test_that("wrong input supplied to box_main_text", {
  expect_error({
    plot_flowchart(out, box_main_text=list(box1=c("lol", "lol2"),
                                           box2l="Potential vacc",
                                           box4r="Final Controls"))
  }, paste0("Every entry in 'box_main_text' must be a single ",
            "character string."),
  fixed=TRUE)
})

test_that("replacing standard text in secondary boxes", {
  expect_snapshot_output(
    plot_flowchart(out, box_sec_text=list(box1l="lol",
                                          box1r="Potential vacc",
                                          box2r1="Final Controls"))
  )
})

test_that("wrong name supplied to box_sec_text", {
  expect_error({
    plot_flowchart(out, box_sec_text=list(box12="lol",
                                           box2l="Potential vacc",
                                           box4r="Final Controls"))
  }, paste0("'box_sec_text' should be a named list of characters, where the",
            " names can only be one or more of 'box1l', 'box1r', 'box2l1', ",
            "'box2l2', 'box2r1', 'box2r2', not: box12"),
  fixed=TRUE)
})

test_that("wrong input supplied to box_sec_text", {
  expect_error({
    plot_flowchart(out, box_sec_text=list(box1l=c("lol", "lol2"),
                                           box2l1="Potential vacc",
                                           box2r2="Final Controls"))
  }, paste0("Every entry in 'box_sec_text' must be a single ",
            "character string."),
  fixed=TRUE)
})
