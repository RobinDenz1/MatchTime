
# TODO:
# - documentation page
# - tests
# - include in README.rmd
# - make all box texts arguments
# - allow further stylistic customization options on boxes / lines
# - allow changes to inclusion criteria names
# - add option to remove 0 values and boxes if empty
# - check with different setting, methods, match_methods
# - maybe add "style" or similar to automatically put the number
#   before the text instead of TEXT <br> n =

## plots a consort type flowchart of the matching process
#' @importFrom data.table fifelse
#' @importFrom data.table .N
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @export
plot_flowchart <- function(x, digits=2, halign_box1=0.5,
                           halign_box2=0, fill_box1="lightblue",
                           fill_box2="cornsilk", arrow_vjust=-0.4,
                           arrow_size=0.1, arrow_type="closed",
                           n_fontface="normal", xlim=c(-20, 20),
                           ylim=c(-6, 11)) {

  stopifnotm(inherits(x, "match_time"),
             "'x' must be a 'match_time' object created using match_time().")

  requireNamespace("ggplot2")
  requireNamespace("ggtext")

  if (n_fontface=="normal") {
    pref <- ""
  } else if (n_fontface=="italic") {
    pref <- "*"
  } else if (n_fontface=="bold") {
    pref <- "**"
  }

  ## define labels for the main boxes in the middle
  # 1 row
  label_box1 <- paste0("Total Input Data <br> ", pref,
                       "N = ", x$sizes$n_input_all, pref)

  # 2 row
  label_box2l <- paste0("Potential Cases <br> ", pref,
                        "n = ", x$sizes$n_input_cases, pref)
  label_box2r <- paste0("Potential Controls <br> ", pref, "n = ",
                        x$sizes$n_input_controls, pref)

  # 3 row
  label_box3l <- paste0("Potential Cases <br> Included in Matching <br> ",
                        pref, "n = ",
                        x$sizes$n_input_cases - sum(x$exclusion$stage1$.treat),
                        pref)
  label_box3r <- paste0(
    "Potential Controls <br> Included in Matching <br> ", pref, "n = ",
    x$sizes$n_incl_controls, pref
  )

  # 4 row
  label_box4l <- paste0("Matched Cases <br> ", pref, "n = ",
                        x$sizes$n_matched_cases, pref)
  label_box4r <- paste0("Matched Controls <br> ", pref, "n = ",
                        x$sizes$n_matched_controls, pref)

  ## define labels for secondary boxes between row 2 and 3
  # calculate n and % controls
  n2.5r <- as.vector(unlist(
    x$exclusion$stage1[.treat_at_0!=TRUE, lapply(.SD, sum),
                       .SDcols=x$info$inclusion]
  ))
  perc2.5r <- (n2.5r / x$sizes$n_input_controls) * 100

  # calculate n and % cases
  n2.5l <- as.vector(unlist(
    x$exclusion$stage1[.treat==TRUE, lapply(.SD, sum),
                       .SDcols=x$info$inclusion]))
  perc2.5l <- (n2.5l / x$sizes$n_input_cases) * 100

  # labels
  label_box2.5l <- paste0(paste0(round(n2.5l, digits=digits),
                                 " (", round(perc2.5l, digits=digits), "%) ",
                                 x$info$inclusion),
                          collapse="<br>")
  label_box2.5l <- paste0(sum(x$exclusion$stage1$.treat),
                          " never met inclusion criteria: <br> <br>",
                          label_box2.5l)

  label_box2.5r <- paste0(paste0(round(n2.5r, digits=digits),
                                 " (", round(perc2.5r, digits=digits), "%) ",
                                 x$info$inclusion),
                          collapse="<br>")
  label_box2.5r <- paste0(nrow(x$exclusion$stage1),
                          " never met inclusion criteria: <br> <br>",
                          label_box2.5r)

  ## define labels for secondary boxes between row 3 and 4

  # calculate n and %
  n3.5l <- as.vector(unlist(
    x$exclusion$stage2[, lapply(.SD, sum), .SDcols=x$info$inclusion]
  ))
  perc3.5l <- (n3.5l / x$sizes$n_incl_cases) * 100

  # labels
  label_box3.5l <- paste0(paste0(round(n3.5l, digits=digits),
                                 " (", round(perc3.5l, digits=digits), "%) ",
                                 x$info$inclusion),
                          collapse="<br>")
  label_box3.5l <- paste0(
    x$sizes$n_incl_cases - x$sizes$n_matched_cases,
    " could not be matched <br>",
    x$sizes$n_input_cases - x$sizes$n_incl_cases,
    " did not meet inclusion criteria at treatment time: <br> <br>",
    label_box3.5l
  )

  n_geq_1 <- nrow(x$data[, .(.n = .N), by=c(".treat", x$id)][
    .treat==FALSE & .n > 1])
  label_box3.5r <- paste0(x$sizes$n_incl_controls -
                            length(unique(x$data[.treat==FALSE][[x$id]])),
                          " never selected as controls <br>", n_geq_1,
                          " selected as control more than once")

  # coordinates for main boxes
  d_box_coord <- data.frame(x=c(0, -5, 5, -5, 5, -5, 5),
                            y=c(10, 5, 5, 0, 0, -5, -5),
                            label=c(label_box1, label_box2l,
                                    label_box2r, label_box3l,
                                    label_box3r, label_box4l,
                                    label_box4r))

  # coordinates for secondary boxes
  d_box_coord2 <- data.frame(x=c(-14, 14, -14, 14),
                             y=c(2.5, 2.5, -2.5, -2.5),
                             label=c(label_box2.5l, label_box2.5r,
                                     label_box3.5l, label_box3.5r))

  # coordinates for main lines
  d_lines1 <- data.frame(x=c(0, -5, -5, 5),
                         xend=c(0, 5, -5, 5),
                         y=c(10, 7.5, 7.5, 7.5),
                         yend=c(7.5, 7.5, -5, -5))

  # coordinates for lines to secondary boxes
  d_lines2 <- data.frame(x=c(-5, 5, -5, 5),
                         xend=c(-15, 15, -15, 15),
                         y=c(2.5, 2.5, -2.5, -2.5),
                         yend=c(2.5, 2.5, -2.5, -2.5))

  # coordinates for arrow heads
  d_arrows <- data.frame(x=c(5, 5, 5, -5, -5, -5),
                         xend=c(5, 5, 5, -5, -5, -5),
                         y=c(7.5, 7.5, 7.5, 7.5, 7.5, 7.5),
                         yend=c(6, 1.2, -4, 6, 1.2, -4) + arrow_vjust)

  ## main plot
  p <- ggplot2::ggplot(d_box_coord, ggplot2::aes(x=x, y=y, label=label)) +
    # main lines
    ggplot2::geom_segment(data=d_lines1,
                          ggplot2::aes(x=x, xend=xend, y=y, yend=yend),
                          inherit.aes=FALSE) +
    # secondary lines
    ggplot2::geom_segment(data=d_lines2,
                          ggplot2::aes(x=x, xend=xend, y=y, yend=yend),
                          inherit.aes=FALSE, linetype="dashed") +
    # arrows
    ggplot2::geom_segment(data=d_arrows,
                          ggplot2::aes(x=x, xend=xend, y=y, yend=yend),
                          inherit.aes=FALSE,
                          arrow=ggplot2::arrow(
                            type=arrow_type,
                            length=ggplot2::unit(arrow_size, "inches"))
                          ) +
    # main boxes
    ggtext::geom_textbox(halign=halign_box1, fill=fill_box1) +
    # inclusion boxes
    ggtext::geom_textbox(data=d_box_coord2, halign=halign_box2,
                         fill=fill_box2) +
    # scaling the plot
    ggplot2::xlim(xlim) +
    ggplot2::ylim(ylim) +
    # removing all axis stuff
    ggplot2::theme_void()

  return(p)
}
