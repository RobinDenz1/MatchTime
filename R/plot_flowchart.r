
# TODO:
# - tests
# - make all box texts arguments
# - allow further stylistic customization options on boxes / lines
# - allow percentages on / off for main text + items separately
# - maybe add "style" or similar to automatically put the number
#   before the text instead of TEXT <br> n =

## plots a consort type flowchart of the matching process
#' @importFrom data.table fifelse
#' @importFrom data.table .N
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @export
plot_flowchart <- function(x,
                           digits=2,
                           n_fontface="italic",
                           inclusion_text=NULL,
                           remove_0_lines=TRUE,
                           remove_0_boxes=FALSE,
                           box_main_halign=0.5,
                           box_main_nudge_x=0,
                           box_main_nudge_y=0,
                           box_main_padding=ggplot2::unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
                           box_main_margin=ggplot2::unit(c(0, 0, 0, 0), "pt"),
                           box_main_r=ggplot2::unit(5.5, "pt"),
                           box_main_width=ggplot2::unit(2, "inch"),
                           box_main_minwidth=NULL,
                           box_main_maxwidth=NULL,
                           box_main_height=NULL,
                           box_main_minheight=NULL,
                           box_main_maxheight=NULL,
                           box_main_colour="black",
                           box_main_fill="lightblue",
                           box_sec_halign=0,
                           box_sec_nudge_x=box_main_nudge_x,
                           box_sec_nudge_y=box_main_nudge_y,
                           box_sec_padding=box_main_padding,
                           box_sec_margin=box_main_margin,
                           box_sec_r=box_main_r,
                           box_sec_width=box_main_width,
                           box_sec_minwidth=box_main_minwidth,
                           box_sec_maxwidth=box_main_maxwidth,
                           box_sec_height=box_main_height,
                           box_sec_minheight=box_main_minheight,
                           box_sec_maxheight=box_main_maxheight,
                           box_sec_colour=box_main_colour,
                           box_sec_fill="cornsilk",
                           line_main_colour="black",
                           line_main_linetype="solid",
                           line_main_linewidth=0.5,
                           line_sec_colour=line_main_colour,
                           line_sec_linetype="dashed",
                           line_sec_linewidth=line_main_linewidth,
                           arrow=TRUE,
                           arrow_type="closed",
                           arrow_vjust=-0.4,
                           arrow_size=0.1,
                           xlim=c(-20, 20),
                           ylim=c(-6, 11)) {

  .inclusion_names <- .treat_at_0 <- .treat <- . <- .n <- label <- box_id <-
    y <- xend <- yend <- NULL

  stopifnotm(inherits(x, "match_time"),
             "'x' must be a 'match_time' object created using match_time().")

  requireNamespace("ggplot2")
  requireNamespace("ggtext")

  # set highliting for later
  if (n_fontface=="normal") {
    pref <- ""
  } else if (n_fontface=="italic") {
    pref <- "*"
  } else if (n_fontface=="bold") {
    pref <- "**"
  }

  # change inclusion criteria labels
  if (!is.null(inclusion_text) & !all(is.na(x$info$inclusion))) {
    incl_names <- as.vector(unlist(inclusion_names[x$info$inclusion]))
  } else {
    incl_names <- x$info$inclusion
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
  if (!all(is.na(x$info$inclusion))) {

    # calculate n and % controls for left box
    n2.5r <- as.vector(unlist(
      x$exclusion$stage1[.treat_at_0!=TRUE, lapply(.SD, neg_sum),
                         .SDcols=x$info$inclusion]
    ))
    perc2.5r <- (n2.5r / x$sizes$n_input_controls) * 100

    # calculate n and % cases for right box
    n2.5l <- as.vector(unlist(
      x$exclusion$stage1[.treat==TRUE, lapply(.SD, neg_sum),
                         .SDcols=x$info$inclusion]))
    perc2.5l <- (n2.5l / x$sizes$n_input_cases) * 100

    # remove those with 0
    if (remove_0_lines) {
      incl_vec_l <- incl_names[n2.5l!=0]
      n2.5l <- n2.5l[n2.5l!=0]
      perc2.5l <- perc2.5l[perc2.5l!=0]

      incl_vec_r <- incl_names[n2.5r!=0]
      n2.5r <- n2.5r[n2.5r!=0]
      perc2.5r <- perc2.5r[perc2.5r!=0]
    } else {
      incl_vec_l <- incl_vec_r <- incl_names
    }

    # labels for left box
    if (length(n2.5l)!=0 | remove_0_lines) {
      label_box2.5l <- paste0(
        paste0(round(n2.5l, digits=digits), " (",
               round(perc2.5l, digits=digits), "%) ",
               incl_vec_l),
        collapse="<br>"
      )
      label_box2.5l <- paste0(": <br> <br>", label_box2.5l)
    } else {
      label_box2.5l <- ""
    }

    label_box2.5l <- paste0(sum(x$exclusion$stage1$.treat),
                            " never met inclusion criteria",
                            label_box2.5l)

    # labels for right box
    if (length(n2.5r)!=0 | remove_0_lines) {
      label_box2.5r <- paste0(
        paste0(round(n2.5r, digits=digits), " (",
               round(perc2.5r, digits=digits), "%) ",
               incl_vec_r),
        collapse="<br>"
      )
      label_box2.5r <- paste0(": <br> <br>", label_box2.5r)
    } else {
      label_box2.5r <- ""
    }

    label_box2.5r <- paste0(nrow(x$exclusion$stage1),
                            " never met inclusion criteria",
                            label_box2.5r)
  } else {
    label_box2.5l <- "No inclusion criteria applied"
    label_box2.5r <- "No inclusion criteria applied"
  }

  ## define labels for secondary boxes between row 3 and 4
  if (!all(is.na(x$info$inclusion))) {

    # calculate n and % for left box
    n3.5l <- as.vector(unlist(
      x$exclusion$stage2[, lapply(.SD, neg_sum), .SDcols=x$info$inclusion]
    ))
    perc3.5l <- (n3.5l / (x$sizes$n_input_cases -
                            sum(x$exclusion$stage1$.treat))) * 100

    if (remove_0_lines) {
      incl_vec <- incl_names[n3.5l!=0]
      n3.5l <- n3.5l[n3.5l!=0]
      perc3.5l <- perc3.5l[perc3.5l!=0]
    } else {
      incl_vec <- incl_names
    }

    # label for left box
    label_box3.5l <- paste0(paste0(round(n3.5l, digits=digits),
                                   " (", round(perc3.5l, digits=digits), "%) ",
                                   incl_vec),
                            collapse="<br>")
    n_crit3.5l <- sum(n3.5l)
  } else {
    n_crit3.5l <- 0
    label_box3.5l <- ""
  }

  # could not be matched
  no_match3.5l <- paste0(x$sizes$n_incl_cases - x$sizes$n_matched_cases,
                         " could not be matched")

  # did not meet inclusion criteria at treatment time
  if (all(is.na(x$info$inclusion))) {
    no_crit3.5l <- "No inclusion criteria applied"
  } else if (n_crit3.5l==0 & remove_0_lines) {
    no_crit3.5l <- "0 did not meet inclusion criteria at treatment time"
  } else {
    no_crit3.5l <- paste0(
      n_crit3.5l,
      " did not meet inclusion criteria at treatment time: <br> <br>",
      label_box3.5l)
  }

  # put together no match & inclusion at t
  label_box3.5l <- paste0(no_match3.5l, "<br>", no_crit3.5l)

  # right side bottom
  n_geq_1 <- nrow(x$data[, .(.n = .N), by=c(".treat", x$id)][
    .treat==FALSE & .n > 1])
  label_box3.5r <- paste0(x$sizes$n_incl_controls -
                            length(unique(x$data[.treat==FALSE][[x$id]])),
                          " never selected as controls")

  if (!(remove_0_lines & n_geq_1==0)) {
    label_box3.5r <- paste0(label_box3.5r, "<br>", n_geq_1,
                            " selected as control more than once")
  }

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
                                     label_box3.5l, label_box3.5r),
                             box_id=c(1, 2, 3, 4))

  # coordinates for main lines
  d_lines1 <- data.frame(x=c(0, -5, -5, 5),
                         xend=c(0, 5, -5, 5),
                         y=c(10, 7.5, 7.5, 7.5),
                         yend=c(7.5, 7.5, -5, -5))

  # coordinates for lines to secondary boxes
  d_lines2 <- data.frame(x=c(-5, 5, -5, 5),
                         xend=c(-15, 15, -15, 15),
                         y=c(2.5, 2.5, -2.5, -2.5),
                         yend=c(2.5, 2.5, -2.5, -2.5),
                         box_id=c(1, 2, 3, 4))

  # coordinates for arrow heads
  d_arrows <- data.frame(x=c(5, 5, 5, -5, -5, -5),
                         xend=c(5, 5, 5, -5, -5, -5),
                         y=c(7.5, 7.5, 7.5, 7.5, 7.5, 7.5),
                         yend=c(6, 1.2, -4, 6, 1.2, -4) + arrow_vjust)

  # remove no information boxes if specified
  if (remove_0_lines & remove_0_boxes) {
    empty_labs <- c("0 never met inclusion criteria",
                    "0 never selected as controls",
                    "No inclusion criteria applied",
                    paste0("0 could not be matched<br>0 did not meet ",
                           "inclusion criteria at treatment time"))
    d_box_coord2 <- subset(d_box_coord2, !label %in% empty_labs)
    d_lines2 <- subset(d_lines2, box_id %in% d_box_coord2$box_id)
  }

  ## main plot
  p <- ggplot2::ggplot(d_box_coord, ggplot2::aes(x=x, y=y, label=label)) +
    # main lines
    ggplot2::geom_segment(data=d_lines1,
                          ggplot2::aes(x=x, xend=xend, y=y, yend=yend),
                          inherit.aes=FALSE,
                          colour=line_main_colour,
                          linewidth=line_main_linewidth,
                          linetype=line_main_linetype) +
    # secondary lines
    ggplot2::geom_segment(data=d_lines2,
                          ggplot2::aes(x=x, xend=xend, y=y, yend=yend),
                          inherit.aes=FALSE,
                          colour=line_sec_colour,
                          linewidth=line_sec_linewidth,
                          linetype=line_sec_linetype)
  # arrows
  if (arrow) {
    p <- p +
      ggplot2::geom_segment(data=d_arrows,
                            ggplot2::aes(x=x, xend=xend, y=y, yend=yend),
                            inherit.aes=FALSE,
                            arrow=ggplot2::arrow(
                              type=arrow_type,
                              length=ggplot2::unit(arrow_size, "inches")
                            )
      )
  }

  p <- p +
    # main boxes
    ggtext::geom_textbox(halign=box_main_halign,
                         nudge_x=box_main_nudge_x,
                         nudge_y=box_main_nudge_y,
                         box.padding=box_main_padding,
                         box.margin=box_main_margin,
                         box.r=box_main_r,
                         width=box_main_width,
                         minwidth=box_main_minwidth,
                         maxwidth=box_main_maxwidth,
                         height=box_main_height,
                         minheight=box_main_minheight,
                         maxheight=box_main_maxheight,
                         box.colour=box_main_colour,
                         fill=box_main_fill) +
    # inclusion boxes
    ggtext::geom_textbox(data=d_box_coord2,
                         halign=box_sec_halign,
                         nudge_x=box_sec_nudge_x,
                         nudge_y=box_sec_nudge_y,
                         box.padding=box_sec_padding,
                         box.margin=box_sec_margin,
                         box.r=box_sec_r,
                         width=box_sec_width,
                         minwidth=box_sec_minwidth,
                         maxwidth=box_sec_maxwidth,
                         height=box_sec_height,
                         minheight=box_sec_minheight,
                         maxheight=box_sec_maxheight,
                         box.colour=box_sec_colour,
                         fill=box_sec_fill) +
    # scaling the plot
    ggplot2::xlim(xlim) +
    ggplot2::ylim(ylim) +
    # removing all axis stuff
    ggplot2::theme_void()

  return(p)
}

# take the sum of the inverse of a logical variable
neg_sum <- function(x, ...) {
  sum(!x, ...)
}
