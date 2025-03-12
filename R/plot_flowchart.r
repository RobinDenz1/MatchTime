
# TODO:
# - tests
# - make all box texts arguments
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
                           perc_inclusion=TRUE,
                           perc_inclusion_total=FALSE,
                           perc_other=FALSE,
                           number_format=format,
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
                           ylim=c(-6, 11),
                           ...) {

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
    incl_names <- as.vector(unlist(inclusion_text[x$info$inclusion]))
  } else {
    incl_names <- x$info$inclusion
  }

  ## define labels for the main boxes in the middle
  # 1 row
  label_box1 <- paste0("Total Input Data <br> ", pref,
                       "N = ", number_format(x$sizes$n_input_all, ...), pref)

  # 2 row
  label_box2l <- paste0("Potential Cases <br> ", pref,
                        "n = ", number_format(x$sizes$n_input_cases, ...), pref)
  label_box2r <- paste0("Potential Controls <br> ", pref, "n = ",
                        number_format(x$sizes$n_input_controls, ...), pref)

  # 3 row
  label_box3l <- paste0(
    "Potential Cases <br> Included in Matching <br> ", pref, "n = ",
     number_format(x$sizes$n_input_cases - sum(x$exclusion$stage1$.treat),
                   ...), pref
  )
  label_box3r <- paste0(
    "Potential Controls <br> Included in Matching <br> ", pref, "n = ",
    number_format(x$sizes$n_incl_controls, ...), pref
  )

  # 4 row
  label_box4l <- paste0("Matched Cases <br> ", pref, "n = ",
                        number_format(x$sizes$n_matched_cases, ...), pref)
  label_box4r <- paste0("Matched Controls <br> ", pref, "n = ",
                        number_format(x$sizes$n_matched_controls, ...), pref)

  # 2.5 row left & right
  label_box2.5l <- get_label_inclusion(x=x, type="left1", digits=digits,
                                      incl_names=incl_names,
                                      perc_inclusion=perc_inclusion,
                                      perc_inclusion_total=perc_inclusion_total,
                                      remove_0_lines=remove_0_lines,
                                      format_fun=number_format, ...)
  label_box2.5r <- get_label_inclusion(x=x, type="right1", digits=digits,
                                      incl_names=incl_names,
                                      perc_inclusion=perc_inclusion,
                                      perc_inclusion_total=perc_inclusion_total,
                                      remove_0_lines=remove_0_lines,
                                      format_fun=number_format, ...)

  # 3.5 row left
  label_box3.5l <- get_label_inclusion(x=x, type="left2", digits=digits,
                                      incl_names=incl_names,
                                      perc_inclusion=perc_inclusion,
                                      perc_inclusion_total=perc_inclusion_total,
                                      remove_0_lines=remove_0_lines,
                                      format_fun=number_format, ...)
  label_no_match3.5l <- get_label_nomatch(x=x, perc_other=perc_other,
                                          digits=digits,
                                          format_fun=number_format, ...)
  label_box3.5l <- paste0(label_no_match3.5l, "<br>", label_box3.5l)

  # 3.5 row right
  label_box3.5r <- get_label_box_3.5r(x=x, remove_0_lines=remove_0_lines,
                                      perc_other=perc_other,
                                      format_fun=number_format, ...)

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

## utility function to calculate n (percentage) for the boxes
## with inclusion criteria
calculate_n_box <- function(x, type, digits) {

  # specify what to calculate based on box
  if (type=="left1") {
    cond <- x$exclusion$stage1$.treat
    n_total <- x$sizes$n_input_cases
    stage <- "stage1"
  } else if (type=="right1") {
    cond <- !x$exclusion$stage1$.treat_at_0
    n_total <- x$sizes$n_input_controls
    stage <- "stage1"
  } else if (type=="left2") {
    cond <- TRUE
    n_total <- x$sizes$n_input_cases - sum(x$exclusion$stage1$.treat)
    stage <- "stage2"
  }

  # calculate n and % of individuals not meeting inclusion criteria x
  n_box <- as.vector(unlist(
    x$exclusion[[stage]][eval(cond), lapply(.SD, neg_sum),
                         .SDcols=x$info$inclusion]
  ))
  perc <- round((n_box / n_total) * 100, digits=digits)

  # calculate total number of individuals not meeting inclusion criteria
  n_total_excl <- nrow(x$exclusion[[stage]][eval(cond)])
  perc_total_excl <- round((n_total_excl / n_total) * 100, digits=digits)

  # put together
  out <- list(n=n_box, perc=perc, n_total=n_total_excl,
              perc_total=perc_total_excl, incl_vec=x$info$inclusion)
  return(out)
}

## removes all lines with n = 0
remove_0_n <- function(numbers) {

  numbers$incl_vec <- numbers$incl_vec[numbers$n!=0]
  numbers$perc <- numbers$perc[numbers$n!=0]
  numbers$n <- numbers$n[numbers$n!=0]

  return(numbers)
}

## create the label with inclusion criteria items info
get_label_inclusion_items <- function(numbers, perc_inclusion,
                                      perc_inclusion_total,
                                      remove_0_lines, type, format_fun,
                                      ...) {

  # if specified, add percentage to total number excluded as well
  if (perc_inclusion_total) {
    perc_total <- paste0(" (", format_fun(numbers$perc_total, ...), "%) ")
  } else {
    perc_total <- ""
  }

  if (length(numbers$n)!=0 | remove_0_lines) {
    if (perc_inclusion) {
      label_box <- paste0(
        paste0(format_fun(numbers$n, ...), " (", format_fun(numbers$perc, ...),
               "%) ", numbers$incl_vec),
        collapse="<br>"
      )
    } else {
      label_box <- paste0(
        paste0(format_fun(numbers$n, ...), " ", numbers$incl_vec),
        collapse="<br>"
      )
    }

    label_box <- paste0(": <br> <br>", label_box)
  } else {
    label_box <- ""
  }

  if (type=="left2") {
    incl_text <- " did not meet inclusion criteria at treatment time"
  } else {
    incl_text <- " never met inclusion criteria"
  }

  label_box <- paste0(format_fun(numbers$n_total, ...), perc_total, incl_text,
                      label_box)

  return(label_box)
}

## get entire text in inclusion criteria box
get_label_inclusion <- function(x, type, digits, incl_names,
                                perc_inclusion, perc_inclusion_total,
                                remove_0_lines, format_fun, ...) {

  if (!all(is.na(x$info$inclusion))) {
    numbers <- calculate_n_box(x=x, type=type, digits=digits)
    numbers$incl_vec <- incl_names

    # remove those with 0
    if (remove_0_lines) {
      numbers <- remove_0_n(numbers)
    }

    # get list of reasons for exclusion
    label_box <- get_label_inclusion_items(
      numbers=numbers,
      perc_inclusion=perc_inclusion,
      perc_inclusion_total=perc_inclusion_total,
      remove_0_lines=remove_0_lines,
      type=type,
      format_fun=format_fun,
      ...
    )

  } else {
    label_box <- "No inclusion criteria applied"
  }

  return(label_box)
}

## label for lower right box
get_label_box_3.5r <- function(x, remove_0_lines, digits, perc_other,
                               format_fun, ...) {

  . <- .treat <- .n <- NULL

  # number never selected
  n_never <- x$sizes$n_incl_controls -
    length(unique(x$data[.treat==FALSE][[x$id]]))
  perc_never <- round((n_never / x$sizes$n_incl_controls) * 100, digits=digits)

  # number selected more than once
  n_geq_1 <- nrow(x$data[, .(.n = .N), by=c(".treat", x$id)][
    .treat==FALSE & .n > 1])
  perc_geq_1 <- round((n_geq_1 / x$sizes$n_incl_controls) * 100, digits=digits)

  # format all numbers
  n_never <- format_fun(n_never, ...)
  perc_never <- format_fun(perc_never, ...)
  n_geq_1 <- format_fun(n_geq_1, ...)
  perc_geq_1 <- format_fun(perc_geq_1, ...)

  if (perc_other) {
    perc_never <- paste0(" (", perc_never, "%) ")
    perc_geq_1 <- paste0(" (", perc_geq_1, "%) ")
  } else {
    perc_never <- ""
    perc_geq_1 <- ""
  }

  label_box3.5r <- paste0(n_never, perc_never,
                          " never selected as controls")

  if (!(remove_0_lines & n_geq_1==0)) {
    label_box3.5r <- paste0(label_box3.5r, "<br>", n_geq_1, perc_geq_1,
                            " selected as control more than once")
  }

  return(label_box3.5r)
}

## create label for the "number of unmatched cases" string
get_label_nomatch <- function(x, digits, perc_other, format_fun, ...) {

  # calculate
  n_nomatch <- x$sizes$n_incl_cases - x$sizes$n_matched_cases
  perc_nomatch <- round((n_nomatch / x$sizes$n_incl_cases) * 100, digits=digits)

  # format
  n_nomatch <- format_fun(n_nomatch, ...)
  perc_nomatch <- format_fun(perc_nomatch, ...)

  if (perc_other) {
    perc <- paste0(" (", perc_nomatch, "%) ")
  } else {
    perc <- ""
  }

  label <- paste0(n_nomatch, perc, " could not be matched")
  return(label)
}
