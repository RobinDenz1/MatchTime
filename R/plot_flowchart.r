
## plots a consort type flowchart of the matching process
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
                           perc_type="all",
                           number_format=format,
                           box_main_style="n_last",
                           box_main_text=list(),
                           box_main_halign=0.5,
                           box_main_nudge_x=0,
                           box_main_nudge_y=0,
                           box_main_padding=ggplot2::unit(c(5.5, 5.5, 5.5, 5.5),
                                                          "pt"),
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
                           box_sec_text=list(),
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
                           arrow_angle=30,
                           arrow_vjust=-0.4,
                           arrow_size=0.1,
                           xlim=c(-20, 20),
                           ylim=c(-6, 11),
                           ...) {

  .inclusion_names <- .treat_at_0 <- .treat <- . <- .n <- label <- box_id <-
    y <- xend <- yend <- NULL

  stopifnotm(inherits(x, "match_time"),
             "'x' must be a 'match_time' object created using match_time().")

  requireNamespace("ggplot2", quietly=TRUE)
  requireNamespace("ggtext", quietly=TRUE)

  # set highlighting for later
  if (n_fontface=="normal") {
    pref <- ""
  } else if (n_fontface=="italic") {
    pref <- "*"
  } else if (n_fontface=="bold") {
    pref <- "**"
  } else if (n_fontface=="bolditalic") {
    pref <- "***"
  }

  # change inclusion criteria labels
  if (!is.null(inclusion_text) & !all(is.na(x$info$inclusion))) {
    incl_names <- as.vector(unlist(inclusion_text[x$info$inclusion]))
  } else {
    incl_names <- x$info$inclusion
  }

  ## define labels for the main boxes in the middle
  labs_main <- c("Total Input Data", "Potential Cases", "Potential Controls",
                 "Potential Cases <br> Included in Matching",
                 "Potential Controls <br> Included in Matching",
                 "Matched Cases", "Matched Controls")

  # replace with custom ones if specified by user
  if (length(box_main_text) > 0) {
    labs_main <- replace_labs_main(labs_main, box_main_text)
  }

  numbers_main <- c(x$sizes$n_input_all, x$sizes$n_input_cases,
                    x$sizes$n_input_controls,
                    x$sizes$n_input_cases - sum(x$exclusion$stage1$.treat),
                    x$sizes$n_incl_controls, x$sizes$n_matched_cases,
                    x$sizes$n_matched_controls)
  labels_main <- vector(mode="character", length=7)
  for (i in seq_len(7)) {
    labels_main[i] <- get_main_label(n=numbers_main[i],
                                     text=labs_main[i],
                                     n_text="n = ",
                                     pref=pref,
                                     style=box_main_style,
                                     number_format=number_format,
                                     ...)
  }

  sec_labs <- get_sec_labs(box_sec_text)

  # 2.5 row left & right
  label_box2.5l <- get_label_inclusion(x=x, type="left1", digits=digits,
                                      incl_names=incl_names,
                                      perc_inclusion=perc_inclusion,
                                      perc_inclusion_total=perc_inclusion_total,
                                      remove_0_lines=remove_0_lines,
                                      format_fun=number_format,
                                      text=sec_labs[["box1l"]],
                                      perc_type=perc_type, ...)
  label_box2.5r <- get_label_inclusion(x=x, type="right1", digits=digits,
                                      incl_names=incl_names,
                                      perc_inclusion=perc_inclusion,
                                      perc_inclusion_total=perc_inclusion_total,
                                      remove_0_lines=remove_0_lines,
                                      format_fun=number_format,
                                      text=sec_labs[["box1r"]],
                                      perc_type=perc_type, ...)

  # 3.5 row left
  label_box3.5l <- get_label_inclusion(x=x, type="left2", digits=digits,
                                      incl_names=incl_names,
                                      perc_inclusion=perc_inclusion,
                                      perc_inclusion_total=perc_inclusion_total,
                                      remove_0_lines=remove_0_lines,
                                      format_fun=number_format,
                                      text=sec_labs[["box2l2"]],
                                      perc_type=perc_type, ...)
  label_no_match3.5l <- get_label_nomatch(x=x, perc_other=perc_other,
                                          digits=digits,
                                          format_fun=number_format,
                                          text=sec_labs[["box2l1"]], ...)
  label_box3.5l <- paste0(label_no_match3.5l, "<br>", label_box3.5l)

  # 3.5 row right
  label_box3.5r <- get_label_box_3.5r(x=x, remove_0_lines=remove_0_lines,
                                      perc_other=perc_other,
                                      format_fun=number_format,
                                      lab1=sec_labs[["box2r1"]],
                                      lab2=sec_labs[["box2r2"]], ...)

  # coordinates for main boxes
  d_box_coord <- data.frame(x=c(0, -5, 5, -5, 5, -5, 5),
                            y=c(10, 5, 5, 0, 0, -5, -5),
                            label=labels_main)

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
    empty_labs <- c(paste0("0 ", sec_labs[["box1l"]]),
                    paste0("0 ", sec_labs[["box1r"]]),
                    "No inclusion criteria applied",
                    paste0("0 ", sec_labs[["box1l"]], "<br>0 ",
                           sec_labs[["box1r"]]))
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
                            colour=line_sec_colour,
                            linewidth=line_main_linewidth,
                            linetype=line_main_linetype,
                            arrow=ggplot2::arrow(
                              type=arrow_type,
                              angle=arrow_angle,
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
calculate_n_box <- function(x, type, digits, perc_type) {

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

  # calculate total number of individuals not meeting inclusion criteria
  n_total_excl <- nrow(x$exclusion[[stage]][eval(cond)])
  perc_total_excl <- round((n_total_excl / n_total) * 100, digits=digits)

  if (perc_type!="all") {
    n_total <- n_total_excl
  }

  # calculate n and % of individuals not meeting inclusion criteria x
  n_box <- as.vector(unlist(
    x$exclusion[[stage]][eval(cond), lapply(.SD, neg_sum),
                         .SDcols=x$info$inclusion]
  ))
  perc <- round((n_box / n_total) * 100, digits=digits)

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
                                      remove_0_lines, format_fun,
                                      text, ...) {

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

  label_box <- paste0(format_fun(numbers$n_total, ...), perc_total, text,
                      label_box)

  return(label_box)
}

## get entire text in inclusion criteria box
get_label_inclusion <- function(x, type, digits, incl_names,
                                perc_inclusion, perc_inclusion_total,
                                remove_0_lines, format_fun,
                                perc_type, ...) {

  if (!all(is.na(x$info$inclusion))) {
    numbers <- calculate_n_box(x=x, type=type, digits=digits,
                               perc_type=perc_type)
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
                               format_fun, lab1, lab2, ...) {

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

  label_box3.5r <- paste0(n_never, perc_never, lab1)

  if (!(remove_0_lines & n_geq_1==0)) {
    label_box3.5r <- paste0(label_box3.5r, "<br>", n_geq_1, perc_geq_1, lab2)
  }

  return(label_box3.5r)
}

## create label for the "number of unmatched cases" string
get_label_nomatch <- function(x, digits, perc_other, format_fun, text, ...) {

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

  label <- paste0(n_nomatch, perc, text)
  return(label)
}

## create main labels
get_main_label <- function(n, text, pref, style, n_text="n = ",
                           number_format, ...) {

  if (style=="n_first") {
    label <- paste0(pref, n_text, number_format(n, ...), pref, "<br>", text)
  } else if (style=="n_last") {
    label <- paste0(text, "<br>", pref, n_text, number_format(n, ...), pref)
  }
  return(label)
}

# replaces the standard text of main boxes if specified by user
replace_labs_main <- function(labs_main, box_main_text) {

  names(labs_main) <- c("box1", "box2l", "box2r", "box3l", "box3r",
                        "box4l", "box4r")
  for (i in seq_len(length(box_main_text))) {
    name_i <- names(box_main_text[i])

    # must be a single character
    stopifnotm(length(box_main_text[[i]])==1 &&
               is.character(box_main_text[[i]]),
               "Every entry in 'box_main_text' must be a single character",
               "string.")

    if (name_i %in% names(labs_main)) {
      labs_main[[name_i]] <- box_main_text[[i]]
    } else {
      stop("'box_main_text' should be a named list of characters, where ",
           "the names can only be one or more of 'box1', 'box2l', ",
           "'box2r', 'box3l', 'box3r', 'box4l', 'box4r', not: ", name_i,
           call.=FALSE)
    }
  }
  names(labs_main) <- NULL
  return(labs_main)
}

## get text used in all secondary boxes
get_sec_labs <- function(box_sec_text) {

  defaults <- list(box1l=" never met inclusion criteria",
                   box1r=" never met inclusion criteria",
                   box2l1=" could no be matched",
                   box2l2=" did not meet inclusion criteria at treatment time",
                   box2r1=" never selected as controls",
                   box2r2=" selected as control more than once")

  for (i in seq_len(length(box_sec_text))) {
    name_i <- names(box_sec_text[i])

    # must be a single character
    stopifnotm(length(box_sec_text[[i]])==1 &&
                 is.character(box_sec_text[[i]]),
               "Every entry in 'box_sec_text' must be a single character",
               "string.")

    if (name_i %in% names(defaults)) {
      defaults[[name_i]] <- box_sec_text[[i]]
    } else {
      stop("'box_sec_text' should be a named list of characters, where ",
           "the names can only be one or more of 'box1l', 'box1r', ",
           "'box2l1', 'box2l2', 'box2r1', 'box2r2', not: ", name_i,
           call.=FALSE)
    }
  }

  return(defaults)
}
