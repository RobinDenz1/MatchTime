
## plot the timeline of some ids after matching
#' @importFrom data.table :=
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @export
plot_timeline <- function(x, include, id_type=x$id, time_name, status_name,
                          treat_point=TRUE, outcome_point=TRUE,
                          next_treat_point=TRUE, linetype="solid", linewidth=1,
                          size=3, shape_treat=18, shape_outcome=16,
                          shape_next_treat=8, xlab="Time", ylab=".id_new",
                          legend.position="right",
                          gg_theme=ggplot2::theme_bw(), warn=TRUE) {

  .id_new <- .id_pair <- .next_treat_time <- .treat <- .data <- NULL

  requireNamespace("ggplot2")

  ## various input checks

  # x
  stopifnotm(inherits(x, "match_time"),
     "'x' must be a match_time object created using the match_time() function.")

  # include
  if (!missing(include)) {
    stopifnotm(is.numeric(include) & length(numeric) > 0,
    "If supplied, 'include' must be a numeric vector with at least one entry.")
  }

  # time_name
  if (!missing(time_name)) {
    stopifnotm(is_single_character(time_name) &&
                 time_name %in% x$info$added_event_time,
               "'time_name' must specify an event-time in 'x'",
               "specified using the 'outcomes' argument in match_time() or",
               "added using add_outcome().")
  }

  # status name
  if (!missing(status_name)) {
    stopifnotm(is_single_character(status_name) &&
                 status_name %in% x$info$added_status,
               "'status_name' must specify an event-status in 'x'",
               "specified using the 'outcomes' argument in match_time() or",
               "added using add_outcome().")
  }

  # correct .id_pair
  if (id_type==".id_pair" && !".id_pair" %in% colnames(x$data)) {
    stop("Cannot use id_type='.id_pair' if a 'match_method' was used in",
         " the original match_time() call that does not create an '.id_pair'",
         " column.", call.=FALSE)
  }

  # if no id is specified, just take all of them
  if (missing(include)) {
    plotdata <- x$data

    if (warn) {
      warning("'include' is not specified.",
              " The entire dataset will be plotted, which might be difficult",
            " to display correctly. Set 'warn=FALSE' to silence this warning.")
    }
  # otherwise, take subset
  } else if (id_type==x$id) {
    plotdata <- subset(x$data, get(x$id) %in% include)
    setkeyv(plotdata, x$id)
  } else if (id_type==".id_new") {
    plotdata <- subset(x$data, .id_new %in% include)
    setkey(plotdata, .id_new)
  } else if (id_type==".id_pair") {
    plotdata <- subset(x$data, .id_pair %in% include)
    setkey(plotdata, .id_pair)
  } else {
    stop("'id_type' must be one of x$id, '.id_new' or '.id_pair'.",
         call.=FALSE)
  }
  plotdata[, .id_new := factor(.id_new, levels=.id_new)]

  # set event-time / status if there is only one
  if (length(x$info$added_event_times)==0) {
    stop("Timelines can only be plotted if at least one event was",
         " specified in the 'outcomes' argument of match_time() or added",
         " to the match_time object using the add_outcome() function.",
         call.=FALSE)
  } else if (length(x$info$added_event_times)==1 & missing(time_name)) {
    time_name <- x$info$added_event_times
    status_name <- x$info$added_status
  } else if (missing(time_name)) {
    stop("Arguments 'time_name' and 'status_name' must be specified when",
         " more than one event is present in 'x'.", call.=FALSE)
  }

  # which way to color the lines and dots
  if (missing(include) | id_type==".id_new") {
    mapping <- ggplot2::aes(y=.data$.id_new)
  } else if (id_type==x$id) {
    mapping <- ggplot2::aes(y=.data$.id_new, color=factor(.data[[x$id]]))
  } else if (id_type==".id_pair") {
    mapping <- ggplot2::aes(y=.data$.id_new, color=.data$.id_pair)
  }

  p <- ggplot2::ggplot(plotdata, mapping)

  # lines from inclusion time until event-time
  p <- p + ggplot2::geom_segment(ggplot2::aes(yend=.data$.id_new,
                                 x=.data$.treat_time,
                                 xend=.data[[time_name]] + .data$.treat_time),
                                 linetype=linetype,
                                 linewidth=linewidth)

  # dot for next treatment time
  if (next_treat_point) {
    p <- p + ggplot2::geom_point(data=subset(plotdata,
                                             !is.na(.next_treat_time)),
                                 ggplot2::aes(x=.data$.next_treat_time),
                                 shape=shape_next_treat,
                                 size=size)
  }

  # dot for whether event status is TRUE
  if (outcome_point) {
    p <- p + ggplot2::geom_point(data=subset(plotdata, get(status_name)==TRUE),
                                 ggplot2::aes(x=.data[[time_name]] +
                                                .data$.treat_time),
                                 size=size, shape=shape_outcome)
  }

  # dot for whether treatment was initiated
  if (treat_point) {
    p <- p + ggplot2::geom_point(data=subset(plotdata, .treat==TRUE),
                                 ggplot2::aes(x=.data$.treat_time),
                                 shape=shape_treat, size=size)
  }

  p <- p + ggplot2::labs(x=xlab, y=ylab, color=id_type) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  return(p)
}
