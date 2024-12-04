
## plot number of cases and number of controls over time
#' @export
plot.match_td <- function(x,
                          include=c("cases", "controls", "potential_controls"),
                          ...) {

  new_cases <- matched_controls <- variable <- time <- value <- NULL

  requireNamespace("ggplot2")

  x <- copy(x$trace)

  # create plotdata
  x[, new_cases := cumsum(new_cases)]
  x[, matched_controls := cumsum(matched_controls)]
  plotdata <- melt.data.table(x, id.vars="time")

  rel_cols <- c()
  if ("cases" %in% include) {
    rel_cols <- c(rel_cols, "new_cases")
  }
  if ("controls" %in% include) {
    rel_cols <- c(rel_cols, "matched_controls")
  }
  if ("potential_controls" %in% include) {
    rel_cols <- c(rel_cols, "potential_controls")
  }

  plotdata <- subset(plotdata, variable %in% rel_cols)
  plotdata[variable=="new_cases", variable := "Cumulative number of cases"]
  plotdata[variable=="matched_controls",
           variable := "Cumulative number of matched controls"]
  plotdata[variable=="potential_controls",
           variable := "Potential Controls at t"]

  # plot it
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=time, y=value,
                                              color=variable)) +
    ggplot2::geom_step() +
    ggplot2::theme_bw() +
    ggplot2::labs(x="Time", y="Number") +
    ggplot2::theme(legend.position="bottom",
                   legend.title=ggplot2::element_blank())
  return(p)
}
