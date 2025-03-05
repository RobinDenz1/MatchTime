\name{plot_flowchart}
\alias{plot_flowchart}

\title{
Plot a flowchart of the sample size flow in a \code{match_time} object
}
\description{
This function is an attempt at creating a CONSORT style flowchart for the sequential time-dependent matching methods implemented in this package. It shows the number of potential cases and potential controls as filtered throughout the matching process. Some of the included numbers may seem confusing at first glance. See details for more information.
}
\usage{
plot_flowchart(
  x,
  digits=2,
  n_fontface="italic",
  inclusion_text=NULL,
  remove_0_lines=TRUE,
  remove_0_boxes=FALSE,
  box_main_halign=0.5,
  box_main_nudge_x=0,
  box_main_nudge_y=0,
  box_main_padding=unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
  box_main_margin=unit(c(0, 0, 0, 0), "pt"),
  box_main_r=unit(5.5, "pt"),
  box_main_width=unit(2, "inch"),
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
  ylim=c(-6, 11)
)
}
\arguments{
  \item{x}{
A \code{match_time} object created using the \code{\link{match_time}} function.
  }
  \item{digits}{

  }
  \item{n_fontface}{

  }
  \item{inclusion_text}{

  }
  \item{remove_0_lines}{

  }
  \item{remove_0_boxes}{

  }
  \item{box_main_halign}{

  }
  \item{box_main_nudge_x}{

  }
  \item{box_main_nudge_y}{

  }
  \item{box_main_padding}{

  }
  \item{box_main_margin}{

  }
  \item{box_main_r}{

  }
  \item{box_main_width}{

  }
  \item{box_main_minwidth}{

  }
  \item{box_main_maxwidth}{

  }
  \item{box_main_height}{

  }
  \item{box_main_minheight}{

  }
  \item{box_main_maxheight}{

  }
  \item{box_main_colour}{

  }
  \item{box_main_fill}{

  }
  \item{box_sec_halign}{

  }
  \item{box_sec_nudge_x}{

  }
  \item{box_sec_nudge_y}{

  }
  \item{box_sec_padding}{

  }
  \item{box_sec_margin}{

  }
  \item{box_sec_r}{

  }
  \item{box_sec_width}{

  }
  \item{box_sec_minwidth}{

  }
  \item{box_sec_maxwidth}{

  }
  \item{box_sec_height}{

  }
  \item{box_sec_minheight}{

  }
  \item{box_sec_maxheight}{

  }
  \item{box_sec_colour}{

  }
  \item{box_sec_fill}{

  }
  \item{line_main_colour}{

  }
  \item{line_main_linetype}{

  }
  \item{line_main_linewidth}{

  }
  \item{line_sec_colour}{

  }
  \item{line_sec_linetype}{

  }
  \item{line_sec_linewidth}{

  }
  \item{arrow}{

  }
  \item{arrow_type}{

  }
  \item{arrow_vjust}{

  }
  \item{arrow_size}{

  }
  \item{xlim}{

  }
  \item{ylim}{

  }
}
\details{

Below are some FAQ about this type of flowchart and the function itself.

\strong{\emph{Why are the shown values so weird?}}

Multiple of the numbers in the resulting flowchart may seem confusing at first. First of all, the number of potential cases + the number of potential controls is (at least at first) usually much larger than the total amount of data available. The reason for this is that all individuals that do not get treated at \eqn{t = 0} can, theoretically, be used as control before they receive the treatment.

Similarly, sometimes some people do not meet multiple inclusion criteria at the same time, resulting in them being counted more than once in the respective list of reasons for inclusion. The percentages in parenthesis (if shown) are therefore in respect to the total number of potential controls / cases still available at time of exclusion, not to the total number of excluded individuals.

Finally, the resulting matched dataset (matched cases + matched controls) may be larger than the total input data, even if \code{replace_over_t=FALSE} and \code{replace_at_t=FALSE}. This is again because, unless \code{replace_cases=FALSE} is used, individuals used as controls may later become cases and are thus included twice in the output data. To get the number of distinct individuals in the resulting data instead, one simply needs to call \code{length(unique(data$id))} on the final matched dataset.

\strong{\emph{How do I make all the text fit on a page?}}

Usually, the resulting plot will look very poorly in the lower right corner of an interactive R studio session. Only when clicking the "zoom" button, a much better version will appear. This is because the plot is usually way too big to fit in the corner. When saving to a file it therefore usually makes sense to make the \code{width} and \code{height} parameters rather large to get the same effect. Users are encouraged to play around with these parameters. Additional optimisations can be done through the various arguments relating to the boxes.

\strong{\emph{Can I add more boxes?}}

When using this function for a publication, it might make sense to add more boxes below the plot with additional information. This may be done using regular \pkg{ggplot2} syntax. The boxes can be created using the \code{\link[ggtext]{geom_textbox}} function from the \pkg{ggtext} package. Lines and arrows are drawn simply using the \code{\link[ggplot2]{geom_segment}} function.

\strong{\emph{Why are the arrows placed so poorly?}}

Because the actual width of the boxes is dependent on the size of the plot and internally the boxes and lines are independent of one another, there is no simple way to make the arrows fit right on top of the boxes naturally. Users can usually create the desired effect by changing the \code{arrow_hjust} argument through trial and error. If this fails, users may set \code{arrow=FALSE} and add their own arrows using the \code{\link[ggplot2]{geom_segment}} function.

}
\value{
Returns a standard \code{ggplot2} object that can be saved with a \code{\link[ggplot2]{ggsave}} call.
}
\author{
Robin Denz
}
\seealso{
\code{\link{match_time}}, \code{\link{plot.match_time}}
}
\examples{

}
