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
  perc_inclusion=TRUE,
  perc_inclusion_total=FALSE,
  perc_other=FALSE,
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
  ylim=c(-6, 11)
)
}
\arguments{
  \item{x}{
A \code{match_time} object created using the \code{\link{match_time}} function.
  }
  \item{digits}{
A single integer, specifying the number of digits the percentages should be rounded to.
  }
  \item{n_fontface}{
Either \code{TRUE} or \code{FALSE}, specifying whether the "n = X" texts in the main boxes should be \code{"normal"}, \code{"italic"} or \code{"bold"}.
  }
  \item{inclusion_text}{
A named list specifying the text that should be used for the exclusion criteria reasons. This list should contain only names that have been used as inclusion criteria in the \code{inclusion} argument of the \code{\link{match_time}} function.
  }
  \item{remove_0_lines}{
Either \code{TRUE} or \code{FALSE}, specifying whether lines about 0 occurrences should be removed from the secondary boxes.
  }
  \item{remove_0_boxes}{
Either \code{TRUE} or \code{FALSE}, specifying whether boxes that are basically empty because of 0 occurrences should be removed from the plot.
  }
  \item{perc_inclusion}{
Either \code{TRUE} or \code{FALSE}, specifying whether percentages should be shown for the individual exclusion criteria reasons.
  }
  \item{perc_inclusion_total}{
Either \code{TRUE} or \code{FALSE}, specifying whether percentages should be shown for total numbers of individuals excluded because of the inclusion criteria.
  }
  \item{perc_other}{
Either \code{TRUE} or \code{FALSE}, specifying whether percentages should be shown for all numbers in the side-boxes that are not related to the inclusion criteria.
  }
  \item{box_main_halign}{
Passed to the \code{halign} aesthetic in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_nudge_x}{
Passed to the \code{nudge_x} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_nudge_y}{
Passed to the \code{nudge_y} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_padding}{
Passed to the \code{box.padding} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_margin}{
Passed to the \code{box.margin} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_r}{
Passed to the \code{box.r} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_width}{
Passed to the \code{width} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_minwidth}{
Passed to the \code{minwidth} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_maxwidth}{
Passed to the \code{maxwidth} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_height}{
Passed to the \code{height} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_minheight}{
Passed to the \code{minheight} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_maxheight}{
Passed to the \code{maxheight} argument in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_colour}{
Passed to the \code{box.colour} aesthetic in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_main_fill}{
Passed to the \code{fill} aesthetic in the \code{\link[ggtext]{geom_textbox}} function used for the main boxes.
  }
  \item{box_sec_halign}{
Passed to the \code{halign} aesthetic in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_nudge_x}{
Passed to the \code{nudge_x} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_nudge_y}{
Passed to the \code{nudge_y} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_padding}{
Passed to the \code{box.padding} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_margin}{
Passed to the \code{box.margin} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_r}{
Passed to the \code{box.r} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_width}{
Passed to the \code{width} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_minwidth}{
Passed to the \code{minwidth} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_maxwidth}{
Passed to the \code{maxwidth} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_height}{
Passed to the \code{height} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_minheight}{
Passed to the \code{minheight} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_maxheight}{
Passed to the \code{maxheight} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_colour}{
Passed to the \code{box.colour} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{box_sec_fill}{
Passed to the \code{fill} argument in the \code{\link[ggtext]{geom_textbox}} function used for the secondary boxes.
  }
  \item{line_main_colour}{
The colour of the main lines connecting the main boxes.
  }
  \item{line_main_linetype}{
The type of the lines connecting the main boxes.
  }
  \item{line_main_linewidth}{
The width of the lines connecting the main boxes.
  }
  \item{line_sec_colour}{
The color of the lines connecting the secondary boxes to the main lines.
  }
  \item{line_sec_linetype}{
The type of the lines connecting the secondary boxes to the main lines.
  }
  \item{line_sec_linewidth}{
The width of the lines connecting the secondary boxes to the main lines.
  }
  \item{arrow}{
Either \code{TRUE} or \code{FALSE}, specifying whether to draw arrows on the main lines.
  }
  \item{arrow_type}{
The type of the arrow heads drawn, ignored if \code{arrow=FALSE}.
  }
  \item{arrow_vjust}{
A single number that may be used to shift the arrow heads up or down. This is probably needed in order to correctly place the arrows. If this does not work, users are encouraged to set \code{arrow=FALSE} and add their own arrows using the \code{\link[ggplot2]{geom_segment}} function.
  }
  \item{arrow_size}{
The size of the drawn arrow heads, ignored if \code{arrow=FALSE}.
  }
  \item{xlim}{
The x limits of the plot.
  }
  \item{ylim}{
The y limits of the plot.
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
library(data.table)
library(MatchTime)

set.seed(12341234)

if (requireNamespace("survival") & requireNamespace("ggplot2") &
    requireNamespace("ggplot2") & requireNamespace("ggtext")) {

library(ggtext)
library(ggplot2)
library(survival)

# load heart dataset from survival package
data("heart", package="survival")

# add some random variables that will be used as inclusion criteria
# (in reality you would already have them)
heart$A <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.9, 0.1))
heart$B <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.9, 0.1))
heart$C <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.9, 0.1))

# perform some time-dependent matching with inclusion criteria
m_objs <- match_time(transplant ~ surgery, data=heart, id="id",
                     match_method="fast_exact", method="brsm",
                     replace_over_t=TRUE, ratio=1,
                     inclusion=c("A", "B", "C"))

# plot the flowchart
plot_flowchart(m_objs)
}
}
