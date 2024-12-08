\name{plot_timeline}
\alias{plot_timeline}

\title{
Plot the time under observation for ids in a \code{match_td} object
}
\description{
Given a \code{match_td} object created using the \code{\link{match_td}} function, this \code{plot_timeline} function displays the time under observation of ids in relation to an event added using the \code{\link{add_outcome}} function. This function may be used to visually explain the matching process or the used censoring method.
}
\usage{
plot_timeline(x, include, id_type=x$id, time_name,
              status_name, treat_point=TRUE,
              outcome_point=TRUE, next_treat_point=TRUE,
              linetype="solid", linewidth=1,
              size=3, shape_treat=18, shape_outcome=16,
              shape_next_treat=8, xlab="Time",
              ylab=".id_new", legend.position="right",
              gg_theme=ggplot2::theme_bw(), warn=TRUE)
}
\arguments{
  \item{x}{
A \code{match_td} object created using the \code{\link{match_td}} function.
  }
  \item{include}{
An optional numeric vector of ids that should be displayed in the plot. If this argument is not specified, the timelines for all \code{.id_new} in the matched data will be displayed. Even for small sample sizes this might not be the best option, which is why a warning message is returned by default if \code{warn=TRUE}. Which of the three possible ids in \code{x$data} should be used should be specified using the \code{id_type} argument.
  }
  \item{id_type}{
There are three id variables in \code{x$data}: \code{x$id} (the original person identifier), \code{.id_new} (an id identifying separate rows in the final matched data) and \code{.id_pair} (an id identifying the matched pairs). By specifying this argument, users can choose which ids the \code{include} argument refers to. The y-axis will always display the \code{.id_new} id, but the plot will only include the ids mentioned in \code{include} of type \code{id_type}.
  }
  \item{time_name}{
A single character string, specifying the name of a time-to-event variable added to \code{x} using the \code{\link{add_outcome}} function. This event time will be used as the end of the id specific timeline.
  }
  \item{status_name}{
A single character string, specifying the name of the status of a time-to-event variable added to \code{x} using the \code{\link{add_outcome}} function.
  }
  \item{treat_point}{
Either \code{TRUE} or \code{FALSE}, specifying whether a point should be added whenever a person received the treatment at inclusion time (essentially marking all cases).
  }
  \item{outcome_point}{
Either \code{TRUE} or \code{FALSE}, specifying whether a point should be added whenever a person experiences an event.
  }
  \item{next_treat_point}{
Either \code{TRUE} or \code{FALSE}, specifying whether a point should be added whenever a person receives the treatment \emph{after} being included during the matching process.
  }
  \item{linetype}{
A single character string specifying the linetype of the displayed lines.
  }
  \item{linewidth}{
A single number specifying the width of the displayed lines.
  }
  \item{size}{
A single number specifying the size of the drawn points (if any).
  }
  \item{shape_treat}{
The shape of the point drawn if \code{treat_point=TRUE}.
  }
  \item{shape_outcome}{
The shape of the point drawn if \code{outcome_point=TRUE}.
  }
  \item{shape_next_treat}{
The shape of the point drawn if \code{next_treat_point=TRUE}.
  }
  \item{xlab}{
A single character string specifying the label of the x-axis.
  }
  \item{ylab}{
A single character string specifying the label of the y-axis.
  }
  \item{legend.position}{
A single character string specifying the position of the label.
  }
  \item{gg_theme}{
A \code{ggplot2} theme to be added to the plot.
  }
  \item{warn}{
Either \code{TRUE} or \code{FALSE}, specifying whether a warning should be printed if \code{include} is not specified.
  }
}
\author{
Robin Denz
}
\details{
The displayed timelines always start at the "time-zero" defined by the matching process (\code{.treat_time} in \code{x$data}). How far the lines extend depends on the argument used when calling the \code{\link{add_outcome}} function.
}
\value{
Returns a \code{ggplot2} object.
}
\seealso{
\code{\link{match_td}}, \code{\link{add_outcome}}
}
\examples{
library(data.table)
library(MatchTD)

# only execute if packages are available
if (requireNamespace("survival") & requireNamespace("ggplot2")) {

library(survival)
library(ggplot2)

# set random seed to make the output replicably
set.seed(1234)

# load "heart" data from survival package
data("heart")

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
out <- match_td(transplant ~ age + surgery, data=heart, id="id",
                match_method="nearest", replace_over_t=TRUE)

# suppose we had an extra dataset with events that looks like this
# NOTE: these are not all events in the real "heart" data and is merely used
#       for showcasing the functionality of add_outcome()
d_events <- data.table(id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       time=c(50, 6, 16, 39, 18, 3, 675, 40, 85, 58))

# add the outcome to the match_td object
out <- add_outcome(out, data=d_events, time="time", censor_at_treat=TRUE)

# plot the timeline for the original ids
# NOTE: here we see that id = 4 occurs 3 times in the matched dataset
#       1.) as a case, starting from ~ t = 36 (.id_new = 89)
#       2.) as a control, starting from ~ t = 23 (.id_new = 63)
#       3.) as a control, starting from ~ t = 2 (.id_new = 5)
#       This is possible because replace_over_t=TRUE was used
plot_timeline(out, include=c(1, 2, 3, 4, 5),
              time_name=".event_time", status_name=".status")
}
}
