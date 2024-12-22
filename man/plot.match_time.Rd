\name{plot.match_time}
\alias{plot.match_time}

\title{
Plot cumulative number of matched cases and controls
}
\description{
Given a \code{match_time} object created using the \code{\link{match_time}} function, this \code{plot} method displays the cumulative number of included cases, the cumulative number of matched controls and the number of potential controls available over the observed time range.
}
\usage{
\method{plot}{match_time}(x,
     include=c("cases", "controls", "potential_controls"),
     xlab="Time", ylab="Number",
     gg_theme=ggplot2::theme_bw(),
     linealpha=1, linetype="solid", linewidth=0.5,
     ...)
}
\arguments{
  \item{x}{
A \code{match_time} object created using the \code{\link{match_time}} function.
  }
  \item{include}{
A character vector including one or more of \code{"cases"}, \code{"controls"} and \code{"potential_controls"}. Controls whether the plot should contain the cumulative number of cases, cumulative number of controls and number of potential controls at t respectively.
  }
  \item{xlab}{
A single character string specifying the label of the x-axis, defaults to \code{"Time"}.
  }
  \item{ylab}{
A single character string specifying the label of the y-axis, defaults to \code{"Number"}.
  }
  \item{gg_theme}{
A \code{ggplot2} theme that will be used for the plot.
  }
  \item{linealpha}{
A single numeric value specifying the transparency level of the lines.
  }
  \item{linetype}{
A single character string specifying the type of the drawn lines.
  }
  \item{linewidth}{
A single numeric value specifying the width of the drawn lines.
  }
  \item{...}{
Currently not used.
  }
}
\author{
Robin Denz
}
\details{
If \code{ratio=1} was used and the matching was successful for all cases, the cumulative number of cases and the cumulative number of events will be exactly the same for all points in time, meaning that the curves will be on top of each other. The plot method therefore offers a simple and fast way to check whether (and if so, when) issues with finding suitable controls ocurred. It uses only the \code{$trace} object included in the input \code{x} to create the plot. Since it returns a standard \code{ggplot2} object, it can be easily customized by users.

Note that the displayed number of cases is the \emph{actual} number of cases at t in the supplied data, which fufilled the inclusion criteria (if any) at that point in time, \emph{not} the number of cases that received any matches at that point in time.
}
\value{
Returns a \code{ggplot2} object.
}
\seealso{
\code{\link{match_time}}, \code{\link[MatchIt]{summary.matchit}}, \code{\link[MatchIt]{matchit}} \code{\link[cobalt]{bal.tab}}, \code{\link[cobalt]{love.plot}}
}
\examples{
library(data.table)
library(MatchTime)

# only execute if packages are available
if (requireNamespace("survival") & requireNamespace("ggplot2") &
    requireNamespace("MatchIt")) {

library(survival)
library(ggplot2)
library(MatchIt)

# set random seed to make the output replicably
set.seed(1234)

# load "heart" data from survival package
data("heart")

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
out <- match_time(transplant ~ age + surgery, data=heart, id="id",
                  match_method="nearest")

# plot all three statistics over time
# NOTE: here we can see that the green line (cum. no. matched controls)
#       and the red line (cum. no. cases) is exactly the same up until
#       around t = 45, at which point no controls are left (blue line at 0)
#       the number of cases still grows, but no controls are matched anymore
plot(out)

# plot only controls and cases
plot(out, include=c("cases", "controls"))
}
}
