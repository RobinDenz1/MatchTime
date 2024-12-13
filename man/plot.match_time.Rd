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
     ...)
}
\arguments{
  \item{x}{
A \code{match_time} object created using the \code{\link{match_time}} function.
  }
  \item{include}{
A character vector including one or more of \code{"cases"}, \code{"controls"} and \code{"potential_controls"}. Controls whether the plot should contain the cumulative number of cases, cumulative number of controls and number of potential controls at t respectively.
  }
  \item{...}{
Currently not used.
  }
}
\author{
Robin Denz
}
\details{
If \code{ratio=1} was used and the matching was successful for all cases, the cumulative number of cases and the cumulative number of events will be exactly the same for all points in time, meaning that the curves will be on top of each other. The plot method therefore offers a simple and fast way to check whether (and if so, when) issues with finding suitable controls ocurred. It uses only the \code{$trace} object included in the input \code{x} to create the plot. If users want to customize the plot, it is probably easiest to just use that object to create a personalized plot. This plot function is mostly used for data analysis purposes, not to be publication ready.
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
if (requireNamespace("survival") & requireNamespace("ggplot2")) {

library(survival)
library(ggplot2)

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
