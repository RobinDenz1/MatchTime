\name{times_from_start_stop}
\alias{times_from_start_stop}

\title{
Extract "event" times from start-stop format datasets
}
\description{
Given a \code{data.table} like object in the start-stop format, it returns a new \code{data.table} containing the times at which events of a particular type happened.
}
\usage{
times_from_start_stop(data, id, name, type, start="start",
                      stop="stop", time_name="time")
}
\arguments{
  \item{data}{
A \code{data.table} like object including at least three columns: \code{id} (the unique case identifier), \code{start} (the beginning of the time-interval) and \code{stop} (the end of the time-interval). May also be any object that can be coerced to be a \code{data.table}, such as a \code{data.frame} or a \code{tibble}. Intervals should be coded as \code{[start, stop)}, like in all other functions of this package.
  }
  \item{id}{
A single character string specifying the column containing the unique case identifier.
  }
  \item{name}{
A single character string specifying the "event" column in \code{data}. The specified column should be of class "logical" (containing only either \code{TRUE} or \code{FALSE}). Alternatively, the specified variable may be a numeric variable containing only 0 (considered \code{FALSE}) and 1 (considered \code{TRUE}).
  }
  \item{type}{
A single character string specifying which type of variable the column specified by \code{name} is. If the variable is an actual event, meaning that existing intervals end at the exact time that \code{name} occured, it should be set to \code{type="event"}. In this case, the \code{stop} value of all intervals where \code{name} is \code{TRUE} are extracted. If the variable refers to a time-varying binary variable instead (for example a time-dependent exposure that can be present or absent), it should be set to \code{"var"}, in which case the \code{start} time of each duration where \code{name} was \code{TRUE} are extracted. See details.
  }
  \item{start}{
A single character string specifying a column in \code{data} specifying the beginning of a time-interval. Defaults to \code{"start"}.
  }
  \item{stop}{
A single character string specifying a column in \code{data} specifying the ending of a time-interval. Defaults to \code{"stop"}.
  }
  \item{time_name}{
A single character string specifying the name that the \code{"time"} variable should have in the output data. Defaults to \code{"time"}.
  }
}
\value{
Returns a single \code{data.table} containing the subsetted start-stop data.
}
\details{

This function may be useful to extract times of occurence of binary time-dependent exposures or actual events from start-stop data.

\strong{\emph{Use on Time-Varying Variables}}:

If \code{type="var"} is used the variable specified by \code{name} is treated as a simple time-varying variable and only the start times of each uninterrupted duration where this variable is \code{TRUE} are extracted. For example, if the variable starts being \code{TRUE} at t = 20 and stops being \code{TRUE} at t = 123 and it was never \code{TRUE} before or after these times, \code{time_name} would simple be 20 for this individual, regardless of how many intervals are present where \code{name} is \code{TRUE}. This is done because it is continuously \code{TRUE} and we only want to extract the initial time where it "occured" or "happened". In this case, if \code{name} goes back to \code{FALSE} and is \code{TRUE} again later, for example at t = 700, the output would contain another entry for this \code{id} including the time 700, because this constitutes another occurence.

\strong{\emph{Use on actual events}}:

If \code{type="event"} is used instead, every single occurence of \code{TRUE} in the input \code{data} is considered to specify a single event occurence in \code{name}, regardless of whether these intervals are directly after one another. This is the classic difference between coding time-varying variables and events in start-stop data, as discussed in the \pkg{survival} package documentation and in the vignettes of this package.

}
\author{
Robin Denz
}
\seealso{
\code{\link{merge_start_stop}}, \code{\link{add_missing_intervals}}, \code{\link{simplify_start_stop}}
}
\examples{
library(MatchTime)
library(data.table)

# define some example start-stop data
data <- data.table(id=c(1, 1, 1, 1, 1, 2, 2, 2),
                   start=c(0, 10, 25, 812, 1092, 90, 9023, 10000),
                   stop=c(10, 25, 812, 1092, 34334, 8021, 9823, 220022),
                   exposure=c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
                              TRUE))

# treating it as an exposure
# NOTE: in this case, the first two rows of id = 1 are considered to be
#       one continuous occurence, because "exposure" stayed TRUE the entire
#       time
out1 <- times_from_start_stop(data, id="id", name="exposure", type="var")
head(out1)

# treating it as an event
# NOTE: in this case the first two rows of id = 1 are considered to be
#       two independent events, events force a time-interval to stop
out2 <- times_from_start_stop(data, id="id", name="exposure", type="event")
head(out2)
}
