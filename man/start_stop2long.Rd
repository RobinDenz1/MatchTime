\name{start_stop2long}
\alias{start_stop2long}

\title{
Transform a start-stop dataset into the long-format
}
\description{
Given a \code{data.table} like object in the start-stop format, this function returns a \code{data.table} in the long-format.
}
\usage{
start_stop2long(data, id, events=NULL, start="start",
                stop="stop", add_missing_intervals=FALSE,
                include_last_t=FALSE, time_name="time",
                ...)
}
\arguments{
  \item{data}{
A \code{data.table} like object including at least three columns: \code{id} (the unique case identifier), \code{start} (the beginning of the time-interval) and \code{stop} (the end of the time-interval). May also be any object that can be coerced to be a \code{data.table}, such as a \code{data.frame} or a \code{tibble}. Intervals should be \emph{right-open} (coded as \code{[start, stop)}) and thus overlapping . May contain any number of additional columns.
  }
  \item{id}{
A single character string specifying a column in \code{data} specifying the unique case identifier.
  }
  \item{events}{
Either \code{NULL} (default) or a character vector specifying variable names in \code{data}. The columns specified by this argument should be logical and are considered to be event indicators, meaning that they are not coded as time-varying variables. Instead they should be coded as occurring exactly on \code{stop} and have no duration themselves. In the long-format output, these columns will only be \code{TRUE} on the time of occurrence, not during the interval in which they were coded.
  }
  \item{start}{
A single character string specifying a column in \code{data} specifying the beginning of a time-interval. Defaults to \code{"start"}.
  }
  \item{stop}{
A single character string specifying a column in \code{data} specifying the ending of a time-interval. Defaults to \code{"stop"}.
  }
  \item{add_missing_intervals}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether intervals that are missing from \code{data} should still be present in the output. If set to \code{TRUE}, the \code{\link{add_missing_intervals}} function is called on the input \code{data} first.
  }
  \item{include_last_t}{
Whether to include the last value of \code{stop} per \code{id} in the output. Whether this should be done or not depends on how the intervals are coded.
  }
  \item{time_name}{
A single character string, specifying the name of the "time" column in the output.
  }
  \item{...}{
Further arguments passed to \code{\link{add_missing_intervals}} if \code{add_missing_intervals=TRUE}, ignored otherwise.
  }
}
\value{
Returns a single \code{data.table} containing the long-format data. The \code{start} and \code{stop} columns from the input are replaced by a single \code{time_name} column.
}
\author{
Robin Denz
}
\seealso{
\code{\link{add_missing_intervals}}, \code{\link{long2start_stop}}
}
\examples{
library(MatchTD)
library(data.table)

# define some example start-stop data
data <- data.table(id=c(1, 1, 1, 2, 2, 3),
                   start=c(0, 14, 26, 0, 18, 0),
                   stop=c(14, 26, 30, 18, 32, 51),
                   A=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                   B=c(1L, 1L, 2L, 3L, 5L, 6L),
                   C=c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
                   D=c("A", "B", "C", "D", "E", "F"))

# transform to long-format
out <- start_stop2long(data, id="id")
head(out)

# if C was coded as an event instead, we would want to use:
out <- start_stop2long(data, id="id", events="C")
head(out)
}
