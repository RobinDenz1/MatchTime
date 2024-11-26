\name{subset_start_stop}
\alias{subset_start_stop}

\title{
Subsetting start-stop format datasets
}
\description{
Returns subsets of a \code{data.table} like object in the start-stop format. Contrary to the usual \code{\link{subset}} function, this function subsets (and truncates) specific \emph{time-intervals} and does not use a logical expression to subset data based on other column values. May be useful to limit start-stop based datasets to a certain time-range.
}
\usage{
subset_start_stop(data, first_time, last_time,
                  truncate=TRUE, start="start",
                  stop="stop", na.rm=FALSE)
}
\arguments{
  \item{data}{
A \code{data.table} like object including at least two columns: \code{start} (the beginning of the time-interval) and \code{stop} (the end of the time-interval). May also be any object that can be coerced to be a \code{data.table}, such as a \code{data.frame} or a \code{tibble}. Intervals should be coded as \code{[start, stop)}, like in all other functions of this package.
  }
  \item{first_time}{
A single value or a vector of size \code{nrow(data)} of class \code{numeric}, \code{Date} or something similar, specifying the first time that should be kept in the output. All intervals ending before this value will be removed. Additionally, if \code{truncate=TRUE}, all intervals starting before \code{first_time} and ending after \code{first_time} will be truncated to start at \code{first_time}.
  }
  \item{last_time}{
A single value or a vector of size \code{nrow(data)} of class \code{numeric}, \code{Date} or something similar, specifying the last time that should be kept in the output. All intervals beginning before this value will be removed. Additionally, if \code{truncate=TRUE}, all intervals starting before \code{last_time} and ending after \code{last_time} will be truncated to end at \code{last_time}.
  }
  \item{truncate}{
Either \code{TRUE} or \code{FALSE}, controls whether existing intervals should be truncated at \code{first_time} and or \code{last_time}. See the respective arguments for more info.
  }
  \item{start}{
A single character string specifying a column in \code{data} specifying the beginning of a time-interval. Defaults to \code{"start"}.
  }
  \item{stop}{
A single character string specifying a column in \code{data} specifying the ending of a time-interval. Defaults to \code{"stop"}.
  }
  \item{na.rm}{
Either \code{TRUE} or \code{FALSE} (default), controls whether to remove rows where either \code{first_time} or \code{last_time} is \code{NA}.
  }
}
\value{
Returns a single \code{data.table} containing the subsetted start-stop data.
}
\author{
Robin Denz
}
\seealso{
\code{\link{merge_td}}, \code{\link{add_missing_intervals}}, \code{\link{simplify_start_stop}}
}
\examples{
library(MatchTD)
library(data.table)

# define some example start-stop data
data <- data.table(id=c(1, 1, 1, 1, 1, 2, 2, 2),
                   start=c(0, 10, 25, 812, 1092, 90, 9023, 10000),
                   stop=c(10, 25, 812, 1092, 34334, 8021, 9823, 220022),
                   some_col=c(1, 2, 3, 4, 5, 6, 7, 8))

# limit it to the time-range 28 - 1900
out <- subset_start_stop(data, first_time=28, last_time=1900)
print(out)

# don't truncate intervals
out <- subset_start_stop(data, first_time=28, last_time=1900,
                         truncate=FALSE)
print(out)

# only cut-off intervals before t = 28
out <- subset_start_stop(data, first_time=28)
print(out)

# only cut-off intervals after t = 28
out <- subset_start_stop(data, last_time=28)
print(out)

# using different cut-off values for each person
# note that we have to repeat the respective cut-off values as many times
# as each id appears to make this work
out <- subset_start_stop(data, last_time=c(rep(723, 5), rep(815, 3)))
print(out)
}
