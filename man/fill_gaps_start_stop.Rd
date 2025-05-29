\name{fill_gaps_start_stop}
\alias{fill_gaps_start_stop}

\title{
Add missing intervals to incomplete start-stop data
}
\description{
Some start-stop datasets may be missing some intervals for some cases. This function adds those intervals to the data, which might be needed for further processing.
}
\usage{
fill_gaps_start_stop(data, id, start="start", stop="stop",
                     first_time=NULL, last_time=NULL,
                     missing_indicator=TRUE, ...)
}
\arguments{
  \item{data}{
A \code{data.table} like object including at least three columns: \code{id} (the unique case identifier), \code{start} (the beginning of the time-interval) and \code{stop} (the end of the time-interval). May also be any object that can be coerced to be a \code{data.table}, such as a \code{data.frame} or a \code{tibble}. Intervals should be coded as \code{(start, stop]}.
  }
  \item{id}{
A single character string specifying a column in \code{id} specifying the unique case identifier.
  }
  \item{start}{
A single character string specifying a column in \code{data} specifying the beginning of a time-interval. Defaults to \code{"start"}.
  }
  \item{stop}{
A single character string specifying a column in \code{data} specifying the ending of a time-interval. Defaults to \code{"stop"}.
  }
  \item{first_time}{
Corresponds to the argument of the same name in the \code{\link{merge_start_stop}} function.
  }
  \item{last_time}{
Corresponds to the argument of the same name in the \code{\link{merge_start_stop}} function.
  }
  \item{missing_indicator}{
Either \code{TRUE} (default) or \code{FALSE}, specifying whether a logical column should be added to the data which includes an indicator whether the corresponding interval was present in the original data or not. If \code{TRUE}, this column will be added using the name \code{".in_data"}. This column is \code{TRUE} if the interval was present and \code{FALSE} if it was not.
  }
  \item{...}{
Further arguments passed to the \code{\link{merge_start_stop}} function.
  }
}
\details{
If the input data does not have any missing intervals, the input stays unchanged. If missing intervals are added, they have a value of \code{NA} in all columns other than \code{start}, \code{stop} and \code{id}.

Internally, this function simply calls the \code{\link{merge_start_stop}} function with appropriate input argument (merging the input \code{data} with an essentially empty second dataset using \code{all=TRUE}). As such, all arguments of the \code{\link{merge_start_stop}} function, such as setting default values using the \code{defaults} argument may also be used here.
}
\value{
Returns a single \code{data.table} containing the start-stop data with all missing intervals added.
}
\author{
Robin Denz
}
\seealso{
\code{\link{merge_start_stop}}, \code{\link{subset_start_stop}}, \code{\link{simplify_start_stop}}
}
\examples{
library(MatchTime)
library(data.table)

## define some example data
# this dataset is missing two intervals: (24, 28] for id = 1 and
# (245, 343] for id = 2
data <- data.table(id=c(1, 1, 1, 2, 2, 3),
                   start=c(0, 28, 66, 25, 343, 10),
                   stop=c(24, 66, 143, 245, 1233, 3214),
                   A=c(10, 122, 3434, 223, 233, 0.46))

# add those intervals back
out <- fill_gaps_start_stop(data, id="id")
print(out)

# add the missing intervals, but also add missing intervals from 0 to first
# observed value per id
out <- fill_gaps_start_stop(data, id="id", first_time=0)
print(out)
}
