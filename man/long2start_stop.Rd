\name{long2start_stop}
\alias{long2start_stop}

\title{
Transform a long-format dataset into the start-stop format
}
\description{
This function transforms a \code{data.table} like object in the long-format (one row per person per time point) to a \code{data.table} in the start-stop format (one row per person-specific period in which no variables changed).
}
\usage{
long2start_stop(data, id, time, varying, start_name="start",
                stop_name="stop")
}
\arguments{
  \item{data}{
A \code{data.table} or an object that can be coerced to a \code{data.table} (such as a \code{data.frame}) including data in the long-format. The supplied \code{data} should include full information for all individuals, e.g. no points in time between the first time and last time per individual should be missing.
  }
  \item{id}{
A single character string specifying a unique person identifier included in in \code{data}.
  }
  \item{time}{
A single character string specifying a time variable included in in \code{data} coded as integers.
  }
  \item{varying}{
A character vector specifying names of variables included in in \code{data} that may change over time.
  }
  \item{start_name}{
A single character string, specifying the name that the \code{"start"} variable should have in the resulting data. Defaults to \code{"start"}.
  }
  \item{stop_name}{
Same as \code{start_name} but for the \code{"stop"} column.
  }
}
\details{
The created start-stop columns are coded as \code{[start, stop)}, meaning that they will be overlapping. If non-overlapping intervals are desired, users can simply substract 1 from the \code{stop} column.

This function relies on \code{data.table} syntax to make the data transformation as RAM efficient and fast as possible.
}
\value{
Returns a \code{data.table} containing the columns \code{id} (the unique person identifier), \code{start_name} (an integer variable encoding the beginning of the intervals), \code{stop} (an integer variable encoding the end of the intervals) and all other variables included in the input \code{data} in the start-stop format.
}
\author{
Robin Denz
}
\examples{
library(simDAG)
library(data.table)

# generate example data in long format
long <- data.table(.id=rep(seq_len(10), each=5),
                   .time=rep(seq_len(5), 10),
                   A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                           TRUE),
                   B=FALSE)
setkey(long, .id, .time)

# transform to start-stop format
long2start_stop(data=long, id=".id", time=".time", varying=c("A", "B"))
}
