\name{simplify_start_stop}
\alias{simplify_start_stop}

\title{
Combines rows with the same values in start-stop data
}
\description{
Given a \code{data.table}-like object containing information in the start-stop format, this function searches for consecutive intervals where values of specific covariates do not change and "simplifies" the dataset by combining these interval into one interval. This may be useful to reduce RAM usage and computation time when dealing with large start-stop data.
}
\usage{
simplify_start_stop(data, id, start="start", stop="stop",
                    cols, remove_other_cols=TRUE)
}
\arguments{
  \item{data}{
A \code{data.table} like object including at least four columns: \code{id} (the case identifier), \code{start} (the beginning of the time-interval), \code{stop} (the end of the time-interval) and one or more arbitrary columns. May also be any object that can be coerced to be a \code{data.table}, such as a \code{data.frame} or a \code{tibble}. Intervals should be \emph{right-open} (e.g. coded as \code{[start, stop)}).
  }
  \item{id}{
A single character string specifying a column in \code{data} containing the case identifiers.
  }
  \item{start}{
A single character string specifying a column in \code{data} specifying the beginning of a time-interval. Defaults to \code{"start"}.
  }
  \item{stop}{
A single character string specifying a column in \code{data} specifying the ending of a time-interval. Defaults to \code{"stop"}.
  }
  \item{cols}{
A character vector specifying the columns that should be used to check whether the intervals are unique. If not specified, all columns other than \code{id}, \code{start} and \code{stop} will be used.
  }
  \item{remove_other_cols}{
Either \code{TRUE} or \code{FALSE}, specifying whether the columns \emph{not} named in the \code{cols} argument (other than \code{id}, \code{start}, \code{stop}) should be removed from the output. Defaults to \code{TRUE}, because keeping these columns may be misleading. If set to \code{FALSE}, please remember that the value of these columns is not neccesarily correct, since intervals have been combined without looking at their values first.
  }
}
\details{
The intervals defined by the \code{start} and \code{stop} columns are expected to be coded as \code{[start, stop)}, meaning that the value of \code{start} must always be equal to the value of \code{stop} in the previous row. Intervals of length 0 are not supported and will produce an error message.

Note that if the input \code{data} contains events, users probably want to exclude these event columns from the \code{cols} argument. The reason is that the \code{data} may contain consecutive intervals that are indeed exactly the same, but refer to two separate events (because intervals always end when an event indicator is \code{TRUE}).
}
\value{
Returns a single \code{data.table} containing the simplified dataset.
}
\author{
Robin Denz
}
\seealso{
\code{\link{merge_start_stop}}, \code{\link{add_missing_intervals}}, \code{\link{subset_start_stop}}
}
\examples{
library(MatchTD)
library(data.table)

# get some fake example data
data1 <- data.table(id=1,
                    start=c(1, 20, 35, 120, 923, 1022, 2000, 3011),
                    stop=c(20, 35, 120, 923, 1022, 2000, 3011, 3013),
                    A=c(0, 0, 0, 1, 1, 0, 0, 0),
                    B=c(1, 0, 0, 1, 0, 0, 0, 0),
                    C=c(11, 0.2, 17.8, 2.1, 9.0001, 1.2, 33, 22))
data2 <- data.table(id=2,
                    start=c(1, 20, 35, 120, 923),
                    stop=c(20, 35, 120, 923, 1022),
                    A=c(0, 0, 1, 1, 1),
                    B=c(1, 0, 0, 1, 0),
                    C=c(11, 0.2, 17.8, 2.1, 9.0001)+1)
data <- rbind(data1, data2)

# simplify in regards to columns "A" and "B"
out <- simplify_start_stop(data, id="id", cols=c("A", "B"))
print(out)

# simplify in regards to column "A" only
out <- simplify_start_stop(data, id="id", cols="A")
print(out)

# calling it without specifying "cols" results in no changes,
# because C always changes over the defined intervals
out <- simplify_start_stop(data, id="id")
print(out)
}
