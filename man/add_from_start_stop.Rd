\name{add_from_start_stop}
\alias{add_from_start_stop}

\title{
Add information from start-stop data to a \code{match_time} object
}
\description{
This function may be used to add variables saved in the start-stop format to a \code{match_time} object created using the \code{\link{match_time}} function. For example, there might be a time-varying variable that was not included in the original \code{data} used in the \code{\link{match_time}} call to save memory, that should now be added later. Using this function, the value of these variables at \code{.treat_time} will be added to the \code{data} in the \code{match_time} object.
}
\usage{
add_from_start_stop(x, data, variable, id=x$id,
                    start="start", stop="stop", default=NA)
}
\arguments{
  \item{x}{
A \code{match_time} object created using the \code{\link{match_time}} function.
  }
  \item{data}{
A \code{data.table} like object including at least four columns: \code{id} (the unique case identifier), \code{start} (beginning of interval), \code{stop} (end of interval) and one or more columns defined by the \code{variables} argument. May also be any object that can be coerced to be a \code{data.table}, such as a \code{data.frame} or a \code{tibble}. The dataset should be in the same start-stop format that would be required for a regular \code{\link{match_time}} call.
  }
  \item{id}{
A single character string specifying a column in \code{data}, specifying the unique case identifier. By default the same name that was used in the original \code{\link{match_time}} is used here.
  }
  \item{variable}{
A single character string specifying a variable in \code{data} which should be added to the \code{$data} object inside of \code{x}. Note that if \code{x$data} already contains columns with the same names as in \code{variables}, an error will be produced.
  }
  \item{start}{
A single character string specifying a column in \code{data} specifying the beginning of a time-interval. Defaults to \code{"start"}.
  }
  \item{stop}{
A single character string specifying a column in \code{data} specifying the end of a time-interval. Defaults to \code{"stop"}.
  }
  \item{default}{
A single value that should be used as default if no information can be found in \code{data} for the required point in time for some individuals. If the supplied \code{data} does not contain information for some \code{id} at \code{.treat_time}, this default value will be used instead.
  }
}
\details{
Although this function may be useful for very large datasets, if RAM is not an issue we recommend using the \code{\link{merge_start_stop}} or similar functions to create a single start-stop dataset to use in \code{\link{match_time}} instead. This way, all information contained in the start-stop dataset is automatically included in the output as well, without the need to use this function.
}
\value{
Returns a modified \code{match_time} object. It is essentially the same object as the supplied \code{x}, but it also contains a new column: \code{name} (the time of occurence of the next event).
}
\author{
Robin Denz
}
\seealso{
\code{\link{match_time}}
}
\examples{
library(data.table)
library(MatchTime)

# only execute if packages are available
if (requireNamespace("survival") & requireNamespace("MatchIt")) {

library(survival)
library(MatchIt)

# set random seed to make the output replicably
set.seed(1234)

# load "heart" data from survival package
data("heart")
heart <- heart[, c("id", "start", "stop", "transplant", "age", "surgery")]

}
}
