\name{add_previous_event_count}
\alias{add_previous_event_count}

\title{
Add a count of the number of events occured before inclusion to a \code{match_time} object
}
\description{
In some cases, some relevant events occur \emph{before} the time at which a case was included during matching using the \code{\link{match_time}} function. This might be an unrelated treatment or any other time-dependent variable with a duration. To count how many such "events" occured in a set duration before inclusion time might be required for further confounder adjustment or other analysis. If these variables are already included in the start-stop data supplied to \code{\link{match_time}}, they will be included in the matched data automatically, in which case this function is not needed. Otherwise, this function offers an easy and fast way to add such a count to the matched data.
}
\usage{
add_previous_event_count(x, data, id=x$id, time=x$time,
                         duration, include_same_t=TRUE,
                         units="auto", name=".prev_event_count")
}
\arguments{
  \item{x}{
A \code{match_time} object created using the \code{\link{match_time}} function.
  }
  \item{data}{
A \code{data.table} like object including exactly two columns: \code{id} (the unique case identifier), \code{time} (the time at which an "event" occurred). May also be any object that can be coerced to be a \code{data.table}, such as a \code{data.frame} or a \code{tibble}. If multiple events per person exist, they should be included in the long-format (multiple rows per \code{id}).
  }
  \item{id}{
A single character string specifying a column in \code{data}, specifying the unique case identifier. By default the same name that was used in the original \code{\link{match_time}} is used here.
  }
  \item{time}{
A single character string specifying a column in \code{data}, specifying the column containing the event times. By default the same name that was used in the original \code{\link{match_time}} is used here.
  }
  \item{duration}{
A single positive number specifying the duration before the inclusion time (\code{.treat_time} in \code{x$data}) in which the events should be counted. For example, setting \code{duration = 20} means that the number of events occuring 20 time units before the inclusion time will be counted.
  }
  \item{include_same_t}{
Either \code{TRUE} or \code{FALSE}, specifying whether the time of inclusion (\code{.treat_time} in \code{x$data}) should be included when adding the next indicator. If \code{TRUE} (default), an event occuring exactly at the time of inclusion will be considered as a previous event, resulting in the added indicator being one point higher. If \code{FALSE}, an event occuring exactly at the time of inclusion will not be considered a previous event.
  }
  \item{units}{
Corresponds to the argument of the same name in the \code{\link{difftime}} function. This argument is only used when the \code{time} column corresponds to a \code{Date} (or similar) variable. It should be used to indicate the time-scale of the \code{duration} (seconds, days, years, ...).
  }
  \item{name}{
A single character string specifying the name of the column containing the count that will be added to the \code{data} object contained in \code{x}. Defaults to \code{.prev_event_count}. If the name is already present, an error message is returned instead.
  }
}
\details{
In most cases it is easier and cleaner to just add all variables to the start-stop data supplied to the \code{data} argument in \code{\link{match_time}}, regardless of whether matching should be performed on these variables or not. This way, they will be present in the output data without any further function calls. In some cases, however, the dataset may be too large to allow all variables to be present. This might be the case when the variable changes at many points in time, requiring many rows per \code{id}. In these cases it might be necessary to add them later to make the matching process possible.
}
\value{
Returns a modified \code{match_time} object. It is essentially the same object as the supplied \code{x}, but it also contains a new column: \code{name} (the count of events that occured \code{duration} time units before the inclusion time).
}
\author{
Robin Denz
}
\seealso{
\code{\link{match_time}}, \code{\link{add_outcome}}
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

# suppose we had an extra dataset with events that looks like this
# NOTE: these are not actual events in the real "heart" data and is merely used
#       for showcasing the functionality of add_previous_event()
d_events <- data.table(id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       time=c(5, 2, 12, 39, 2, 665, 675, 4, 1, 23))

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
m_obj <- match_time(transplant ~ age + surgery, data=heart, id="id",
                    match_method="nearest")

# add the count of events occuring 20 time units before inclusion
# to the match_time object
m_obj <- add_previous_event_count(m_obj, data=d_events, time="time",
                                  duration=20)
head(m_obj$data)
}
}
