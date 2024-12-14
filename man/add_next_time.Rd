\name{add_next_time}
\alias{add_next_time}

\title{
Add the next time of an event to a \code{match_time} object
}
\description{
In some cases, some relevant events occur after the time at which a case was included during matching using the \code{\link{match_time}} function. These times may correspond to some censoring event, such as time to death etc. This function allows users to add this time directly to the matched data. Contrary to the \code{\link{add_outcome}} function, it only adds one column to the data that actually contains the time of the "event", not the time until the event as measured from study entry.
}
\usage{
add_next_time(x, data, id=x$id, time=x$time,
              include_same_t=TRUE,
              name=".next_time")
}
\arguments{
  \item{x}{
A \code{match_time} object created using the \code{\link{match_time}} function.
  }
  \item{data}{
A \code{data.table} like object including exactly two columns: \code{id} (the unique case identifier), \code{time} (the time at which an event occurred). May also be any object that can be coerced to be a \code{data.table}, such as a \code{data.frame} or a \code{tibble}. The time of the next event per case will be added to the matched data. Cases with no events can simply be omitted from \code{data}. If multiple events per person exist, they should be included in the long-format (multiple rows per \code{id}). Only the time of the first event after inclusion into the matching data will be added, all previous events and all events afterwards are ignored.
  }
  \item{id}{
A single character string specifying a column in \code{data}, specifying the unique case identifier. By default the same name that was used in the original \code{\link{match_time}} is used here.
  }
  \item{time}{
A single character string specifying a column in \code{data}, specifying the column containing the event times. By default the same name that was used in the original \code{\link{match_time}} is used here.
  }
  \item{include_same_t}{
Either \code{TRUE} or \code{FALSE}, specifying whether the time of inclusion (\code{.treat_time} in \code{x$data}) should be included when adding the next event time. If \code{TRUE} (default), an event happening exactly at the time of inclusion will be considered the next event. If \code{FALSE}, an event happening exactly at the time of inclusion will be considered a "past event" instead. In the latter case, only events strictly after inclusion time are added to the matched data.
  }
  \item{name}{
A single character string specifying the name of the column containing the next event time that will be added to the \code{data} object contained in \code{x}. Defaults to \code{.next_time}. If the name is already present, an error message is returned instead.
  }
}
\details{
On first glance it might be confusing whether to use \code{add_next_time} or \code{\link{add_outcome}}, but the distinction is actually quite simple. Use \code{add_next_time} whenever you want to add the \emph{actual time of occurence of the next event after inclusion time} and use \code{\link{add_outcome}} whenever you are interested in adding the \emph{time until the next event as measured from the inclusion time} (possible subject to censoring). As the name suggests, the latter is usually used for time-to-event outcomes, while the former is usually used to add extra information required to add more sorts of right-censoring etc.
}
\value{
Returns a modified \code{match_time} object. It is essentially the same object as the supplied \code{x}, but it also contains a new column: \code{name} (the time of occurence of the next event).
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
# NOTE: these are not all events in the real "heart" data and is merely used
#       for showcasing the functionality of add_next_time()
d_events <- data.table(id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       time=c(50, 6, 16, 39, 18, 3, 675, 40, 85, 58))

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
m_obj <- match_time(transplant ~ age + surgery, data=heart, id="id",
                    match_method="nearest")

# add the time of next event to match_time object
m_obj <- add_next_time(m_obj, data=d_events, time="time")
head(m_obj$data)
}
}
