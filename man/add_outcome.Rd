\name{add_outcome}
\alias{add_outcome}

\title{
Add a Time-to-Event Outcome to a \code{match_td} object
}
\description{
After performing time-dependent matching using the \code{\link{match_td}} function, users usually want to add one or more outcomes to the data. This function may be used to do this efficiently, potentially using different censoring schemes.
}
\usage{
add_outcome(x, data, censor_at_treat=TRUE,
            censor_pairs=FALSE, units="auto",
            id=x$id, time=x$time,
            event_time_name=".event_time",
            status_name=".status", copy_data=TRUE)
}
\arguments{
  \item{x}{
A \code{match_td} object created using the \code{\link{match_td}} function.
  }
  \item{data}{
A \code{data.table} like object including exactly two columns: \code{id} (the unique case identifier), \code{time} (the time at which an event occurred). May also be any object that can be coerced to be a \code{data.table}, such as a \code{data.frame} or a \code{tibble}. The event times supplied using this argument will then be put into relation with the time at which each case was included during the matching process, giving the time until the event. Cases with no events can simply be omitted from \code{data}. If multiple events per person exist, they should be included in the long-format (multiple rows per \code{id}). Only the time until the first event after inclusion into the matching data will be used, all previous events and all events afterwards are not used.
  }
  \item{censor_at_treat}{
Either \code{TRUE} or \code{FALSE}, indicating whether the created event time should be censored at the time of the next treatment. This only applies to cases that were included in the matching process as controls but later become cases themselves. Defaults to \code{TRUE}.
  }
  \item{censor_pairs}{
Either \code{TRUE} or \code{FALSE}. Only used if \code{censor_at_treat=TRUE}. If set to \code{TRUE}, the case matched to a control is censored at the same time that the control was censored due to the next treatment occurring. If \code{ratio > 1}, the minimum time to "artificial censoring" is used as censoring time for all cases with the same \code{.id_pair}. This may in some cases be sufficient to deal with the covariate dependent censoring induced by using \code{censor_at_treat=TRUE}.
  }
  \item{units}{
Corresponds to the argument of the same name in the \code{\link{difftime}} function. This argument is only used when the \code{time} column corresponds to a \code{Date} (or similar) variable. It should be used to indicate the time-scale of the created event time (seconds, days, years, ...).
  }
  \item{id}{
A single character string specifying a column in \code{data}, specifying the unique case identifier. By default the same name that was used in the original \code{\link{match_td}} is used here.
  }
  \item{time}{
A single character string specifying a column in \code{data}, specifying the column containing the event times. By default the same name that was used in the original \code{\link{match_td}} is used here.
  }
  \item{event_time_name}{
A single character string specifying the name of the event time column that will be added to the \code{data} object contained in \code{x}. Defaults to \code{.event_time}. If the name is already present, an error message is returned instead.
  }
  \item{status_name}{
A single character string specifying the name of the status column that will be added to the \code{data} object contained in \code{x}. Defaults to \code{.status}. If the name is already present, an error message is returned instead.
  }
  \item{copy_data}{
Either \code{TRUE} (default) or \code{FALSE}, specifying whether to take a shallow copy of the supplied data before running the code. Doing so ensures that all data in the parent environment stays unchanged. If set to \code{FALSE}, they may be modified in place which is more RAM efficient.
  }
}
\details{
Different censoring schemes can be used for different target estimands. In intention-to-treat type analysis, users usually should not be censoring cases at their following treatment times. For per-protocol type analysis, setting \code{censor_at_treat} is usually the right decision. Whether to use \code{censor_pairs} is an additional choice that depends on how the user wants to handle the dependent censoring introduced by using \code{censor_at_treat=TRUE}.
}
\value{
Returns a modified \code{match_td} object. It is essentially the same object as the supplied \code{x}, but it also contains two new columns: \code{event_time_name} (the time until the event or censoring) and \code{status_name} (a logical variable indicating whether the event occurred or the case was censored).
}
\author{
Robin Denz
}
\seealso{
\code{\link{match_td}}
}
\examples{

}
