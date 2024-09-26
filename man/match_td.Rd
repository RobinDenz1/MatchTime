\name{match_td}
\alias{match_td}

\title{
Perform Time-Dependent Matching on Time-Fixed and Time-Dependent Covariates in Discrete and Continuous Time
}
\description{
This function utilizies the approach described in XX and XX to match untreated controls to treated individuals in a time-dependent fashion. This approach is also known as sequential target trial emulation. In contrast to other implementations, this function supports continuous and datetime input and allows matching directly on time-fixed and time-dependent covariates at the same time. It internally uses the \code{data.table} package to keep the function fast and allows users to use the excellent \code{MatchIt} package to perform the actual matching at each point in time for more flexibility.
}
\usage{
match_td(formula, data, id, inclusion=NA, event=NA,
         replace_over_t=FALSE, replace_at_t=replace_over_t,
         replace_cases=TRUE, estimand="ATT",
         ratio=1, if_lt_n_at_t="stop", censor_pairs=TRUE,
         use_matchit=FALSE, matchit_method="nearest",
         keep_all_columns=FALSE, verbose=FALSE, ...)
}
\arguments{
  \item{formula}{
A \code{formula} object with a binary treatment variable on the left hand side and the covariates to be balanced on the right hand side. Interactions and functions of covariates are currently not allowed.
  }
  \item{data}{
A \code{data.table} like object in the start-stop format, containing information about both variables that are time-invariant or time-dependent. Each row corresponds to a period of time in which no variables changed. These intervals are defined by the \code{start} and \code{stop} columns. The \code{start} column gives the time at which the period started, the \code{stop} column denotes the time when the period ended. Continuous (float) and discrete (integer, datetime) values are supported for both time columns. The dataset should also include an \code{id} variable (see argument \code{id}). See details and examples for more information.
  }
  \item{id}{
A single character string specifying the unique person identifier in \code{data}.
  }
  \item{inclusion}{
A single character string specifying an optional logical variable which should be \code{TRUE} if the period specified by the \code{start} and \code{stop} columns in \code{data} corresponds to a period in which the individual fufills the inclusion criteria and \code{FALSE} if the individual does not. All periods where the variable is \code{FALSE} will be excluded from the matching process dynamically. Set to \code{NA} to not use this functionality (default).
  }
  \item{event}{
A single character string specifying an optional logical variable which specifies whether an event happened in the period specified by the \code{start} and \code{stop} columns in \code{data}. If specified, two columns will be added to the final matched dataset: \code{event_time} and \code{status}, corresponding to the time from entry into the study until the event or censoring happened and the respective status indicator, as used in standard survival analysis.
  }
  \item{replace_over_t}{
Whether to allow usage of the same persons as controls at multiple points in time.
  }
  \item{replace_at_t}{
Whether to allow usage of the same persons as controls at the same point in time.
  }
  \item{replace_cases}{
Whether to include persons that have already been used as controls as cases if they also get the treatment later.
  }
  \item{estimand}{
Which estimand to target. Can be either \code{"ATT"} (default) to get a dataset with which to estimate the average treatment effect on the treated, or to \code{"ATC"} which would target the average treatment effect for the untreated. May also allow other inputs when using \code{use_matchit=TRUE}.
  }
  \item{ratio}{
How many control units should be matched to each treated unit in k:1 matching. Should be a single integer value. The default is 1 for 1:1 matching.
  }
  \item{if_lt_n_at_t}{
Must be either \code{"stop"}, \code{"warn"} or \code{"nothing"}. Controls whether to throw an error, a warning or silently accept when not enough controls could be matched to one or more cases. Only works when \code{use_matchit=FALSE}.
  }
  \item{censor_pairs}{
Whether to censor the case if one of the matched controls gets censored later due to becoming a case themself. This argument is only used when \code{!is.na(event)}.
  }
  \item{use_matchit}{
Whether to use the \code{\link[MatchIt]{matchit}} function from the \pkg{MatchIt} package to perform the matching at each point in time. If set to \code{FALSE} (default), a fast version of exact matching is used instead.
  }
  \item{matchit_method}{
Passed to the \code{method} argument of the \code{\link[MatchIt]{matchit}} function when using \code{use_matchit=TRUE}.
  }
  \item{keep_all_columns}{
Whether to keep all columns created internally or to only keep the most relevant columns.
  }
  \item{verbose}{
Whether to print a summary of how many matches were made for each point in time or not (default).
  }
  \item{...}{
Further arguments passed to \code{\link[MatchIt]{matchit}} when using \code{use_matchit=TRUE}.
  }
}
\details{

}
\value{

}
\references{

}
\author{
Robin Denz
}

\seealso{
\code{\link{fast_exact_matching}}, \code{\link{stratified_sample}}, \code{\link[MatchIt]{matchit}}
}
\examples{

}
