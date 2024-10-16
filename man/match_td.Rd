\name{match_td}
\alias{match_td}

\title{
Perform Time-Dependent Matching on Time-Fixed and Time-Dependent Covariates in Discrete and Continuous Time
}
\description{
This function utilizies the approach described in XX and XX to match untreated controls to treated individuals in a time-dependent fashion. This approach is also known as sequential target trial emulation. In contrast to other implementations, this function supports continuous and datetime input and allows matching directly on time-fixed and time-dependent covariates at the same time. It internally uses the \code{data.table} package to keep the function fast and allows users to use the \code{\link[MatchIt]{matchit}} function from the excellent \code{MatchIt} package to perform the actual matching at each point in time for more flexibility.
}
\usage{
match_td(formula, data, id, inclusion=NA, event=NA,
         replace_over_t=FALSE, replace_at_t=replace_over_t,
         replace_cases=TRUE, estimand="ATT",
         ratio=1, if_lt_n_at_t="stop", censor_at_treat=TRUE,
         censor_pairs=TRUE, match_method="fast_exact",
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
A single character string specifying an optional logical variable in \code{data} which should be \code{TRUE} if the period specified by the \code{start} and \code{stop} columns in \code{data} corresponds to a period in which the individual fufills the inclusion criteria and \code{FALSE} if the individual does not. All periods where the variable is \code{FALSE} will be excluded from the matching process dynamically. Set to \code{NA} to not use this functionality (default).
  }
  \item{event}{
A single character string specifying an optional logical variable in \code{data} which specifies whether an event happened in the period specified by the \code{start} and \code{stop} columns in \code{data}. If specified, two columns will be added to the final matched dataset: \code{event_time} and \code{status}, corresponding to the time from entry into the study until the event or censoring happened and the respective status indicator, as used in standard survival analysis.
  }
  \item{replace_over_t}{
Whether to allow usage of the same individuals as controls at multiple points in time. If \code{TRUE}, the same person may be used as control at every point in time until it switches from being a control to being a case.
  }
  \item{replace_at_t}{
Whether to allow usage of the same individuals as controls at the same point in time. If \code{match_method} is set to a valid method in \code{\link[MatchIt]{matchit}} this argument will be passed to the \code{replace} argument of the \code{\link[MatchIt]{matchit}} function.
  }
  \item{replace_cases}{
Whether to include individuals that have already been used as controls as cases if they also get the treatment later. This should usually stay at its default value of \code{TRUE}, unless there are some good reasons to change it.
  }
  \item{estimand}{
Which estimand to target. Can be either \code{"ATT"} (default) to get a dataset with which to estimate the average treatment effect on the treated, or to \code{"ATC"} which would target the average treatment effect for the untreated. May also allow other inputs when \code{match_method} is set to a valid method in \code{\link[MatchIt]{matchit}}.
  }
  \item{ratio}{
How many control units should be matched to each treated unit in k:1 matching. Should be a single integer value. The default is 1 for 1:1 matching. If \code{match_method} is set to a valid method in \code{\link[MatchIt]{matchit}}, this argument will be passed to the argument of the same name in the \code{\link[MatchIt]{matchit}} function.
  }
  \item{if_lt_n_at_t}{
Must be either \code{"stop"}, \code{"warn"} or \code{"nothing"}. Controls whether to throw an error, a warning or silently accept when not enough controls could be matched to one or more cases. Only works when \code{match_method="fast_exact"}.
  }
  \item{censor_at_treat}{
Whether to censor controls at the time at which they subsequently received the treatment. This should usually be set to \code{TRUE} (default) if a per-protocol type effect should be estimated, although it will then be necessary to adjust the analysis for covariate-dependent artificial censoring (see details). If intention-to-treat type effects are of interest, this argument should usually be set to \code{FALSE}. This argument is only used when \code{!is.na(event)}.
  }
  \item{censor_pairs}{
Whether to censor the case if one of the matched controls gets censored later due to becoming a case themself. This argument is only used when \code{!is.na(event)} and when \code{censor_at_treat=TRUE}.
  }
  \item{match_method}{
A single character string specifying which method should be used to perform matching at each point in time. Allowed values are \code{"none"} (to perform no matching on covariates), \code{"fast_exact"} (default, to use fast exact matching as implemented in the \code{\link{fast_exact_matching}} function of this package) or any valid method of the \code{\link[MatchIt]{matchit}} function. If the latter is used, this argument is passed to the \code{method} argument of the \code{\link[MatchIt]{matchit}} function directly. Further arguments may be passed to \code{\link[MatchIt]{matchit}} in this case using the three-dot syntax.
  }
  \item{keep_all_columns}{
Whether to keep all columns created internally or to only keep the most relevant columns.
  }
  \item{verbose}{
Whether to print a summary of how many matches were made for each point in time or not (default).
  }
  \item{...}{
Further arguments passed to \code{\link[MatchIt]{matchit}} when \code{match_method} is set to a valid method in \code{\link[MatchIt]{matchit}}.
  }
}
\details{

}
\value{
Returns a single \code{data.table} containing the following columns:

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
