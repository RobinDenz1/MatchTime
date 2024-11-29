\name{match_td.fit}
\alias{match_td.fit}

\title{
Perform Time-Dependent Matching on Time-Fixed and Time-Dependent Covariates in Discrete and Continuous Time
}
\description{
This function actually performs the matching. It is an internal function used by \code{\link{match_td}} and should probably not be used except in special cases. In contrast to \code{\link{match_td}} it does not perform any input checks and does not have a formula interface. It instead expects the user to input the data in three different \code{data.table}s and to specify the relevant columns using character vectors.
}
\usage{
match_td.fit(id, time, d_treat, d_event, d_covars,
             match_vars=NULL, replace_over_t=FALSE,
             replace_at_t=FALSE, replace_cases=TRUE,
             censor_at_treat=TRUE, censor_pairs=TRUE,
             estimand="ATT", ratio=1, if_no_match="stop",
             match_method="fast_exact",
             keep_all_columns=FALSE, units="auto",
             verbose=FALSE, start="start", stop="stop",
             ...)
}
\arguments{
  \item{id}{
A single character string specifying the unique person identifier column in each dataset.
  }
  \item{time}{
A single character string specifying the variable representing the time in each dataset.
  }
  \item{d_treat}{
A \code{data.table} containing exactly two columns with the names specified by \code{id} and \code{time}. This dataset should contain the first point in time at which a person received the treatment. If a person never received the treatment, it should not be in this dataset.
  }
  \item{d_event}{
A \code{data.table} in the long format, containing exactly two columns with the names specified by \code{id} and \code{time}. This dataset should contain every outcome event per person. Allows multiple events per person.
  }
  \item{d_covars}{
A \code{data.table} in the start-stop format containing covariate information for each \code{id}. The matching at each point in time will be done on these variables. To use only a subset of these variable for matching, the \code{match_vars} argument may be used. By excluding some stretches of time for individuals from this dataset, these individuals are not considered at risk at these times and are thus excluded from the matching process. This functionality may be used to define inclusion criteria.
  }
  \item{match_vars}{
A character vector of variable names that should be matched on.
  }
  \item{replace_over_t}{
Whether to allow usage of the same persons as controls at multiple points in time.
  }
  \item{replace_at_t}{
Whether to allow usage of the same individuals as controls at the same point in time. If \code{match_method} is set to a valid method in \code{\link[MatchIt]{matchit}} this argument will be passed to the \code{replace} argument of the \code{\link[MatchIt]{matchit}} function.
  }
  \item{replace_cases}{
Whether to include persons that have already been used as controls as cases if they also get the treatment later.
  }
  \item{censor_at_treat}{
Whether to censor controls at the time at which they subsequently received the treatment. This should usually be set to \code{TRUE} (default) if a per-protocol type effect should be estimated, although it will then be necessary to adjust the analysis for covariate-dependent artificial censoring (see details). If intention-to-treat type effects are of interest, this argument should usually be set to \code{FALSE}. This argument is only used when \code{!is.na(event)}.
  }
  \item{censor_pairs}{
Whether to censor the case if the matched control gets censored later due to becoming a case themself. This argument is only used when \code{!is.na(event)} and \code{censor_at_treat=TRUE}.
  }
  \item{estimand}{
Which estimand to target. Can be either \code{"ATT"} (default) to get a dataset with which to estimate the average treatment effect on the treated, or to \code{"ATC"} which would target the average treatment effect for the untreated. May also allow other inputs when \code{match_method} is set to a valid method in \code{\link[MatchIt]{matchit}}.
  }
  \item{ratio}{
How many control units should be matched to each treated unit in k:1 matching. Should be a single integer value. The default is 1 for 1:1 matching. If \code{match_method} is set to a valid method in \code{\link[MatchIt]{matchit}}, this argument will be passed to the argument of the same name in the \code{\link[MatchIt]{matchit}} function.
  }
  \item{if_no_match}{
Must be either \code{"stop"}, \code{"warn"} or \code{"nothing"}. Controls whether to throw an error, a warning or silently accept when not enough controls could be matched to one or more cases. Only works when \code{match_method="fast_exact"}.
  }
  \item{match_method}{
A single character string specifying which method should be used to perform matching at each point in time. Allowed values are \code{"none"} (to perform no matching on covariates), \code{"fast_exact"} (default, to use fast exact matching as implemented in the \code{\link{fast_exact_matching}} function of this package) or any valid method of the \code{\link[MatchIt]{matchit}} function. If the latter is used, this argument is passed to the \code{method} argument of the \code{\link[MatchIt]{matchit}} function directly. Further arguments may be passed to \code{\link[MatchIt]{matchit}} in this case using the three-dot syntax.
  }
  \item{keep_all_columns}{
Whether to keep all columns created internally or to only keep the most relevant columns.
  }
  \item{units}{
Corresponds to the \code{units} argument of the \code{\link{difftime}} function. Only used when the time variables (such as \code{start} and \code{stop} in \code{d_covars}) are of class \code{Date} (or similar class) and \code{d_event} is not \code{NULL}. Controls which unit the created \code{event_time} column should have.
  }
  \item{verbose}{
Whether to print a summary of how many matches were made for each point in time or not (default).
  }
  \item{start}{
A single character string specifying a column in \code{d_covars} specifying the beginning of a time-interval. Defaults to \code{"start"}.
  }
  \item{stop}{
A single character string specifying a column in \code{d_covars} specifying the end of a time-interval. Defaults to \code{"stop"}.
  }
  \item{...}{
Further arguments passed to \code{\link[MatchIt]{matchit}} when \code{match_method} is set to a valid method in \code{\link[MatchIt]{matchit}}.
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
