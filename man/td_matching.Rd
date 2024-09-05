\name{td_matching}
\alias{td_matching}

\title{
Perform Time-Dependent Matching with a Time-To-Event Outcome
}
\description{

}
\usage{
td_matching(id, time, d_treat, d_event, d_baseline,
            replace_over_t=FALSE, replace_at_t=FALSE,
            replace_cases=TRUE, censor_pairs=TRUE,
            estimand="ATT", ratio=1, if_lt_n_at_t="stop",
            use_matchit=FALSE, matchit_method="nearest",
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
  \item{d_baseline}{
A \code{data.table} containing baseline information for each \code{id}. The matching at each point in time will be done on these variables.
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
  \item{censor_pairs}{
Whether to censor the case if the matched control gets censored later due to becoming a case themself.
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
  \item{use_matchit}{
Whether to use the \code{\link[MatchIt]{matchit}} function from the \pkg{MatchIt} package to perform the matching at each point in time. If set to \code{FALSE} (default), a fast version of exact matching is used instead.
  }
  \item{matchit_method}{
Passed to the \code{method} argument of the \code{\link[MatchIt]{matchit}} function when using \code{use_matchit=TRUE}.
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
\code{\link{fast_exact_matching}}, \code{\link{sample_dt_stratified}}, \code{\link[MatchIt]{matchit}}
}
\examples{

}
