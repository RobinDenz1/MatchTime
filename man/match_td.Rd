\name{match_td}
\alias{match_td}

\title{
Perform Time-Dependent Matching in Discrete and Continuous Time
}
\description{
This function utilizies the approach described in XX and XX to match untreated controls to treated individuals in a time-dependent fashion. This approach is also known as sequential target trial emulation. In contrast to other implementations, this function supports continuous and datetime input and allows matching directly on time-fixed and time-dependent covariates at the same time. It internally uses the \code{data.table} package to keep the function fast and allows users to use the \code{\link[MatchIt]{matchit}} function from the excellent \code{MatchIt} package to perform the actual matching at each point in time for more flexibility.
}
\usage{
match_td(formula, data, id, inclusion=NA,
         start="start", stop="stop",
         replace_over_t=FALSE, replace_at_t=FALSE,
         replace_cases=TRUE, estimand="ATT",
         ratio=1, match_method="fast_exact",
         if_no_match="warn", verbose=FALSE,
         ...)
}
\arguments{
  \item{formula}{
A \code{formula} object with a binary treatment variable on the left hand side and the covariates to be balanced on the right hand side. Interactions and functions of covariates are currently not allowed. The treatment variable is ideally coded as a logical variable (\code{TRUE} = treatment, \code{FALSE} = control). See details for how the "treated" group is identified with other input types.
  }
  \item{data}{
A \code{data.table} like object in the start-stop format, containing information about variables that are time-invariant or time-dependent. Each row corresponds to a period of time in which no variables changed. These intervals are defined by the \code{start} and \code{stop} columns. The \code{start} column gives the time at which the period started, the \code{stop} column denotes the time when the period ended. Intervals should be coded to be \emph{right-open} (corresponds to \code{[start, stop)}). Continuous (float) and discrete (integer, datetime) values are supported for both time columns. The dataset should also include an \code{id} variable (see argument \code{id}). See details and examples for more information.
  }
  \item{id}{
A single character string specifying the unique case identifier in \code{data}.
  }
  \item{inclusion}{
A single character string specifying an optional logical variable in \code{data} which should be \code{TRUE} if the period specified by the \code{start} and \code{stop} columns in \code{data} corresponds to a period in which the individual fufills the inclusion criteria and \code{FALSE} if the individual does not. All periods where the variable is \code{FALSE} will be excluded from the matching process dynamically. Set to \code{NA} to not use this functionality (default).
  }
  \item{start}{
A single character string specifying a column in \code{data} specifying the beginning of a time-interval. Defaults to \code{"start"}.
  }
  \item{stop}{
A single character string specifying a column in \code{data} specifying the end of a time-interval. Defaults to \code{"stop"}.
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
  \item{match_method}{
A single character string specifying which method should be used to perform matching at each point in time. Allowed values are \code{"none"} (to perform no matching on covariates), \code{"fast_exact"} (default, to use fast exact matching as implemented in the \code{\link{fast_exact_matching}} function of this package) or any valid method of the \code{\link[MatchIt]{matchit}} function. If the latter is used, this argument is passed to the \code{method} argument of the \code{\link[MatchIt]{matchit}} function directly. Further arguments may be passed to \code{\link[MatchIt]{matchit}} in this case using the three-dot syntax.
  }
  \item{if_no_match}{
Must be either \code{"stop"}, \code{"warn"} (default) or \code{"nothing"}. Controls whether to throw an error, a warning or silently accept when not enough controls could be matched to one or more cases. Only works when \code{match_method="fast_exact"}.
  }
  \item{verbose}{
Whether to print a summary of how many matches were made for each point in time or not (default).
  }
  \item{...}{
Further arguments passed to \code{\link[MatchIt]{matchit}} when \code{match_method} is set to a valid method in \code{\link[MatchIt]{matchit}}.
  }
}
\details{

\strong{\emph{Identifying the "treated" group}}:

Ideally, the treatment specified on the LHS of the \code{formula} argument is coded as a logical variable, where \code{TRUE} corresponds to the "treated" group and \code{FALSE} corresponds to the "control" group. If this is not the case, this function will coerce it to this type internally using the following rules:

1.) if the variable only consists of the numbers \code{0} and \code{1} (coded as numeric), \code{0} will be considered the "control" group and \code{1} the "treated" group; 2.) otherwise, if the variable is a factor, \code{levels(treat)[1]} will be considered the "control" group and the other value the "treated" group; 3.) otherwise \code{sort(unique(treat))[1]} will be considered "control" and the other value the treated. It is safest to ensure that the treatment variable is a logical variable. In either case, the output will only contain the treatment as logical variable.

\strong{\emph{Interval Coding}}:

The intervals supplied to the \code{data} argument are required to be \emph{right-open} intervals \code{[start, stop)}, which is the usual data format expected for time-to-event modelling and corresponds to the interval format of the \code{tmerge} function of the \pkg{survival} package. As a consequence, intervals of length 0 (where \code{start==stop}) are not supported and will result in an error message. Although events should be coded differently, this does not matter for this function itself because \code{data} should not contain events.

\strong{\emph{Adding more Variables}}

Users usually want to add outcomes and / or further baseline covariates to the data after matching. This can be done using the \code{add_outcome} and \code{add_covariate} functions and is described in detail in the respective documentation and vignette.

\strong{\emph{Assessing Covariate Balance}}

The balance of the covariates at baseline can be assessed using the associated \code{\link{summary.match_td}} or \code{\link{bal.tab.match_td}} functions. Note that with time-dependent matching it is only possible to assess balance at baseline, because the treatment is also time-dependent.

}
\value{
Returns a \code{match_td} object containing the following objects:

\item{data}{A \code{data.table} containing the matched data. Note that this dataset also contains unmatched cases. To obtain a dataset without unmatched individuals, please use the \code{\link{match_data}} function. The dataset here contains at least the following columns: \code{id} (the original \code{id} used in the supplied \code{data}), \code{.id_new} (a new case-specific id in which \code{id}s who occur multiple times are treated as distinct values), \code{.id_pair} (an id to distinguish the matched pairs), \code{.treat} (the supplied treatment variable), \code{.treat_time} (the time at which the \code{id} was included in the matching process), \code{.next_treat_time} (for controls that later receive treatment, the time at which they received the treatment) and \code{.fully_matched} (a logical variable that is \code{TRUE} if the corresponding \code{.id_pair} consists of one case and \code{ratio} matched controls and \code{FALSE} otherwise). Potentially contains any number of additional covariates supplied in the original \code{data}, plus potential further variables added using \code{\link{add_outcome}}.}
\item{d_longest}{A \code{data.table} containing the last time under observation for each \code{id} in the supplied \code{data}.}
\item{trace}{A \code{data.table} containing four columns: \code{time} (the time at which matching occurred), \code{new_cases} (the number of new cases at that point in time), \code{matched_controls} (the number of controls matched to the new cases at \code{time}) and \code{potential_controls} (the number of potential controls at \code{time}).}
\item{id}{The value of the supplied \code{id} argument.}
\item{time}{A character string used internally to identify the time in other datasets.}
\item{info}{A \code{list} containing various information on the matching process.}
\item{sizes}{A \code{list} containing various information on the overall sample sizes at each stage.}
\item{call}{The original function call.}

}
\references{

}
\note{
Column names starting with a single point (e.g. names like \code{".variable"} or \code{".id"}) cannot be used in \code{data}, because they are used internally, which could lead to weird errors.
}
\author{
Robin Denz
}
\seealso{
\code{\link{fast_exact_matching}}, \code{\link{stratified_sample}}, \code{\link[MatchIt]{matchit}}, \code{\link{summary.match_td}}, \code{\link{bal.tab.match_td}}
}
\examples{

}
