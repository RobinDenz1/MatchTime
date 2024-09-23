\name{fast_exact_matching}
\alias{fast_exact_matching}

\title{
Perform Fast Exact Matching on Catgeorical Variables
}
\description{
This function matches one or multiple controls to cases that have exactly the same values in one or multiple categorical variables. Technically works with continuous variables as well, but this will probably not work well in practice due to a very low probability of exact matches.
}
\usage{
fast_exact_matching(data, treat, strata, replace=FALSE,
                    ratio=1, estimand="ATT", if_lt_n="stop")
}
\arguments{
  \item{data}{
A \code{data.table} containing two columns representing the treatment status (\code{treat}) and the strata \code{strata} that should be matched on. If multiple categorical variables should be matched on, the user should paste them together to get a single \code{strata} variable identifying the unique strata.
  }
  \item{treat}{
A single character string specifying the logical variable representing the treatment in \code{data}.
  }
  \item{strata}{
A single character string specifying the integer, character or factor variable representing the strata that should be matched on in \code{data}.
  }
  \item{replace}{
Whether to sample controls with or without replacement.
  }
  \item{ratio}{
How many control units should be matched to each treated unit in k:1 matching. Should be a single integer value. The default is 1 for 1:1 matching.
  }
  \item{estimand}{
Which estimand to target. Can be either \code{"ATT"} (default) to get a dataset with which to estimate the average treatment effect on the treated, or to \code{"ATC"} which would target the average treatment effect for the untreated.
  }
  \item{if_lt_n_at_t}{
Must be either \code{"stop"}, \code{"warn"} or \code{"nothing"}. Controls whether to throw an error, a warning or silently accept when not enough controls could be matched to one or more cases.
  }
}
\details{
This function first extracts all cases from \code{data}, which are defined as all rows in \code{data} where the binary variable denoted by \code{treat} is \code{TRUE}. A stratified random sample is then drawn from the remaining dataset consisting only of controls, where the strata are the variables that should be matched on. By drawing \code{ratio} controls for each case with exactly the same values in \code{strata} as the respective case, perfectly exact \code{ratio:1} matching is performed. This is a very basic form of matching without a lot of flexibility. In almost every case it would be better to just use the \code{matchit()} function from the excellent \code{MatchIt} package instead. The only reason to use the \code{fast_exact_matching} function is the better performance on large datasets.

Because controls are matched directly to cases by default, the resulting dataset may generally only be used to get estimates for the average treatment effect on the treated (ATT). By setting \code{estimand="ATC"}, the matching process is reversed. Instead of matching controls to cases, cases are then matched to controls resulting in a dataset that may be used to estimate the average treatment effect on the untreated (ATC). Other estimands are currently not supported.
}
\value{
Returns a single \code{data.table} containing the same columns as \code{data} plus one additional column called \code{"pair_id"}, which identifies which rows belong to one matched control / case pair.
}
\references{

}
\author{
Robin Denz
}

\seealso{
\code{\link{sample_dt_stratified}}
}
\examples{

}
