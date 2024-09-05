\name{fast_exact_matching}
\alias{fast_exact_matching}

\title{
Perform Fast Exact Matching on Catgeorical Variables
}
\description{

}
\usage{
fast_exact_matching(data, treat, strata, replace=FALSE,
                    ratio=1, if_lt_n="stop", estimand="ATT")
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
  \item{if_lt_n_at_t}{
Must be either \code{"stop"}, \code{"warn"} or \code{"nothing"}. Controls whether to throw an error, a warning or silently accept when not enough controls could be matched to one or more cases.
  }
  \item{estimand}{
Which estimand to target. Can be either \code{"ATT"} (default) to get a dataset with which to estimate the average treatment effect on the treated, or to \code{"ATC"} which would target the average treatment effect for the untreated.
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
\code{\link{sample_dt_stratified}}
}
\examples{

}
