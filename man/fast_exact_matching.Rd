\name{fast_exact_matching}
\alias{fast_exact_matching}

\title{
Fast Exact Matching with a Binary Treatment
}
\description{
This function matches one or multiple controls to cases that have exactly the same values in one or multiple categorical variables. Technically works with continuous variables as well, but this will probably not work well in practice due to a very low probability of exact matches.
}
\usage{
fast_exact_matching(formula, data, replace=FALSE,
                    ratio=1, estimand="ATT", if_no_match="warn",
                    check_inputs=TRUE, copy_data=TRUE)
}
\arguments{
  \item{formula}{
A formula object with the binary treatment variable on the left-hand side and the variables to be matched on on the right hand side.
  }
  \item{data}{
A \code{data.table} like object containing the columns mentioned in \code{formula}. Additional columns are allowed and will be included in the output, although they will not be matched on if not mentioned on the right hand side of \code{formula}.
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
  \item{if_no_match}{
Must be either \code{"stop"}, \code{"warn"} (default) or \code{"nothing"}. Controls whether to throw an error, a warning or silently accept when not enough controls could be matched to one or more cases.
  }
  \item{check_inputs}{
Whether to perform input checks or not. Can be set to \code{FALSE} for a very small speed-up if the user is sure that the input is correct.
  }
  \item{copy_data}{
Whether to take a shallow copy of the \code{data} first. Taking the copy ensures that \code{data} is not changed by operations performed in this function. If you do not care about preserving the original \code{data} and want to save some RAM, this can be set to \code{FALSE}, in which case all operations are performed in place.
  }
}
\details{
This function first extracts all cases from \code{data}, which are defined as all rows in \code{data} where the binary variable (left-hand side of \code{formula}) is \code{TRUE}. A stratified random sample is then drawn from the remaining dataset consisting only of controls, where the strata are the variables that should be matched on. By drawing \code{ratio} controls for each case with exactly the same values in the strata as the respective case, perfectly exact \code{ratio:1} matching is performed. This is a very basic form of matching without a lot of flexibility. It also only supports binary treatments. In almost every case it would be much better to just use the \code{\link[MatchIt]{matchit}} function from the excellent \code{MatchIt} package instead. The only reason to use the \code{fast_exact_matching} function is the better performance on large datasets.

Because controls are matched directly to cases by default, the resulting dataset may generally only be used to get estimates for the average treatment effect on the treated (ATT). By setting \code{estimand="ATC"}, the matching process is reversed. Instead of matching controls to cases, cases are then matched to controls resulting in a dataset that may be used to estimate the average treatment effect on the untreated (ATC). Other estimands are currently not supported.
}
\value{
Returns a single \code{data.table} containing the same columns as \code{data} plus one additional column called \code{".id_pair"}, which identifies which rows belong to one matched control / case pair.
}
\references{
Elizabeth A. Stuart (2010). "Matching Methods for Causal Inference: A Review and a Look Forward". In: Statistical Science 25.1, pp. 1-21.
}
\author{
Robin Denz
}
\seealso{
\code{\link{stratified_sample}}, \code{\link[MatchIt]{matchit}}
}
\examples{
library(MatchTD)
library(data.table)

set.seed(12341)

## generate some random example data
n <- 1000
data <- data.table(sex=sample(c("m", "f"), size=n, replace=TRUE),
                   age_cat=sample(c("10", "20", "30"), size=n, replace=TRUE),
                   treatment=sample(c(TRUE, FALSE), size=n, prob=c(0.1, 0.9),
                                    replace=TRUE))

# perform 1:1 exact matching on sex, without replacement
out <- fast_exact_matching(treatment ~ sex,
                           data=data,
                           ratio=1)

# perform 3:1 exact matching on sex, without replacement
out <- fast_exact_matching(treatment ~ sex,
                           data=data,
                           ratio=3,
                           replace=TRUE)

# perform 1:1 exact matching on sex, with replacement using the ATC
out <- fast_exact_matching(treatment ~ sex,
                           data=data,
                           ratio=1,
                           estimand="ATC",
                           replace=TRUE)

# perform 1:1 exact matching on sex and age_cat, without replacement
out <- fast_exact_matching(treatment ~ sex + age_cat,
                           data=data,
                           ratio=1)
}
