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
                    ratio=1, estimand="ATT",
                    remove_unmatched=TRUE, n_required=ratio,
                    if_no_match="warn")
}
\arguments{
  \item{formula}{
A formula object with the binary treatment variable on the left-hand side and the variables to be matched on on the right hand side. The binary treatment should ideally be coded as a logical variable (\code{TRUE} = treated, \code{FALSE} = untreated). If this is not the case, the function internally coerces the variable to be logical using the rules described in the details.
  }
  \item{data}{
A \code{data.table} like object containing the columns mentioned in \code{formula}. Additional columns are allowed and will be included in the output, although they will not be matched on if not mentioned on the right hand side of \code{formula}.
  }
  \item{replace}{
Whether to sample controls with or without replacement. As is usual in matching, when using \code{ratio > 1} each control can only occur once per case.
  }
  \item{ratio}{
How many control units should be matched to each treated unit in k:1 matching. Should be a single integer value. The default is 1 for 1:1 matching.
  }
  \item{estimand}{
Which estimand to target. Can be either \code{"ATT"} (default) to get a dataset with which to estimate the average treatment effect on the treated, or to \code{"ATC"} which would target the average treatment effect for the untreated. When using \code{"ATC"}, the treatment labels are simply swapped internally before matching and swapped back afterwards.
  }
  \item{remove_unmatched}{
Either \code{TRUE} (default) or \code{FALSE}, specifying whether to remove pairs with no or not enough matches. The number of matches that each case should have received during matching can be set using the \code{n_required} argument.
  }
  \item{n_required}{
A single positive integer, specifying the number of matched controls a pair needs to have to stay in the data. For example, if 1:1 matching was used and the user only wants to keep pairs where cases actually did receive one control, it should be set to 1. If \code{ratio=3} was used and users want to keep all pairs with 2 or more matched controls, it should be set to 2 etc. By default, the number of matches specified in the \code{ratio} argument of the original \code{\link{match_time}} call is used.
  }
  \item{if_no_match}{
Must be either \code{"stop"}, \code{"warn"} (default) or \code{"nothing"}. Controls whether to throw an error, a warning or silently accept when not enough controls could be matched to one or more cases.
  }
}
\details{
This function first extracts all cases from \code{data}, which are defined as all rows in \code{data} where the binary variable (left-hand side of \code{formula}) is \code{TRUE}. A stratified random sample is then drawn from the remaining dataset consisting only of controls, where the strata are the variables that should be matched on. By drawing \code{ratio} controls for each case with exactly the same values in the strata as the respective case, perfectly exact \code{ratio:1} matching is performed. This is a very basic form of matching without a lot of flexibility. It also only supports binary treatments. In almost every case it would be much better to just use the \code{\link[MatchIt]{matchit}} function from the excellent \code{MatchIt} package instead. The only reason to use the \code{fast_exact_matching} function is the better performance on large datasets.

Because controls are matched directly to cases by default, the resulting dataset may generally only be used to get estimates for the average treatment effect on the treated (ATT). By setting \code{estimand="ATC"}, the matching process is reversed. Instead of matching controls to cases, cases are then matched to controls resulting in a dataset that may be used to estimate the average treatment effect on the untreated (ATC). Other estimands are currently not supported.

The \code{.weights} column in the output is calculated using the same method that is used in \code{\link[MatchIt]{matchit}}.

\strong{\emph{Identifying the "treated" group}}:

Ideally, the treatment specified on the LHS of the \code{formula} argument is coded as a logical variable, where \code{TRUE} corresponds to the "treated" group and \code{FALSE} corresponds to the "control" group. If this is not the case, this function will coerce it to this type internally using the following rules:

1.) if the variable only consists of the numbers \code{0} and \code{1} (coded as numeric), \code{0} will be considered the "control" group and \code{1} the "treated" group; 2.) otherwise, if the variable is a factor, \code{levels(treat)[1]} will be considered the "control" group and the other value the "treated" group; 3.) otherwise \code{sort(unique(treat))[1]} will be considered "control" and the other value the treated. It is safest to ensure that the treatment variable is a logical variable. In either case, the output will only contain the treatment as logical variable.

\strong{\emph{Using \code{replace=TRUE}}}:

If \code{replace=TRUE} is used, the output may contain multiple rows for some supplied rows. Namely, it will include one row for every time the control was choosen as a control. For example, if id = 3 was used as a control for id = 5, 6, 7, 8, it will occur in the dataset 4 times, each time with a different \code{.id_pair}. If the user wants to differentiate between these cases, all that needs to be done is to add a unique id column (of any name) to the data before the matching process. As with any other variable in \code{data}, this variable will be preserved after matching.

}
\value{
Returns a single \code{data.table} containing the same columns as \code{data} plus two additional columns called \code{".id_pair"}, which identifies which rows belong to one matched control / case pair and \code{.weights} (a numeric column including the matching weights).
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
library(MatchTime)
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
