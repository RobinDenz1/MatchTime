\name{summary.match_td}
\alias{summary.match_td}

\title{
View a balance summary of a \code{match_td} object
}
\description{
Computes and prints balance statistics for \code{match_td} objects at baseline, similar to the \code{summary} method of \code{\link[MatchIt]{matchit}} objects. Similar functionality is implemented in \code{\link{bal.tab.match_td}}.
}
\usage{
\method{summary}{match_td}(object, standardize=TRUE,
        remove_unmatched=TRUE,
        n_required=object$info$ratio, ...)
}
\arguments{
  \item{object}{
A \code{match_td} object created using the \code{\link{match_td}} function.
  }
  \item{standardize}{
Either \code{TRUE} or \code{FALSE}; whether to compute standardized (\code{TRUE}) or unstandardized (\code{FALSE}) statistics. The standardized statistics are the standardized mean difference and the mean and maximum of the difference in the (weighted) empirical cumulative distribution functions (ECDFs). The unstandardized statistics are the raw mean difference and the mean and maximum of the quantile-quantile (QQ) difference. Variance ratios are produced either way. See Details below. Default is \code{TRUE}.
  }
  \item{remove_unmatched}{
Whether to remove unmatched individuals before calculating the balance statistics. Internally, the \code{\link{match_data}} function is called for this, please see the documentation of that function for more information. This has no influence on the computed sample sizes.
  }
  \item{n_required}{
Same as the argument of the same name in \code{\link{match_data}}.
  }
  \item{...}{
Currently not used.
  }
}
\author{
Robin Denz
}
\details{

\strong{\emph{Balance Statistics}}:

The summary method for \code{match_td} objects is made to resemble the summary method of \code{matchit} objects. In fact it directly uses code from the \code{MatchIt} package to compute the balance statistics. For more details on how the balance statistics are calculated and what they mean, please consult the associated documentation page of \code{\link[MatchIt]{summary.matchit}}.

Note that contrary to standard matching, the matching performed by \code{\link{match_td}} is time-dependent. Because of this, presenting balance statistics for unmatched data is impossible. This function therefore only prints the balance statistics at baseline (e.g. at the time on which the cases were included in the matching process).

\strong{\emph{Sample Sizes}}:

The displayed sample sizes may be confusing without further explanation. Because the matching process is based on dynamically changing risk sets, where each individual may be either "control" or "treated" at different points in time, it is not easy to summarise the sample sizes in terms of the binary distinction "Controls" and "Treated".

Displayed under "Matched" are the numbers of individuals who form fully matched pairs in the output (e.g. each case received \code{ratio} matched controls). The "Unmatched Controls" below refer to potential controls that were never used as such in the matching process, while "Unmatched Treated" refers to cases that did not receive \code{ratio} matched controls. Note that the individuals mentioned as "Unmatched Treated" may still have been used as controls during the matching process. The "All" column is simply the sum of both "Controls" and "Treated" for "Matched" and "Unmatched" individuals.

"Included Controls" refers to all individuals which fufilled the inclusion criteria at some point in time before becoming "treated", while the "Included Treated" refers to all individuals who fufilled the inclusion criteria at some point in time during which they received the treatment. The "Supplied Controls" similarly refers to the number of individuals who, before applying the inclusion criteria, were present as "untreated" at some point in time in the \code{data} supplied to \code{\link{match_td}}. The "Supplied Treated" are the number of individuals who, before applying the inclusion criteria, were present as "treated" at some point in time in \code{data}. The "All" column is usually not the sum of "Controls" and "Treated" here, because unless the treatment occured exactly at t = 0, the cases may be used as controls as well.

These are total numbers, calculated irrespective of the replacement options used. For more detailed information on how the number of cases, number of matched controls and number of potential controls evolved over time during the matching process, the \code{\link{plot.match_td}} function may be used. Alternatively, users may directly inspect the \code{trace} object included in the output object of \code{\link{match_td}}.
}
\value{
Silently returns a \code{list} containing the balance statistics and sample sizes.
}
\seealso{
\code{\link{match_td}}, \code{\link[MatchIt]{summary.matchit}}, \code{\link[MatchIt]{matchit}} \code{\link[cobalt]{bal.tab}}, \code{\link[cobalt]{love.plot}}
}
\examples{
library(data.table)
library(MatchTD)

# only execute if packages are available
if (requireNamespace("MatchIt") & requireNamespace("survival")) {

library(MatchIt)
library(survival)

# set random seed to make the output replicably
set.seed(1234)

# load "heart" data from survival package
data("heart")

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
out <- match_td(transplant ~ age + surgery, data=heart, id="id",
                match_method="nearest")
summary(out)
}
}
