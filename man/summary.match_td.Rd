\name{summary.match_td}
\alias{summary.match_td}

\title{
View a balance summary of a \code{match_td} object
}
\description{
Computes and prints balance statistics for \code{match_td} objects at baseline, similar to the \code{summary} method of \code{\link[MatchIt]{matchit}} objects. Similar functionality is implemented in \code{\link{bal.tab.match_td}}.
}
\usage{
\method{summary}{match_td}(object, standardize=TRUE, ...)
}
\arguments{
  \item{object}{
A \code{match_td} object created using the \code{\link{match_td}} function.
  }
  \item{standardize}{
Either \code{TRUE} or \code{FALSE}; whether to compute standardized (\code{TRUE}) or unstandardized (\code{FALSE}) statistics. The standardized statistics are the standardized mean difference and the mean and maximum of the difference in the (weighted) empirical cumulative distribution functions (ECDFs). The unstandardized statistics are the raw mean difference and the mean and maximum of the quantile-quantile (QQ) difference. Variance ratios are produced either way. See Details below. Default is \code{TRUE}.
  }
  \item{...}{
Currently not used.
  }
}
\author{
Robin Denz
}
\details{
The summary method for \code{match_td} objects is made to resemble the summary method of \code{matchit} objects. In fact it directly uses code from the \code{MatchIt} package to compute the balance statistics. For more details on how the balance statistics are calculated and what they mean, please consult the associated documentation page of \code{\link[MatchIt]{summary.matchit}}.

Note that contrary to standard matching, the matching performed by \code{\link{match_td}} is time-dependent. Because of this, presenting balance statistics for unmatched data is impossible. This function therefore only prints the balance statistics at baseline (e.g. at the time on which the cases were included in the matching process).
}
\value{
Silently returns a \code{list} containing the balance statistics and sample sizes.
}
\seealso{
\code{\link{match_td}}, \code{\link[MatchIt]{summary.matchit}}, \code{\link[MatchIt]{matchit}} \code{\link[cobalt]{bal.tab}}, \code{\link[cobalt]{love.plot}}
}
\examples{

}
