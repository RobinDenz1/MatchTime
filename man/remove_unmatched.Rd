\name{remove_unmatched}
\alias{remove_unmatched}

\title{
Remove cases with no (or not enough) matched controls from a \code{match_td} object
}
\description{
After performing time-dependent matching using the \code{\link{match_td}} function, users usually want to extract the resulting data. If the resulting data should not contain cases that did not receive the specified number of matches, this function can be called to remove them before data extraction.
}
\usage{
remove_unmatched(x, n_required=x$info$ratio)
}
\arguments{
  \item{x}{
A \code{match_td} object created using the \code{\link{match_td}} function.
  }
  \item{n_required}{
A single integer, specifying the number of matched controls a case has to have to stay in the data. For example, if 1:1 matching was used and the user only wants to keep cases that actually did receive one control, it should be set to 1. If \code{ratio=3} was used and users want to keep all cases with 2 or more matched controls, it should be set to 2 etc. By default, the number of matches specified in the \code{ratio} argument of the original \code{\link{match_td}} call is used.
  }
}
\value{
Returns a modified \code{match_td} object. It is essentially the same object as the supplied \code{x}, but with less rows in \code{data}.
}
\author{
Robin Denz
}
\seealso{
\code{\link{match_td}}
}
\examples{
library(data.table)
library(MatchTD)

# only execute if packages are available
if (requireNamespace("survival")) {

library(survival)

# set random seed to make the output replicably
set.seed(1234)

# load "heart" data from survival package
data("heart")

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
m_obj <- match_td(transplant ~ age + surgery, data=heart, id="id",
                  match_method="nearest")

# remove unmatched individuals
m_obj <- remove_unmatched(m_obj)
head(m_obj$data)
}
}
