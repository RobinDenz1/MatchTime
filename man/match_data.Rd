\name{match_data}
\alias{match_data}

\title{
Construct a matched dataset from a \code{match_time} object
}
\description{
After performing time-dependent matching using the \code{\link{match_time}} function, users usually want to extract the resulting data. If the resulting data should not contain cases that did not receive the specified number of matches, this function can be called to remove them before data extraction.
}
\usage{
match_data(object, remove_unmatched=TRUE, n_required=object$info$ratio)
}
\arguments{
  \item{object}{
A \code{match_time} object created using the \code{\link{match_time}} function.
  }
  \item{remove_unmatched}{
Either \code{TRUE} (default) or \code{FALSE}, specifying whether to remove pairs with no or not enough matches. The number of matches that each case should have received during matching can be set using the \code{n_required} argument.
  }
  \item{n_required}{
A single positive integer, specifying the number of matched controls a pair needs to have to stay in the data. For example, if 1:1 matching was used and the user only wants to keep pairs where cases actually did receive one control, it should be set to 1. If \code{ratio=3} was used and users want to keep all pairs with 2 or more matched controls, it should be set to 2 etc. By default, the number of matches specified in the \code{ratio} argument of the original \code{\link{match_time}} call is used.
  }
}
\value{
Returns a single \code{data.table}.
}
\details{
The resulting data may contain multiple rows per actual individual, depending on the arguments used in the original \code{\link{match_time}} call. This might be because individuals were included as controls \emph{and} cases at different points in time, or because they were included as controls at multiple points in time (\code{replace_over_t=TRUE}), or because they were included as controls multiple times at the same point in time (\code{replace_at_t=TRUE}), or due to a mix of these reasons.

Contrary to the \pkg{MatchIt} package, there is no option to change this. "Replaced" individuals will always appear as often as they were selected during the matching process. The output will therefore also always contain an \code{".id_pair"} column, indicating in which "pair" the cases and controls are. The only instance where this column is not included is when \code{match_method} is set to some method in \code{\link[MatchIt]{matchit}} that does not create pairs internally.
}
\author{
Robin Denz
}
\seealso{
\code{\link{match_time}}
}
\examples{
library(data.table)
library(MatchTime)

# only execute if packages are available
if (requireNamespace("survival") & requireNamespace("MatchIt")) {

library(survival)
library(MatchIt)

# set random seed to make the output replicably
set.seed(1234)

# load "heart" data from survival package
data("heart")

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
m_obj <- match_time(transplant ~ age + surgery, data=heart, id="id",
                    match_method="nearest")

# get data without unmatched individuals
data <- match_data(m_obj)
head(data)
}
}
