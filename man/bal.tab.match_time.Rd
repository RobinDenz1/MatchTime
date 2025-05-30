\name{bal.tab.match_time}
\alias{bal.tab.match_time}

\title{
Display Balance Statistics in a Table for \code{match_time} objects
}
\description{
This function extends the \code{\link[cobalt]{bal.tab}} function from the \code{cobalt} package to allow the input of a \code{match_time} object. This allows users to directly call the \code{\link[cobalt]{bal.tab}} function on output of the \code{\link{match_time}} function, mimicking the usage of the same function when using the \code{\link[MatchIt]{matchit}} function of the \code{MatchIt} package. The existence of this function also allows users to directly call the \code{\link[cobalt]{love.plot}} function on \code{match_time} objects.
}
\usage{
\method{bal.tab}{match_time}(x, s.d.denom, remove_unmatched=TRUE,
        n_required=x$info$ratio, ...)
}
\arguments{
  \item{x}{
A \code{match_time} object created using the \code{\link{match_time}} function.
  }
  \item{s.d.denom}{
Argument of the same name in \code{\link[cobalt]{bal.tab}}.
  }
  \item{remove_unmatched}{
Whether to remove unmatched individuals before calculating the balance statistics. Internally, the \code{\link{get_match_data}} function is called for this, please see the documentation of that function for more information. This has no influence on the computed sample sizes.
  }
  \item{n_required}{
Same as the argument of the same name in \code{\link{get_match_data}}.
  }
  \item{...}{
Further arguments passed to \code{\link[cobalt]{bal.tab}}.
  }
}
\author{
Robin Denz
}
\details{
All balance statistics calculated using this method are always considered "unadjusted". The reason is that it is not possible to make a distinction between "unadjusted" and "adjusted" balance statistics directly when using time-dependent matching, because balance is assessed at "baseline" (e.g. the time at which individuals were first included into the matching process). This is easy for the matched data. Here we just use the covariate value at time of entry (\code{.treat_time}), which is the whole point of the matching process. Since there is no such baseline period for the unmatched data, however, there are no such balance statistics to be calculated.

The only conceivable strategy to get "unadjusted" and "adjusted" estimates would be to only match on time (using \code{match_method="none"} in \code{\link{match_time}}) and to calculate the balance statistics immediatly afterwards. Then one could adjust for unbalanced covariates using any regular method such as weighting, assessing the balance statistics again afterwards. Once matching on covariates is included in \code{\link{match_time}}, this no longer works.

When calling \code{\link[cobalt]{love.plot}}, estimates are thus also always displayed as "unadjusted". Users can change or entirely remove this labelling through options in the \code{\link[cobalt]{love.plot}} function.
}
\value{
Returns a \code{bal.tab} object.
}
\seealso{
\code{\link[cobalt]{bal.tab}}, \code{\link[cobalt]{love.plot}}
}
\examples{
library(data.table)
library(MatchTime)

# only execute if packages are available
if (requireNamespace("cobalt") & requireNamespace("survival") &
    requireNamespace("MatchIt")) {

library(cobalt)
library(survival)
library(MatchIt)

# set random seed to make the output replicably
set.seed(1234)

# load "heart" data from survival package
data("heart")

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
out <- match_time(transplant ~ age + surgery, data=heart, id="id",
                  match_method="nearest")
bal.tab(out)
}
}
