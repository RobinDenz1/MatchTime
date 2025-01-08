\name{method_brsm}
\alias{method_brsm}

\title{
Balanced Risk Set Matching
}
\description{
This documentation page describes the matching process used when setting \code{method="brsm"} in the \code{\link{match_time}} function. No additional arguments are supported in this case.
}
\details{
Balanced risk set matching was first proposed by Li et al. (2005) and is also known as sequential stratification as proposed by Schaubel et al. (2006). It works by first identifying all times in point at which the treatment status of an individual switches from "control" to "treated" and sorting them from the first to the last. The matching is then performed subsequently at each of these distinct points in time. All individuals whose treatment status changed from "control" to "treated" at \eqn{t} are included in the matched data as "cases". For each included individual, \code{ratio} controls are choosen from those individuals who did not yet receive the treatment at \eqn{t} and are also included in the matched data. The controls are choosen either at random (\code{match_method="none"}) or by matching on the supplied covariates using one of the available \code{match_method}s. The time of inclusion is then considered the "time-zero" for all individuals included in this way.

All other methods implemented in this package use essentially the same methodology. They only differ in how the controls are matched to the cases.
}
\references{
Li, Yunfei Pail, Kathleen J. Propert, and Paul R. Rosenbaum (2001). "Balanced Risk Set Matching". In: Journal of the American Statistical Association 96.455, pp. 870-882.

Schaubel, Douglas E., Robert A. Wolfe, and Friedrich K. Port (2006). "A Sequential Stratification Method for Estimating the Effect of a Time-Dependent Experimental Treatment in Observational Studies". In: Biometrics 62, pp. 910–917.

Thomas, Laine E., Siyun Yang, Daniel Wojdyla, and Douglas E. Schaubel (2020). "Matching with Time-Dependent Treatments: A Review and Look Forward". In: Statistics in Medicine 39, pp. 2350-2370.
}
\author{
Robin Denz
}
\examples{
library(data.table)
library(MatchTime)

if (requireNamespace("survival") & requireNamespace("MatchIt") &
    requireNamespace("ggplot2")) {

library(survival)
library(MatchIt)
library(ggplot2)

# load some example data from the survival package
data("heart", package="survival")

# keep only columns relevant for matching
heart <- heart[, c("id", "start", "stop", "transplant", "age", "surgery")]

## time-dependent matching, using "transplant" as treatment and only
## "surgery" as variable to match on
m.obj <- match_time(transplant ~ surgery, data=heart, id="id",
                    match_method="fast_exact")
}
}
