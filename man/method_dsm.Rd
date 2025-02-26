\name{method_dsm}
\alias{method_dsm}

\title{
Time-Dependent Double Score Matching
}
\description{
This documentation page describes the matching process used when setting \code{method="dsm"} in the \code{\link{match_time}} function and gives a detailed explanation on the supported additional arguments in this case.
}
\arguments{
  \item{formula_ps}{
An optional formula object, specifying the right-hand side of the formula that should be used for the propensity score model, or \code{NULL} (default). If \code{NULL}, the right-hand side of the \code{formula} argument is used to fit the model. This argument is useful mostly to specify interactions, non-linear effects or similar thing. Note that this formula object should start with \code{~} and have nothing on the left-hand side.
  }
  \item{formula_prog}{
Same as \code{formula_ps} but used for the propensity score model.
  }
  \item{event}{
A single character string specifying a column in \code{data} containing the event status of interest. The outcome specified by this argument will be used as dependent variable in the prognostic score model. This argument therefore \emph{must} be specified, otherwise an error will be produced. Note that this argument will be treated as an outcome by default and does not have to be added to \code{outcomes} in the main function call.
  }
  \item{ps_type}{
A single character string, specifying which type of "propensity score" to use. Currently supports \code{"ps"}, which results in usage of the actual propensity score at time t and \code{"lp"}, which instead uses only the linear predictor of the model (disregarding the baseline hazard), as proposed by Hade et al. (2015). Using \code{"lp"} is faster because the baseline hazard does not need to be estimated and because there is no need to calculate the actual propensity score on each time where matching is done. There are, however, currently no studies showing that the \code{"lp"} method performs as well as the \code{"ps"} method, which is why it is recommended to keep this at \code{"ps"}.
  }
  \item{prog_type}{
A single character string, specifying which type of "prognostic score" to use. Currently supports \code{"p"}, which results in usage of the outcome probability at time t and \code{"lp"}, which instead uses only the linear predictor of the model (disregarding the baseline hazard). Using \code{"lp"} is faster because the baseline hazard does not need to be estimated and because there is no need to calculate the actual probability on each time where matching is done. There are, however, currently no studies showing that the \code{"lp"} method performs as well as the \code{"p"} method, which is why it is recommended to keep this at \code{"p"}.
  }
  \item{standardize_ps}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether to standardize the "propensity score" as specified by \code{ps_type} to the 0 / 1 range. This might be beneficial for some matching strategies and was, for example, used in Richey et al. (2024).
  }
  \item{standardize_prog}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether to standardize the "prognostic score" as specified by \code{prog_type} to the 0 / 1 range. This might be beneficial for some matching strategies.
  }
  \item{basehaz_interpol}{
A single character string controlling how the estimated baseline hazard should be interpolated. This is only used when \code{type_ps="ps"} and / or \code{type_prog="p"}, in which case the baseline hazard is estimated from the fitted Cox model(s) using the \code{\link{basehaz}} function from the \pkg{survival} package. Allowed values are \code{"constant"} (default) for step function interpolation (as usual for survival curves) or \code{"linear"} for linear interpolation. Should usually be kept at \code{"constant"}. Interpolation is performed internally using the \code{\link[stats]{approxfun}} function with \code{rule=2}.
  }
  \item{remove_ps}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether the estimated propensity score should be removed from \code{data}. If \code{FALSE}, the propensity score at \code{.treat_time} is included in the \code{.ps_score} column.
  }
  \item{remove_prog}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether the estimated prognostic score should be removed from \code{data}. If \code{FALSE}, the prognostic score at \code{.treat_time} is included in the \code{.ps_score} column.
  }
}
\details{
All arguments of the \code{\link{match_time}} function may be used when using \code{method="dsm"}, because of the general sequential nature of the matching. The only difference in the matching process is that matching is not performed directly on the covariates in \code{formula}, but instead is done only on an estimated time-dependent prognostic score \emph{and} on the time-dependent propensity score at each point in time.

\strong{\emph{How it works}}:

This method combines time-dependent propensity score matching (see \code{\link{method_psm}}) and time-dependent prognostic score matching (see \code{\link{method_pgm}}) into a single doubly-robust method by estimating both scores and matching on both of them. Doubly-robust in this case means that only one of the two models has to be correctly specified to obtain unbiased estimates after matching. This property has been studied quite well for regular double risk score matching (see Antonelli et al. 2018 or Leacy et al. 2014), but there is not a lot of literature for the time-dependent case. A notable exception is Li et al (2013).

Because \code{\link{match_time}} allows users to use the \code{\link[MatchIt]{matchit}} function internally, there are many possible options on how exactly the matching should be performed. Users could, for example, use caliper based matching for both scores or simply combine them using the mahalanobis distance, as has been described by

\strong{\emph{Estimation of the propensity and prognostic score}}:

The scores are estimated exactly the same way as they are estimated when using \code{method="psm"} or \code{method="pgm"}. Please consult the documentation page for these methods for more details.

}
\references{
Li, Yun, Douglas E. Schaubel, and Kevin He (2013). "Matching Methods for Obtaining Survival Functions to Estimate the Effect of a Time-Dependent Treatment". In: Statistics in Biosciences 6, pp. 105-126.

Antonelli, Joseph, Matthew Cefalu, Nathan Palmer, and Denis Agniel (2018). "Doubly Robust Matching Estimators for High Dimensional Confounding Adjustment". In: Biometrics 74, pp. 1171-1179.

Leacy, Finbarr P. and Elizabeth A. Stuart (2014). "On the Joint Use of Propensity and Prognostic Scores in Estimation of the Average Treatment Effect on the Treated: A Simulation Study". In: Statistics in Medicine 33.20, pp. 3488-3508.

Zhang, Yunshu, Shu Yanh, Wenyu Ye, Douglas E. Faries, Ilya Lipkovich, and Zbigniew Kadziola (2022). "Practical Recommendations on Double Score Matching for Estimating Causal Effects". In: Statistics in Medicine 41, pp. 1421-1445.
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

## time-dependent double score matching, using "transplant" as treatment
## and surgery + age as variables to match on
m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                    method="dsm", event="event", match_method="nearest",
                    replace_over_t=TRUE)

# showing a summary of the used propensity score model
summary(m.obj$ps_model)

# showing a summary of the used prognostic score model
summary(m.obj$prog_model)
}
}
