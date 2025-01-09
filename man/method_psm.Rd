\name{method_psm}
\alias{method_psm}

\title{
Time-Dependent Propensity Score Matching
}
\description{
This documentation page describes the matching process used when setting \code{method="psm"} in the \code{\link{match_time}} function and gives a detailed explanation on the supported additional arguments in this case.
}
\arguments{
  \item{formula_ps}{
An optional formula object, specifying the right-hand side of the formula that should be used for the propensity score model, or \code{NULL} (default). If \code{NULL}, the right-hand side of the \code{formula} argument is used to fit the model. This argument is useful mostly to specify interactions, non-linear effects or similar thing. Note that this formula object should start with \code{~} and have nothing on the left-hand side.
  }
  \item{ps_type}{
A single character string, specifying which type of "propensity score" to use. Currently supports \code{"ps"}, which results in usage of the actual propensity score at time t and \code{"lp"}, which instead uses only the linear predictor of the model (disregarding the baseline hazard), as proposed by Hade et al. (2015). Using \code{"lp"} is faster because the baseline hazard does not need to be estimated and because there is no need to calculate the actual propensity score on each time where matching is done. There are, however, currently no studies showing that the \code{"lp"} method performs as well as the \code{"ps"} method, which is why it is recommended to keep this at \code{"ps"}.
  }
  \item{standardize_ps}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether to standardize the "propensity score" as specified by \code{ps_type} to the 0 / 1 range. This might be beneficial for some matching strategies and was, for example, used in Richey et al. (2024).
  }
  \item{basehaz_interpol}{
A single character string controlling how the estimated baseline hazard should be interpolated. This is only used when \code{type_ps="ps"}, in which case the baseline hazard is estimated from the fitted propensity score Cox model using the \code{\link{basehaz}} function from the \pkg{survival} package. Allowed values are \code{"constant"} (default) for step function interpolation (as usual for survival curves) or \code{"linear"} for linear interpolation. Should usually be kept at \code{"constant"}. Interpolation is performed internally using the \code{\link[stats]{approxfun}} function with \code{rule=2}.
  }
}
\details{

All arguments of the \code{\link{match_time}} function may be used when using \code{method="psm"}, because of the general sequential nature of the matching. The only difference in the matching process is that matching is not performed directly on the covariates in \code{formula}, but instead is done only on an estimated time-dependent propensity score at each point in time.

\strong{\emph{How it works}}:

The propensity score is usually defined as the probability that an individual receives the treatment. In the case of a time-dependent treatment, the propensity score is instead defined as the probability that an individual receives the treatment at time t, given that the individual has not yet received the treatment up to this point. This is equivalent to the \emph{survival probability}, only that the outcome is the treatment of interest, not the outcome of interest. By matching individuals only on this score instead of on the covariates directly, the groups will be balanced over all points in time (Lu 2005).

\strong{\emph{Estimation of the Propensity Score}}:

The propensity score is estimated using a Cox model with the treatment variable as dependent variable in the first step. The hazard of receiving the treatment, e.g. the time-dependent propensity score, is then calculated for all individuals at each point where matching is performed. The matching is then done entirely using this propensity score, ignoring all other covariates. This may in some cases be a faster alternative than matching on many covariates at the same time. The model used to predict the propensity scores is included in the output object whenever \code{method="psm"} is used.
}
\references{
Lu, Bo (2005). "Propensity Score Matching with Time-Dependent Covariates". In: Biometrics 61.3, pp. 721-728.

Hade, Erinn M., Giovannie Nattino, Heather A. Frey, and Bo Lu (2020). "Propensity Score Matching for Treatment Delay Effects with Observational Survival Data". In: Statistical Methods in Medical Research 29.3, pp. 695-708.

Richey, Morgan, Matthew Mayiejewski, Lindsay Tepel, David Arterburn, Aniket Kawatkar, Caroline E. Sloan, and Valerie A. Smith (2024). "A Comparison of Time-Varying Propensity Score vs Sequential Stratification Approaches to Longitudinal Matching with a Time-Varying Treatment". In: BMC Medical Research Methodology 24.280.
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

## time-dependent propensity score matching, using "transplant" as treatment
## and surgery + age as variables to match on
m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                    method="psm", match_method="nearest",
                    replace_over_t=TRUE)

# showing a summary of the used propensity score model
summary(m.obj$ps_model)
}
}
