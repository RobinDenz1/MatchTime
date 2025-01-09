\name{method_pgm}
\alias{method_pgm}

\title{
Time-Dependent Prognostic Score Matching
}
\description{
This documentation page describes the matching process used when setting \code{method="pgm"} in the \code{\link{match_time}} function and gives a detailed explanation on the supported additional arguments in this case.
}
\arguments{
  \item{event}{
A single character string specifying a column in \code{data} containing the event status of interest. The outcome specified by this argument will be used as dependent variable in the prognostic score model. This argument therefore \emph{must} be specified, otherwise an error will be produced.
  }
  \item{formula_prog}{
An optional formula object, specifying the right-hand side of the formula that should be used for the outcome model, or \code{NULL} (default). If \code{NULL}, the right-hand side of the \code{formula} argument is used to fit the model. This argument is useful mostly to specify interactions, non-linear effects or similar thing. Note that this formula object should start with \code{~} and have nothing on the left-hand side.
  }
  \item{prog_type}{
A single character string, specifying which type of "prognostic score" to use. Currently supports \code{"p"}, which results in usage of the outcome probability at time t and \code{"lp"}, which instead uses only the linear predictor of the model (disregarding the baseline hazard). Using \code{"lp"} is faster because the baseline hazard does not need to be estimated and because there is no need to calculate the actual probability on each time where matching is done. There are, however, currently no studies showing that the \code{"lp"} method performs as well as the \code{"p"} method, which is why it is recommended to keep this at \code{"p"}.
  }
  \item{standardize_prog}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether to standardize the "prognostic score" as specified by \code{prog_type} to the 0 / 1 range. This might be beneficial for some matching strategies.
  }
  \item{basehaz_interpol}{
A single character string controlling how the estimated baseline hazard should be interpolated. This is only used when \code{type_prog="p"}, in which case the baseline hazard is estimated from the fitted prognostic score Cox model using the \code{\link{basehaz}} function from the \pkg{survival} package. Allowed values are \code{"constant"} (default) for step function interpolation (as usual for survival curves) or \code{"linear"} for linear interpolation. Should usually be kept at \code{"constant"}. Interpolation is performed internally using the \code{\link[stats]{approxfun}} function with \code{rule=2}.
  }
}
\details{
All arguments of the \code{\link{match_time}} function may be used when using \code{method="pgm"}, because of the general sequential nature of the matching. The only difference in the matching process is that matching is not performed directly on the covariates in \code{formula}, but instead is done only on an estimated time-dependent prognostic score at each point in time.

\strong{\emph{How it works}}:

Time-dependent prognostic score matching is very similar to the more popular time-dependent propensity score matching (\code{method="psm"}). It works by first estimating the probability that an individual will experience an outcome event at t, given that the individual has not yet experienced the event up to this point and given that the individual did not yet receive the treatment. This is essentially just an estimate of the conditional survival probability, which is a standard quantity in time-to-event analysis. Hansen (2008) showed that matching on this score will result in unbiased estimates of the causal effect of interest. These results have been extended to the time-dependent context by He et al. (2020) and other authors (cited below).

This might seem weird at first. After all, one of the main reasons for using time-dependent matching is that it avoids fitting outcome models. Cox models with time-dependent variables also generally do not allow estimation of marginal effects in the presence of treatment-outcome feedback and other complex causal feedback loops. The important thing to keep in mind here is that the fitted outcome model is \emph{not} used to actually estimate effects. It is a pure nuisance model, only used to predict the conditional hazard of the outcome given covariates under control conditions, which does not require as many assumptions as an actual causal analysis.

\strong{\emph{Estimation of the prognostic score}}:

The prognostic score is estimated using a Cox model with the actual outcome of interest as the dependent variable in the first step. All variables on the RHS of the \code{formula} are used as independent variables (time-fixed and time-dependent ones). Importantly, only the time under control conditions for all individuals is used in this model. Individuals who do receive the treatment at some point during the observation period are artificially censored at this point in time. The model therefore does not include the treatment status as covariate.

The hazard of experiencing an outcome event, e.g. the time-dependent propensity score, is then calculated for all individuals at each point where matching is performed. The matching is then done entirely using this prognostic score, ignoring all other covariates. The model used to predict the prognostic scores is included in the output object whenever \code{method="pgm"} is used.

\strong{\emph{Interval Coding}}:

Because the \code{event} column of \code{data} will be used as the outcome in the Cox model as described above, its' intervals should be coded in the standard time-to-event outcome way as described in the \pkg{survival} package. That means that instead of new intervals starting at the occurence of an event, existing intervals should be broken up at the event times with \code{event} being \code{1} or \code{TRUE} only at exactly these times. One option to create such data is to use the \code{\link{merge_start_stop}} function of this package with appropriate input to the \code{event_times} argument.

}
\references{
Hansen, Ben B. (2008). "The Prognostic Analogue of the Propensity Score". In: Biometrika 95.2, pp. 481-488.

He, Kevin, Yun Li, Panduranga S. Rao, Randall S. Sung, and Douglas E. Schaubel (2020). "Prognostic Score Matching Methods for Estimating the Average Effect of a Non-Reversible Binary Time-Dependent Treatment on the Survival Function". In: Lifetime Data Analysis 26, pp. 451-470.

Li, Yun, Douglas E. Schaubel, and Kevin He (2013). "Matching Methods for Obtaining Survival Functions to Estimate the Effect of a Time-Dependent Treatment". In: Statistics in Biosciences 6, pp. 105-126.

Smith, Abigail R. and Douglas E. Schaubel (2015). "Time-Dependent Prognostic Score Matching for Recurrent Event Analysis to Evaluate a Treatment Assigned During Follow-Up". In: Biometrics 71, pp. 950-959.
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

## time-dependent propensity score matching, using "transplant" as treatment
## and surgery + age as variables to match on
m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                    method="pgm", event="event", match_method="nearest",
                    replace_over_t=TRUE)

# showing a summary of the used prognostic score model
summary(m.obj$prog_model)
}
}
