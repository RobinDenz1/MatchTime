# Time-Dependent Prognostic Score Matching

This documentation page describes the matching process used when setting
`method="pgm"` in the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function and gives a detailed explanation on the supported additional
arguments in this case.

## Arguments

- event:

  A single character string specifying a column in `data` containing the
  event status of interest. The outcome specified by this argument will
  be used as dependent variable in the prognostic score model. This
  argument therefore *must* be specified, otherwise an error will be
  produced. Note that this argument will be treated as an outcome by
  default and does not have to be added to `outcomes` in the main
  function call.

- formula_prog:

  An optional formula object, specifying the right-hand side of the
  formula that should be used for the outcome model, or `NULL`
  (default). If `NULL`, the right-hand side of the `formula` argument is
  used to fit the model. This argument is useful mostly to specify
  interactions, non-linear effects or similar thing. Note that this
  formula object should start with `~` and have nothing on the left-hand
  side.

- prog_type:

  A single character string, specifying which type of "prognostic score"
  to use. Currently supports `"p"`, which results in usage of the hazard
  of the outcome at time \\t\\ and `"lp"`, which instead uses only the
  linear predictor of the model (disregarding the baseline hazard).
  Using `"lp"` is faster because the baseline hazard does not need to be
  estimated and because there is no need to calculate the actual hazard
  on each time where matching is done. See details.

- standardize_prog:

  Either `TRUE` or `FALSE` (default), specifying whether to standardize
  the "prognostic score" as specified by `prog_type` to the 0 / 1 range.
  This might be beneficial for some matching strategies.

- basehaz_interpol:

  A single character string controlling how the estimated baseline
  hazard should be interpolated. This is only used when `type_prog="p"`,
  in which case the baseline hazard is estimated from the fitted
  prognostic score Cox model using the
  [`basehaz`](https://rdrr.io/pkg/survival/man/basehaz.html) function
  from the survival package. Allowed values are `"constant"` (default)
  for step function interpolation (as usual for survival curves) or
  `"linear"` for linear interpolation. Should usually be kept at
  `"constant"`. Interpolation is performed internally using the
  [`approxfun`](https://rdrr.io/r/stats/approxfun.html) function with
  `rule=2`.

- remove_prog:

  Either `TRUE` or `FALSE` (default), specifying whether the estimated
  prognostic score should be removed from `data`. If `FALSE`, the
  prognostic score at `.treat_time` is included in the `.prog_score`
  column.

## Details

All arguments of the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function may be used when using `method="pgm"`, because of the general
sequential nature of the matching. The only difference in the matching
process is that matching is not performed directly on the covariates in
`formula`, but instead is done only on an estimated time-dependent
prognostic score at each point in time.

***How it works***:

Time-dependent prognostic score matching is very similar to the more
popular time-dependent propensity score matching (`method="psm"`). It
works by first estimating the probability that an individual will
experience an outcome event at t, given that the individual has not yet
experienced the event up to this point and given that the individual did
not yet receive the treatment. This is essentially just an estimate of
the conditional survival probability, which is a standard quantity in
time-to-event analysis. Hansen (2008) showed that matching on this score
will result in unbiased estimates of the causal effect of interest.
These results have been extended to the time-dependent context by He et
al. (2020) and other authors (cited below).

This might seem weird at first. After all, one of the main reasons for
using time-dependent matching is that it avoids fitting outcome models.
Cox models with time-dependent variables also generally do not allow
estimation of marginal effects in the presence of treatment-outcome
feedback and other complex causal feedback loops. The important thing to
keep in mind here is that the fitted outcome model is *not* used to
actually estimate effects. It is a pure nuisance model, only used to
predict the conditional hazard of the outcome given covariates under
control conditions, which does not require as many assumptions as an
actual causal analysis.

***Estimation of the prognostic score***:

The prognostic score is estimated using a Cox model with the actual
outcome of interest as the dependent variable in the first step. All
variables on the RHS of the `formula` are used as independent variables
(time-fixed and time-dependent ones). Importantly, only the time under
control conditions for all individuals is used in this model.
Individuals who do receive the treatment at some point during the
observation period are artificially censored at this point in time. The
model therefore does not include the treatment status as covariate.

The hazard of experiencing an outcome event, e.g. the time-dependent
prognostic score, is then calculated for all individuals at each point
where matching is performed. The matching is then done entirely using
this prognostic score, ignoring all other covariates. The model used to
predict the prognostic scores is included in the output object whenever
`method="pgm"` is used.

Formally, the model can be described as:

\$\$h_Y(t) = h\_{Y0}(t) e^{\beta X(t)},\$\$

where \\X(t)\\ refers to an arbitrary amount of (possibly)
time-dependent covariates. The matching is done either on
\\\hat{h}\_Y(t)\\ (when using `prog_type="p"`) or only on the linear
predictor \\e^{\beta X(t)}\\ (when using `prog_type="lp"`). As explained
in
[`?method_psm`](https://robindenz1.github.io/MatchTime/reference/method_psm.md),
this only makes a difference when
[`strata`](https://rdrr.io/pkg/survival/man/strata.html) terms are
included in the Cox model `formula`, in which case the default of `"p"`
should be preferred.

***Interval Coding***:

Because the `event` column of `data` will be used as the outcome in the
Cox model as described above, its' intervals should be coded in the
standard time-to-event outcome way as described in the survival package.
That means that instead of new intervals starting at the occurence of an
event, existing intervals should be broken up at the event times with
`event` being `1` or `TRUE` only at exactly these times. One option to
create such data is to use the
[`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md)
function of this package with appropriate input to the `event_times`
argument.

## References

Hansen, Ben B. (2008). "The Prognostic Analogue of the Propensity
Score". In: Biometrika 95.2, pp. 481-488.

He, Kevin, Yun Li, Panduranga S. Rao, Randall S. Sung, and Douglas E.
Schaubel (2020). "Prognostic Score Matching Methods for Estimating the
Average Effect of a Non-Reversible Binary Time-Dependent Treatment on
the Survival Function". In: Lifetime Data Analysis 26, pp. 451-470.

Li, Yun, Douglas E. Schaubel, and Kevin He (2013). "Matching Methods for
Obtaining Survival Functions to Estimate the Effect of a Time-Dependent
Treatment". In: Statistics in Biosciences 6, pp. 105-126.

Smith, Abigail R. and Douglas E. Schaubel (2015). "Time-Dependent
Prognostic Score Matching for Recurrent Event Analysis to Evaluate a
Treatment Assigned During Follow-Up". In: Biometrics 71, pp. 950-959.

## Author

Robin Denz

## Examples

``` r
library(data.table)
library(MatchTime)

if (requireNamespace("survival") & requireNamespace("MatchIt") &
    requireNamespace("ggplot2")) {

library(survival)
library(MatchIt)
library(ggplot2)

# load some example data from the survival package
data("heart", package="survival")

## time-dependent prognostic score matching, using "transplant" as treatment
## and surgery + age as variables to match on
m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                    method="pgm", event="event", match_method="nearest",
                    replace_over_t=TRUE)

# showing a summary of the used prognostic score model
summary(m.obj$prog_model)
}
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Call:
#> survival::coxph(formula = stats::as.formula(cox_form), data = d_outcome)
#> 
#>   n= 103, number of events= 30 
#> 
#>             coef exp(coef) se(coef)      z Pr(>|z|)
#> surgery -0.56342   0.56926  0.61405 -0.918    0.359
#> age      0.01458   1.01469  0.01843  0.791    0.429
#> 
#>         exp(coef) exp(-coef) lower .95 upper .95
#> surgery    0.5693     1.7567    0.1709     1.897
#> age        1.0147     0.9855    0.9787     1.052
#> 
#> Concordance= 0.573  (se = 0.063 )
#> Likelihood ratio test= 1.52  on 2 df,   p=0.5
#> Wald test            = 1.4  on 2 df,   p=0.5
#> Score (logrank) test = 1.43  on 2 df,   p=0.5
#> 
```
