# Time-Dependent Double Score Matching

This documentation page describes the matching process used when setting
`method="dsm"` in the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function and gives a detailed explanation on the supported additional
arguments in this case.

## Arguments

- formula_ps:

  An optional formula object, specifying the right-hand side of the
  formula that should be used for the propensity score model, or `NULL`
  (default). If `NULL`, the right-hand side of the `formula` argument is
  used to fit the model. This argument is useful mostly to specify
  interactions, non-linear effects or similar thing. Note that this
  formula object should start with `~` and have nothing on the left-hand
  side.

- formula_prog:

  Same as `formula_ps` but used for the propensity score model.

- event:

  A single character string specifying a column in `data` containing the
  event status of interest. The outcome specified by this argument will
  be used as dependent variable in the prognostic score model. This
  argument therefore *must* be specified, otherwise an error will be
  produced. Note that this argument will be treated as an outcome by
  default and does not have to be added to `outcomes` in the main
  function call.

- ps_type:

  A single character string, specifying which type of "propensity score"
  to use. Currently supports `"ps"`, which results in usage of the
  actual propensity score at time t and `"lp"`, which instead uses only
  the linear predictor of the model (disregarding the baseline hazard),
  as proposed by Hade et al. (2015). Using `"lp"` is faster because the
  baseline hazard does not need to be estimated and because there is no
  need to calculate the actual propensity score on each time where
  matching is done. There are, however, currently no studies showing
  that the `"lp"` method performs as well as the `"ps"` method, which is
  why it is recommended to keep this at `"ps"`.

- prog_type:

  A single character string, specifying which type of "prognostic score"
  to use. Currently supports `"p"`, which results in usage of the
  outcome probability at time t and `"lp"`, which instead uses only the
  linear predictor of the model (disregarding the baseline hazard).
  Using `"lp"` is faster because the baseline hazard does not need to be
  estimated and because there is no need to calculate the actual
  probability on each time where matching is done. There are, however,
  currently no studies showing that the `"lp"` method performs as well
  as the `"p"` method, which is why it is recommended to keep this at
  `"p"`.

- standardize_ps:

  Either `TRUE` or `FALSE` (default), specifying whether to standardize
  the "propensity score" as specified by `ps_type` to the 0 / 1 range.
  This might be beneficial for some matching strategies and was, for
  example, used in Richey et al. (2024).

- standardize_prog:

  Either `TRUE` or `FALSE` (default), specifying whether to standardize
  the "prognostic score" as specified by `prog_type` to the 0 / 1 range.
  This might be beneficial for some matching strategies.

- basehaz_interpol:

  A single character string controlling how the estimated baseline
  hazard should be interpolated. This is only used when `type_ps="ps"`
  and / or `type_prog="p"`, in which case the baseline hazard is
  estimated from the fitted Cox model(s) using the
  [`basehaz`](https://rdrr.io/pkg/survival/man/basehaz.html) function
  from the survival package. Allowed values are `"constant"` (default)
  for step function interpolation (as usual for survival curves) or
  `"linear"` for linear interpolation. Should usually be kept at
  `"constant"`. Interpolation is performed internally using the
  [`approxfun`](https://rdrr.io/r/stats/approxfun.html) function with
  `rule=2`.

- remove_ps:

  Either `TRUE` or `FALSE` (default), specifying whether the estimated
  propensity score should be removed from `data`. If `FALSE`, the
  propensity score at `.treat_time` is included in the `.ps_score`
  column.

- remove_prog:

  Either `TRUE` or `FALSE` (default), specifying whether the estimated
  prognostic score should be removed from `data`. If `FALSE`, the
  prognostic score at `.treat_time` is included in the `.ps_score`
  column.

## Details

All arguments of the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function may be used when using `method="dsm"`, because of the general
sequential nature of the matching. The only difference in the matching
process is that matching is not performed directly on the covariates in
`formula`, but instead is done only on an estimated time-dependent
prognostic score *and* on the time-dependent propensity score at each
point in time.

***How it works***:

This method combines time-dependent propensity score matching (see
[`method_psm`](https://robindenz1.github.io/MatchTime/reference/method_psm.md))
and time-dependent prognostic score matching (see
[`method_pgm`](https://robindenz1.github.io/MatchTime/reference/method_pgm.md))
into a single doubly-robust method by estimating both scores and
matching on both of them. Doubly-robust in this case means that only one
of the two models has to be correctly specified to obtain unbiased
estimates after matching. This property has been studied quite well for
regular double risk score matching (see Antonelli et al. 2018 or Leacy
et al. 2014), but there is not a lot of literature for the
time-dependent case. A notable exception is Li et al (2013).

Because
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
allows users to use the
[`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
function internally, there are many possible options on how exactly the
matching should be performed. Users could, for example, use caliper
based matching for both scores or simply combine them using the
mahalanobis distance, as has been described by

***Estimation of the propensity and prognostic score***:

The scores are estimated exactly the same way as they are estimated when
using `method="psm"` or `method="pgm"`. Please consult the documentation
page for these methods for more details.

## References

Li, Yun, Douglas E. Schaubel, and Kevin He (2013). "Matching Methods for
Obtaining Survival Functions to Estimate the Effect of a Time-Dependent
Treatment". In: Statistics in Biosciences 6, pp. 105-126.

Antonelli, Joseph, Matthew Cefalu, Nathan Palmer, and Denis Agniel
(2018). "Doubly Robust Matching Estimators for High Dimensional
Confounding Adjustment". In: Biometrics 74, pp. 1171-1179.

Leacy, Finbarr P. and Elizabeth A. Stuart (2014). "On the Joint Use of
Propensity and Prognostic Scores in Estimation of the Average Treatment
Effect on the Treated: A Simulation Study". In: Statistics in Medicine
33.20, pp. 3488-3508.

Zhang, Yunshu, Shu Yanh, Wenyu Ye, Douglas E. Faries, Ilya Lipkovich,
and Zbigniew Kadziola (2022). "Practical Recommendations on Double Score
Matching for Estimating Causal Effects". In: Statistics in Medicine 41,
pp. 1421-1445.

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
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
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
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
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
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
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
