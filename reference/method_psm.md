# Time-Dependent Propensity Score Matching

This documentation page describes the matching process used when setting
`method="psm"` in the
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

- ps_type:

  A single character string, specifying which type of "propensity score"
  to use. Currently supports `"ps"`, which results in usage of the
  actual hazard of treatment at time \\t\\ and `"lp"`, which instead
  uses only the linear predictor of the model (disregarding the baseline
  hazard), as proposed by Hade et al. (2015). Using `"lp"` is faster
  because the baseline hazard does not need to be estimated and because
  there is no need to calculate the actual hazard each time where
  matching is done. See details.

- standardize_ps:

  Either `TRUE` or `FALSE` (default), specifying whether to standardize
  the "propensity score" as specified by `ps_type` to the 0 / 1 range.
  This might be beneficial for some matching strategies and was, for
  example, used in Richey et al. (2024).

- basehaz_interpol:

  A single character string controlling how the estimated baseline
  hazard should be interpolated. This is only used when `type_ps="ps"`,
  in which case the baseline hazard is estimated from the fitted
  propensity score Cox model using the
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

## Details

All arguments of the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function may be used when using `method="psm"`, because of the general
sequential nature of the matching. The only difference in the matching
process is that matching is not performed directly on the covariates in
`formula`, but instead is done only on an estimated time-dependent
propensity score at each point in time.

***How it works***:

The propensity score is usually defined as the probability that an
individual receives the treatment. In the case of a time-dependent
treatment, the propensity score is instead defined as the probability
that an individual receives the treatment at time t, given that the
individual has not yet received the treatment up to this point. This is
equivalent to the *survival probability*, only that the outcome is the
treatment of interest, not the outcome of interest. By matching
individuals only on this score instead of on the covariates directly,
the groups will be balanced over all points in time (Lu 2005).

***Estimation of the Propensity Score***:

The "propensity score" is estimated using a Cox model with the treatment
variable as dependent variable in the first step. In practice, the
hazard of receiving the treatment is then calculated for all individuals
at each point where matching is performed. Although this is technically
not the same as a regular propensity score, because it is not an actual
probability between 0 and 1, it is referred to as the time-dependent
propensity score in the literature. The matching is then done entirely
using this propensity score, ignoring all other covariates. This may in
some cases be a faster alternative than matching on many covariates at
the same time. The model used to predict the propensity scores is
included in the output object whenever `method="psm"` is used.

Formally, following the Cox model, the hazard of receiving treatment
\\A\\ is given by:

\$\$h_A(t) = h\_{A0}(t) e^{\beta X(t)},\$\$

where \\h\_{A0}(t)\\ is the baseline hazard and \\X(t)\\ stands for an
arbitrary amount of (possibly) time-dependent covariates. By default
(`ps_type="ps"`), the estimated hazard of treatment at \\t\\
\\\left(\hat{h}\_{A}(t)\right)\\ is used in matching. Internally, the
[`basehaz`](https://rdrr.io/pkg/survival/man/basehaz.html) function is
simply called on the Cox model to estimate \\h\_{A0}(t)\\. If there are
no [`strata`](https://rdrr.io/pkg/survival/man/strata.html) terms in the
`formula` used to fit this model, the baseline hazard will be the same
for all individuals at \\t\\, in which case it would be equivalent to
use `ps_type="lp"`, in which case only the linear predictor
\\\left(e^{\beta X(t)}\right)\\ is used for matching.

## References

Lu, Bo (2005). "Propensity Score Matching with Time-Dependent
Covariates". In: Biometrics 61.3, pp. 721-728.

Hade, Erinn M., Giovannie Nattino, Heather A. Frey, and Bo Lu (2020).
"Propensity Score Matching for Treatment Delay Effects with
Observational Survival Data". In: Statistical Methods in Medical
Research 29.3, pp. 695-708.

Richey, Morgan, Matthew Mayiejewski, Lindsay Tepel, David Arterburn,
Aniket Kawatkar, Caroline E. Sloan, and Valerie A. Smith (2024). "A
Comparison of Time-Varying Propensity Score vs Sequential Stratification
Approaches to Longitudinal Matching with a Time-Varying Treatment". In:
BMC Medical Research Methodology 24.280.

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
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
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
#> Call:
#> survival::coxph(formula = stats::as.formula(cox_form), data = d_ps_mod)
#> 
#>   n= 69, number of events= 69 
#>    (34 observations deleted due to missingness)
#> 
#>               coef  exp(coef)   se(coef)      z Pr(>|z|)
#> surgery -0.3621390  0.6961856  0.3105484 -1.166    0.244
#> age      0.0008046  1.0008049  0.0143960  0.056    0.955
#> 
#>         exp(coef) exp(-coef) lower .95 upper .95
#> surgery    0.6962     1.4364    0.3788     1.280
#> age        1.0008     0.9992    0.9730     1.029
#> 
#> Concordance= 0.549  (se = 0.039 )
#> Likelihood ratio test= 1.46  on 2 df,   p=0.5
#> Wald test            = 1.36  on 2 df,   p=0.5
#> Score (logrank) test = 1.38  on 2 df,   p=0.5
#> 
```
