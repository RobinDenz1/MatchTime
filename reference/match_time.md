# Matching for Causal Inference with Time-Dependent Treatments

This function implements multiple methods to match untreated controls to
treated individuals in a time-dependent fashion as described in Thomas
et al. (2020). This approach is also known as *sequential trial
emulation*. In contrast to other implementations, this function supports
continuous and datetime input and allows matching directly on time-fixed
and time-dependent covariates at the same time. It internally uses the
data.table package to keep the function fast and allows users to use the
[`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
function from the excellent MatchIt package to perform the actual
matching at each point in time for more flexibility.

Similar to
[`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html),
this page only documents the overall use of `match_time()`. Specifics on
how `match_time()` works with individual methods the individual pages
linked in the Detail section should be consulted.

## Usage

``` r
match_time(formula, data, id, inclusion=NA,
           outcomes=NA, start="start", stop="stop",
           method=c("brsm", "psm", "pgm",
                    "dsm", "greedy"),
           replace_over_t=FALSE, replace_at_t=FALSE,
           replace_cases=TRUE, estimand="ATT", ratio=1,
           recruitment_start=NULL, recruitment_stop=NULL,
           match_method="fast_exact",
           matchit_args=list(), save_matchit=FALSE,
           censor_at_treat=TRUE, censor_pairs=FALSE,
           units="auto", verbose=FALSE, ...)
```

## Arguments

- formula:

  A `formula` object with a binary treatment variable on the left hand
  side and the covariates to be balanced on the right hand side.
  Interactions and functions of covariates are currently not allowed.
  The treatment variable is ideally coded as a logical variable (`TRUE`
  = treatment, `FALSE` = control). See details for how the "treated"
  group is identified with other input types.

- data:

  A `data.table` like object in the start-stop format, containing
  information about variables that are time-invariant or time-dependent
  and potentially outcomes. Each row corresponds to a period of time in
  which no variables changed. These intervals are defined by the `start`
  and `stop` columns. The `start` column gives the time at which the
  period started, the `stop` column denotes the time when the period
  ended. Intervals should be coded to be *right-open* (corresponds to
  `[start, stop)`). Continuous (float) and discrete (integer, datetime)
  values are supported for both time columns. The dataset should also
  include an `id` variable (see argument `id`). See details and examples
  for more information, including how events should be coded.

- id:

  A single character string specifying the unique case identifier in
  `data`.

- inclusion:

  An optional character vector specifying logical variables in `data`
  used as inclusion criteria. These should be `TRUE` if the period
  specified by the `start` and `stop` columns in `data` corresponds to a
  period in which the individual fufills the inclusion criteria and
  `FALSE` if the individual does not. All periods where any of the named
  variables are `FALSE` will be excluded from the matching process
  dynamically. By supplying the criteria as separate columns, reasons
  for exclusions will be automatically included in the output. Set to
  `NA` to not use this functionality (default).

- outcomes:

  An optional character vector to specify which logical variables in
  `data` should be treated as (potentially right-censored) outcomes.
  These should be coded differently than time-dependent variables as
  explained throughout the documentation and the relevant vignette.
  Columns named in this argument will be re-coded in the output so that
  they appear as the time until the first occurrence of the respective
  outcome after inclusion in the matching process. This is equivalent to
  calling the
  [`add_outcome`](https://robindenz1.github.io/MatchTime/reference/add_outcome.md)
  function on the output object once for each value in `outcomes`. Note
  that all `outcomes` cannot be matched on, because they occur *after*
  the respective point in time. Set to `NA` to not use this
  functionality (default).

- start:

  A single character string specifying a column in `data` specifying the
  beginning of a time-interval. Defaults to `"start"`.

- stop:

  A single character string specifying a column in `data` specifying the
  end of a time-interval. Defaults to `"stop"`.

- method:

  A single character string, specifying which method should be used to
  select the controls. Currently supports
  [`"brsm"`](https://robindenz1.github.io/MatchTime/reference/method_brsm.md)
  for balanced risk set matching,
  [`"psm"`](https://robindenz1.github.io/MatchTime/reference/method_brsm.md)
  for time-dependent propensity score matching,
  [`"pgm"`](https://robindenz1.github.io/MatchTime/reference/method_brsm.md)
  for time-dependent prognostic score matching,
  [`"dsm"`](https://robindenz1.github.io/MatchTime/reference/method_dsm.md)
  for double score matching and
  [`"greedy"`](https://robindenz1.github.io/MatchTime/reference/method_greedy.md)
  in which simply all available controls are taken at each step.
  Depending on the method, further arguments may be allowed and or
  required. These are explained on the method-specific documentation
  page. See details for more information.

- replace_over_t:

  Whether to allow usage of the same individuals as controls at multiple
  points in time. If `TRUE`, the same person may be used as control at
  every point in time until it switches from being a control to being a
  case. When using `method="greedy"`, this argument is always treated as
  `TRUE`.

- replace_at_t:

  Whether to allow usage of the same individuals as controls at the same
  point in time. If `match_method` is set to a valid method in
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  this argument will be passed to the `replace` argument of the
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  function.

- replace_cases:

  Whether to include individuals that have already been used as controls
  as cases if they also get the treatment later. This is purely
  experimental and should usually stay at its default value of `TRUE`
  regardless of `method` and other arguments, unless there are some good
  reasons to change it.

- estimand:

  Currently only allows `"ATT"` to get a dataset with which to estimate
  the average treatment effect on the treated (because controls are
  choosen to be similar to treated individuals). Other values are
  currently not supported. Note that this argument is *not* passed to
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  when `match_method` is set to a valid method in
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html).
  It would simply not make sense to use anything but "ATT" here.

- ratio:

  How many control units should be matched to each treated unit in k:1
  matching. Should be a single integer value. The default is 1 for 1:1
  matching. If `match_method` is set to a valid method in
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html),
  this argument will be passed to the argument of the same name in the
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  function.

- recruitment_start:

  An optional single value specifying the time at which the matching
  process should start. This will be the first time at which individuals
  receiving the treatment are included in the matching process. Note
  that individuals receiving the treatment *before* this time period
  will be considered *not treated*, regardless of whether they did
  receive the treatment before this date or not. If users want to
  exclude previously treated individuals, the `inclusion` argument
  should be used or the input `data` should be modified accordingly. Set
  to `NULL` to use all available information in `data` (default).

- recruitment_stop:

  An optional single value specifying the time at which the matching
  process should stop. This will be the last time at which people are
  still included in the matching process. Set to `NULL` to use all
  available information in `data` (default).

- match_method:

  A single character string specifying which method should be used to
  perform matching at each point in time. Allowed values are `"none"`
  (to perform no matching on covariates), `"fast_exact"` (default, to
  use fast exact matching as implemented in the
  [`fast_exact_matching`](https://robindenz1.github.io/MatchTime/reference/fast_exact_matching.md)
  function of this package) or any valid method of the
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  function. If the latter is used, this argument is passed to the
  `method` argument of the
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  function directly. Further arguments may be passed to
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  in this case using the `matchit_args` argument.

- matchit_args:

  A named list of further arguments that should be passed to
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  when `match_method` is set to a valid method in
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html).

- save_matchit:

  Whether to save the objects created by each
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  call at different points in time when using a `match_method` that is
  used in
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html).
  If set to `TRUE`, the `matchit_objects` list will include one entry
  per point in time at which matching was performed. Defaults to `FALSE`
  to save RAM space.

- censor_at_treat:

  Only used when `outcomes` is specified. Either `TRUE` or `FALSE`,
  indicating whether the created event time should be censored at the
  time of the next treatment. This only applies to cases that were
  included in the matching process as controls but later become cases
  themselves. Defaults to `TRUE`.

- censor_pairs:

  Only used when `outcomes` is specified. Either `TRUE` or `FALSE`. Only
  used if `censor_at_treat=TRUE`. If set to `TRUE`, the case matched to
  a control is censored at the same time that the control was censored
  due to the next treatment occurring. If `ratio > 1`, the minimum time
  to "artificial censoring" is used as censoring time for all cases with
  the same `.id_pair`. This may in some cases be sufficient to deal with
  the covariate dependent censoring induced by using
  `censor_at_treat=TRUE`.

- units:

  Only used when `outcomes` is specified. Corresponds to the argument of
  the same name in the
  [`difftime`](https://rdrr.io/r/base/difftime.html) function. This
  argument is only used when the `start` and `stop` columns correspond
  to a `Date` (or similar) variable. It should be used to indicate the
  time-scale of the created event time (seconds, days, years, ...).

- verbose:

  Whether to print a summary of how many matches were made for each
  point in time or not (default). This argument is *not* passed to the
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  function if used.

- ...:

  Further `method` specific arguments that may be specified. For more
  information on which method specific arguments are allowed per method,
  please consult the documentation page of the respective method. Those
  can be accessed using, for example,
  [`?method_brsm`](https://robindenz1.github.io/MatchTime/reference/method_brsm.md)
  or
  [`?method_psm`](https://robindenz1.github.io/MatchTime/reference/method_psm.md).

## Details

***How it works***:

This function offers a very general implementation of multiple methods
for time-dependent matching, also known as *sequential trial emulation*.
It works by first identifying all times in point at which the treatment
status of an individual switches from "control" to "treated" and sorting
them from the first to the last. The matching is then performed
subsequently at each of these distinct points in time. All individuals
whose treatment status changed from "control" to "treated" at \\t\\ are
included in the matched data as "cases". For each included individual,
`ratio` controls are choosen from those individuals who did not yet
receive the treatment at \\t\\ and are also included in the matched
data. The controls can be choosen based on direct matching on covariates
(`method="brsm"` - *balanced risk set matching*) or by matching on
scores estimated using a Cox regression model (`method="psm"`,
`method="pgm"` or `method="dsm"`) or by simply taking all of them
(`method="greedy"`). The time of inclusion is then considered the
"time-zero" for all individuals included in this way.

Individuals who were included as controls at some point will usually
still be included as cases when they switch to "treated", unless
`replace_cases=FALSE`. Controls may be picked as controls multiple times
at the same \\t\\ (argument `replace_at_t`) and / or over multiple
points in time (argument `replace_over_t`). The argument `match_method`
controls how exactly the controls are choosen. It is possible to just
pick them at random (`match_method="none"`) or to pick them by classical
matching methods (setting `match_method` to `"fast_exact"`, `"nearest"`,
etc.).

The result is a dataset that can be analyzed using standard
time-to-event methods, without the need to use special methods, such as
marginal structural models, to adjust for treatment-confounder feedback
or other forms of time-dependent confounding. More details and examples
are given in the cited literature and the vignettes of this package.

***Implemented methods***:

Currently, this function supports the following `method`s:

- "[brsm](https://robindenz1.github.io/MatchTime/reference/method_brsm.md)":
  balanced risk set matching

- "[psm](https://robindenz1.github.io/MatchTime/reference/method_psm.md)":
  time-dependent propensity score matching

- "[pgm](https://robindenz1.github.io/MatchTime/reference/method_pgm.md)":
  time-dependent prognostic score matching

- "[dsm](https://robindenz1.github.io/MatchTime/reference/method_dsm.md)":
  time-dependent double score matching

- "[greedy](https://robindenz1.github.io/MatchTime/reference/method_greedy.md)":
  time-dependent greedy selection of controls

All of these methods are implemented in a sequential way and do not
differ that much from each other. The main difference is that the
selection of controls at each point in time is done differently. When
using `method="brsm"` the controls can be picked based on direct
matching on the covariates (controlled using the `match_method`
argument). In contrast, when using score based methods, the matching is
done exclusively on the estimated time-dependent score (propensity and /
or prognostic score). When using `method="greedy"`, all possible
controls are taken at each point in time.

***Identifying the "treated" group***:

Ideally, the treatment specified on the LHS of the `formula` argument is
coded as a logical variable, where `TRUE` corresponds to the "treated"
group and `FALSE` corresponds to the "control" group. If this is not the
case, this function will coerce it to this type internally using the
following rules:

1.) if the variable only consists of the numbers `0` and `1` (coded as
numeric), `0` will be considered the "control" group and `1` the
"treated" group; 2.) otherwise, if the variable is a factor,
`levels(treat)[1]` will be considered the "control" group and the other
value the "treated" group; 3.) otherwise `sort(unique(treat))[1]` will
be considered "control" and the other value the treated. It is safest to
ensure that the treatment variable is a logical variable. In either
case, the output will only contain the treatment as logical variable.

***Interval Coding***:

The intervals supplied to the `data` argument are required to be
*right-open* intervals `[start, stop)`, which is the usual data format
expected for time-to-event modelling and corresponds to the interval
format of the `tmerge` function of the survival package. As a
consequence, intervals of length 0 (where `start==stop`) are not
supported and will result in an error message. Note that events should
be coded differently, as described in the
[`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md)
function. At least one outcome *needs* to be included when using
`method="pgm"` or `method="dsm"`, but users may include an arbitrary
amount of additional outcome event variables using the `outcomes`
argument.

***Adding more Variables***

Users usually want to add outcomes and / or further baseline covariates
to the data after matching. This can be done using the `add_outcome` and
`add_covariate` functions and is described in detail in the respective
documentation and vignette.

***Assessing Covariate Balance***

The balance of the covariates at baseline can be assessed using the
associated
[`summary.match_time`](https://robindenz1.github.io/MatchTime/reference/summary.match_time.md)
or
[`bal.tab.match_time`](https://robindenz1.github.io/MatchTime/reference/bal.tab.match_time.md)
functions. Note that with time-dependent matching it is only possible to
assess balance at baseline, because the treatment is also
time-dependent. It is recommended to assess the covariate balance
whenever a method is used that is supposed to create such balance. When
using `method="greedy"` or `method="brsm"` with `match_method="none"`,
however, it is not needed.

***Performance Considerations***

This function was designed to be work on very large datasets (~ 20
million rows) with large amounts of points in time (\> 1000) on regular
computers. It achieves this through the use of the incredible data.table
package. While it does work with such large datasets, it does become
slow due to the inherent computational complexity of the method. With
large data, using complicated matching methods such as
`match_method="genetic"` is not feasible. However, only matching on time
`match_method="none"` or matching only on some categorical variables
using `match_method="fast_exact"` should still work.

## Value

Returns a `match_time` object containing the following objects:

- data:

  A `data.table` containing the matched data. Note that this dataset
  also contains unmatched cases. To obtain a dataset without unmatched
  individuals, please use the
  [`get_match_data`](https://robindenz1.github.io/MatchTime/reference/get_match_data.md)
  function. The dataset here contains at least the following columns:

  `id`: the original `id` used in the supplied `data`,

  `.id_new`: a new case-specific id in which `id`s who occur multiple
  times are treated as distinct values,

  `.id_pair`: an id to distinguish the matched pairs, if applicable.
  This column is not created when `match_method` is set to a method in
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  that does not allow usage of
  [`get_matches`](https://kosukeimai.github.io/MatchIt/reference/match_data.html),

  `.treat`: the supplied treatment variable,

  `.treat_time`: the time at which the `id` was included in the matching
  process,

  `.next_treat_time`: for controls that later receive treatment, the
  time at which they received the treatment,

  `.fully_matched`: a logical variable that is `TRUE` if the
  corresponding `.id_pair` consists of one case and `ratio` matched
  controls and `FALSE` otherwise,

  `.weights`: a column containing the matching weights, generated
  separately at each point in time.

  `.ps_score`: Only included if `method="psm"` or `method="dsm"` and
  `remove_ps=FALSE`. Contains the estimates "propensity score" at
  `.treat_time`.

  `.prog_score`: Only included if `method="pgm"` or `method="dsm"` and
  `remove_prog=FALSE`. Contains the estimates "prognostic score" at
  `.treat_time`.

  Potentially contains any number of additional covariates supplied in
  the original `data`, plus potential further variables added using
  [`add_outcome`](https://robindenz1.github.io/MatchTime/reference/add_outcome.md),
  [`add_next_time`](https://robindenz1.github.io/MatchTime/reference/add_next_time.md)
  or similar functions.

- d_longest:

  A `data.table` containing the last time under observation for each
  `id` in the supplied `data`.

- trace:

  A `data.table` containing four columns: `time` (the time at which
  matching occurred), `new_cases` (the number of new cases at that point
  in time), `matched_controls` (the number of controls matched to the
  new cases at `time`) and `potential_controls` (the number of potential
  controls at `time`).

- id:

  The value of the supplied `id` argument.

- time:

  A character string used internally to identify the time in other
  datasets.

- info:

  A `list` containing various information on the matching process.

- sizes:

  A `list` containing various information on the overall sample sizes at
  each stage.

- exclusion:

  A `list` containing two `data.tables` which contain the `id`s removed
  from the data at different stages due to `inclusion` as well as the
  reason for removal.

- matchit_objects:

  A `list` of `matchit` objects created at each point in time where
  matching was performed. Only included if `match_method` is set to a
  valid method in
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  and `save_matchit=TRUE`.

- ps_model:

  A [`coxph`](https://rdrr.io/pkg/survival/man/coxph.html) model fit to
  estimate the time-dependent propensity score. Only included when
  `method="psm"` or `method="dsm"`.

- prog_model:

  A [`coxph`](https://rdrr.io/pkg/survival/man/coxph.html) model fit to
  estimate the time-dependent prognostic score. Only included when
  `method="pgm"` or `method="dsm"`.

- call:

  The original function call.

## References

Thomas, Laine E., Siyun Yang, Daniel Wojdyla, and Douglas E. Schaubel
(2020). "Matching with Time-Dependent Treatments: A Review and Look
Forward". In: Statistics in Medicine 39, pp. 2350-2370.

Li, Yunfei Pail, Kathleen J. Propert, and Paul R. Rosenbaum (2001).
"Balanced Risk Set Matching". In: Journal of the American Statistical
Association 96.455, pp. 870-882.

Lu, Bo (2005). "Propensity Score Matching with Time-Dependent
Covariates". In: Biometrics 61.3, pp. 721-728.

## Note

Column names starting with a single point (e.g. names like `".variable"`
or `".id"`) **can not** be used in `data`, because they are used
internally, which could lead to weird errors.

## Author

Robin Denz

## See also

[`fast_exact_matching`](https://robindenz1.github.io/MatchTime/reference/fast_exact_matching.md),
[`stratified_sample`](https://robindenz1.github.io/MatchTime/reference/stratified_sample.md),
[`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html),
[`summary.match_time`](https://robindenz1.github.io/MatchTime/reference/summary.match_time.md),
[`bal.tab.match_time`](https://robindenz1.github.io/MatchTime/reference/bal.tab.match_time.md)

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
heart$event <- as.logical(heart$event)

## time-dependent matching, using "transplant" as treatment and only
## "surgery" as variable to match on, with "event" as outcome
m.obj <- match_time(transplant ~ surgery, data=heart, id="id",
                    match_method="fast_exact", outcomes="event")

# show some balance statistics + the resulting sample sizes
summary(m.obj)

# plot the number of cases / controls / potential controls over time
plot(m.obj)

## allow replacement of controls over time
m.obj <- match_time(transplant ~ surgery, data=heart, id="id",
                    match_method="fast_exact", replace_over_t=TRUE)

## use nearest neighbor matching instead, matching also on continuous "age"
# NOTE: this requires the "MatchIt" package
m.obj <- match_time(transplant ~ surgery + age, data=heart, id="id",
                    match_method="nearest")
summary(m.obj)
}
#> Call:
#> match_time(formula = transplant ~ surgery, data = heart, id = "id", 
#>     outcomes = "event", match_method = "fast_exact")
#> 
#> Summary of Balance for Matched Data at Baseline:
#>         Means Treated Means Control Std. Mean Diff. Var. Ratio  eCDF Mean
#> age        -2.2455396   -3.51152179              NA         NA 0.04136592
#> year        3.4669861    3.23169062              NA         NA 0.04185837
#> surgery     0.2333333    0.04166667              NA         NA 0.19166667
#>          eCDF Max
#> age     0.1062802
#> year    0.1014493
#> surgery 0.1916667
#> 
#> Sample Sizes:
#>           Controls Treated All
#> Matched         54      54 108
#> Unmatched       18      15  33
#> Included       103      69 103
#> Supplied       103      69 103
#> 
#> Points in Time:
#> Matching was performed at 43 unique points in time between 1 and 310.
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Call:
#> match_time(formula = transplant ~ surgery + age, data = heart, 
#>     id = "id", match_method = "nearest")
#> 
#> Summary of Balance for Matched Data at Baseline:
#>           Means Treated Means Control Std. Mean Diff. Var. Ratio  eCDF Mean
#> eventTRUE     0.4745763     0.4883721              NA         NA 0.01379582
#> age          -2.5997517    -2.6059883              NA         NA 0.04105950
#> year          3.5534519     3.3669198              NA         NA 0.03444160
#> surgery       0.1864407     0.1162791              NA         NA 0.07016161
#>             eCDF Max
#> eventTRUE 0.01379582
#> age       0.12105712
#> year      0.07416880
#> surgery   0.07016161
#> 
#> Sample Sizes:
#>           Controls Treated All
#> Matched         51      51 102
#> Unmatched       17      18  35
#> Included       103      69 103
#> Supplied       103      69 103
#> 
#> Points in Time:
#> Matching was performed at 43 unique points in time between 1 and 310.
```
