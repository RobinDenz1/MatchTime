# View a balance summary of a `match_time` object

Computes and prints balance statistics for `match_time` objects at
baseline, similar to the `summary` method of
[`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
objects. Similar functionality is implemented in
[`bal.tab.match_time`](https://robindenz1.github.io/MatchTime/reference/bal.tab.match_time.md).

## Usage

``` r
# S3 method for class 'match_time'
summary(object, standardize=TRUE,
        remove_unmatched=TRUE,
        n_required=object$info$ratio, ...)
```

## Arguments

- object:

  A `match_time` object created using the
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  function.

- standardize:

  Either `TRUE` or `FALSE`; whether to compute standardized (`TRUE`) or
  unstandardized (`FALSE`) statistics. The standardized statistics are
  the standardized mean difference and the mean and maximum of the
  difference in the (weighted) empirical cumulative distribution
  functions (ECDFs). The unstandardized statistics are the raw mean
  difference and the mean and maximum of the quantile-quantile (QQ)
  difference. Variance ratios are produced either way. See Details
  below. Default is `TRUE`.

- remove_unmatched:

  Whether to remove unmatched individuals before calculating the balance
  statistics. Internally, the
  [`get_match_data`](https://robindenz1.github.io/MatchTime/reference/get_match_data.md)
  function is called for this, please see the documentation of that
  function for more information. This has no influence on the computed
  sample sizes.

- n_required:

  Same as the argument of the same name in
  [`get_match_data`](https://robindenz1.github.io/MatchTime/reference/get_match_data.md).

- ...:

  Currently not used.

## Author

Robin Denz

## Details

***Balance Statistics***:

The summary method for `match_time` objects is made to resemble the
summary method of `matchit` objects. In fact it directly uses code from
the `MatchIt` package to compute the balance statistics. For more
details on how the balance statistics are calculated and what they mean,
please consult the associated documentation page of
[`summary.matchit`](https://kosukeimai.github.io/MatchIt/reference/summary.matchit.html).

Note that contrary to standard matching, the matching performed by
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
is time-dependent. Because of this, presenting balance statistics for
unmatched data is impossible. This function therefore only prints the
balance statistics at baseline (e.g. at the time on which the cases were
included in the matching process).

***Sample Sizes***:

The displayed sample sizes may be confusing without further explanation.
Because the matching process is based on dynamically changing risk sets,
where each individual may be either "control" or "treated" at different
points in time, it is not easy to summarise the sample sizes in terms of
the binary distinction "Controls" and "Treated".

Displayed under "Matched" are the numbers of individuals who form fully
matched pairs in the output (e.g. each case received `ratio` matched
controls). The "Unmatched Controls" below refer to potential controls
that were never used as such in the matching process, while "Unmatched
Treated" refers to cases that did not receive `ratio` matched controls.
Note that the individuals mentioned as "Unmatched Treated" may still
have been used as controls during the matching process. The "All" column
is simply the sum of both "Controls" and "Treated" for "Matched" and
"Unmatched" individuals.

"Included Controls" refers to all individuals which fufilled the
inclusion criteria at some point in time before becoming "treated",
while the "Included Treated" refers to all individuals who fufilled the
inclusion criteria at some point in time during which they received the
treatment. The "Supplied Controls" similarly refers to the number of
individuals who, before applying the inclusion criteria, were present as
"untreated" at some point in time in the `data` supplied to
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md).
The "Supplied Treated" are the number of individuals who, before
applying the inclusion criteria, were present as "treated" at some point
in time in `data`. The "All" column is usually not the sum of "Controls"
and "Treated" here, because unless the treatment occured exactly at t =
0, the cases may be used as controls as well.

These are total numbers, calculated irrespective of the replacement
options used. For more detailed information on how the number of cases,
number of matched controls and number of potential controls evolved over
time during the matching process, the
[`plot.match_time`](https://robindenz1.github.io/MatchTime/reference/plot.match_time.md)
function may be used. Alternatively, users may directly inspect the
`trace` object included in the output object of
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md).

## Value

Silently returns a `list` containing the balance statistics and sample
sizes.

## See also

[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md),
[`summary.matchit`](https://kosukeimai.github.io/MatchIt/reference/summary.matchit.html),
[`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
[`bal.tab`](https://ngreifer.github.io/cobalt/reference/bal.tab.html),
[`love.plot`](https://ngreifer.github.io/cobalt/reference/love.plot.html)

## Examples

``` r
library(data.table)
library(MatchTime)

# only execute if packages are available
if (requireNamespace("MatchIt") & requireNamespace("survival")) {

library(MatchIt)
library(survival)

# set random seed to make the output replicably
set.seed(1234)

# load "heart" data from survival package
data("heart")

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
out <- match_time(transplant ~ age + surgery, data=heart, id="id",
                  match_method="nearest")
summary(out)
}
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Call:
#> match_time(formula = transplant ~ age + surgery, data = heart, 
#>     id = "id", match_method = "nearest")
#> 
#> Summary of Balance for Matched Data at Baseline:
#>         Means Treated Means Control Std. Mean Diff. Var. Ratio  eCDF Mean
#> event       0.4745763     0.4883721              NA         NA 0.01379582
#> age        -2.5997517    -2.6059883              NA         NA 0.04105950
#> year        3.5534519     3.3669198              NA         NA 0.03444160
#> surgery     0.1864407     0.1162791              NA         NA 0.07016161
#>           eCDF Max
#> event   0.01379582
#> age     0.12105712
#> year    0.07416880
#> surgery 0.07016161
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
