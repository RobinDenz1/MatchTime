# Display Balance Statistics in a Table for `match_time` objects

This function extends the
[`bal.tab`](https://ngreifer.github.io/cobalt/reference/bal.tab.html)
function from the `cobalt` package to allow the input of a `match_time`
object. This allows users to directly call the
[`bal.tab`](https://ngreifer.github.io/cobalt/reference/bal.tab.html)
function on output of the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function, mimicking the usage of the same function when using the
[`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
function of the `MatchIt` package. The existence of this function also
allows users to directly call the
[`love.plot`](https://ngreifer.github.io/cobalt/reference/love.plot.html)
function on `match_time` objects.

## Usage

``` r
# S3 method for class 'match_time'
bal.tab(x, s.d.denom, remove_unmatched=TRUE,
        n_required=x$info$ratio, ...)
```

## Arguments

- x:

  A `match_time` object created using the
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  function.

- s.d.denom:

  Argument of the same name in
  [`bal.tab`](https://ngreifer.github.io/cobalt/reference/bal.tab.html).

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

  Further arguments passed to
  [`bal.tab`](https://ngreifer.github.io/cobalt/reference/bal.tab.html).

## Author

Robin Denz

## Details

All balance statistics calculated using this method are always
considered "unadjusted". The reason is that it is not possible to make a
distinction between "unadjusted" and "adjusted" balance statistics
directly when using time-dependent matching, because balance is assessed
at "baseline" (e.g. the time at which individuals were first included
into the matching process). This is easy for the matched data. Here we
just use the covariate value at time of entry (`.treat_time`), which is
the whole point of the matching process. Since there is no such baseline
period for the unmatched data, however, there are no such balance
statistics to be calculated.

The only conceivable strategy to get "unadjusted" and "adjusted"
estimates would be to only match on time (using `match_method="none"` in
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md))
and to calculate the balance statistics immediatly afterwards. Then one
could adjust for unbalanced covariates using any regular method such as
weighting, assessing the balance statistics again afterwards. Once
matching on covariates is included in
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md),
this no longer works.

When calling
[`love.plot`](https://ngreifer.github.io/cobalt/reference/love.plot.html),
estimates are thus also always displayed as "unadjusted". Users can
change or entirely remove this labelling through options in the
[`love.plot`](https://ngreifer.github.io/cobalt/reference/love.plot.html)
function.

## Value

Returns a `bal.tab` object.

## See also

[`bal.tab`](https://ngreifer.github.io/cobalt/reference/bal.tab.html),
[`love.plot`](https://ngreifer.github.io/cobalt/reference/love.plot.html)

## Examples

``` r
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
#>  cobalt (Version 4.6.1, Build Date: 2025-08-20)
#> 
#> Attaching package: ‘cobalt’
#> The following object is masked from ‘package:MatchIt’:
#> 
#>     lalonde
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Balance Measures
#>            Type Diff.Un
#> event    Binary  0.4118
#> age     Contin.  0.1677
#> year    Contin.  0.0048
#> surgery  Binary  0.0000
#> 
#> Sample sizes
#>     FALSE TRUE
#> All    51   51
```
