# Construct a matched dataset from a `match_time` object

After performing time-dependent matching using the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function, users usually want to extract the resulting data. If the
resulting data should not contain cases that did not receive the
specified number of matches, this function can be called to remove them
before data extraction.

## Usage

``` r
get_match_data(object, remove_unmatched=TRUE, n_required=object$info$ratio)
```

## Arguments

- object:

  A `match_time` object created using the
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  function.

- remove_unmatched:

  Either `TRUE` (default) or `FALSE`, specifying whether to remove pairs
  with no or not enough matches. The number of matches that each case
  should have received during matching can be set using the `n_required`
  argument.

- n_required:

  A single positive integer, specifying the number of matched controls a
  pair needs to have to stay in the data. For example, if 1:1 matching
  was used and the user only wants to keep pairs where cases actually
  did receive one control, it should be set to 1. If `ratio=3` was used
  and users want to keep all pairs with 2 or more matched controls, it
  should be set to 2 etc. By default, the number of matches specified in
  the `ratio` argument of the original
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  call is used. A warning is produced if users try to use this method
  when `match_method` in the original
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  call was set to a method in
  [`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
  that does not use pair matching.

## Value

Returns a single `data.table`. This `data.table` will include the same
columns as the `$data` object in the output of
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md),
with the exception of the `".fully_matched"` column. Note that if no
`".id_pair"` variable was created in
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md),
it will also not be present in the output here.

## Details

The resulting data may contain multiple rows per actual individual,
depending on the arguments used in the original
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
call. This might be because individuals were included as controls *and*
cases at different points in time, or because they were included as
controls at multiple points in time (`replace_over_t=TRUE`), or because
they were included as controls multiple times at the same point in time
(`replace_at_t=TRUE`), or due to a mix of these reasons.

Contrary to the MatchIt package, there is no option to change this.
"Replaced" individuals will always appear as often as they were selected
during the matching process. The output will therefore also always
contain an `".id_pair"` column, indicating in which "pair" the cases and
controls are. The only instance where this column is not included is
when `match_method` is set to some method in
[`matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html)
that does not create pairs internally.

## Author

Robin Denz

## See also

[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)

## Examples

``` r
library(data.table)
library(MatchTime)

# only execute if packages are available
if (requireNamespace("survival") & requireNamespace("MatchIt")) {

library(survival)
library(MatchIt)

# set random seed to make the output replicably
set.seed(1234)

# load "heart" data from survival package
data("heart")

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
m_obj <- match_time(transplant ~ age + surgery, data=heart, id="id",
                    match_method="nearest")

# get data without unmatched individuals
data <- get_match_data(m_obj)
head(data)
}
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Key: <id>
#>       id .id_new .id_pair .treat .treat_time .next_treat_time .weights event
#>    <num>   <int>   <char> <lgcl>       <num>            <num>    <num> <num>
#> 1:     1      72       36  FALSE          27               NA        1     1
#> 2:     3       1        1   TRUE           1               NA        1     1
#> 3:     4      10        5  FALSE           2               36        1     0
#> 4:     4      89       45   TRUE          36               NA        1     1
#> 5:     7      86       43  FALSE          32               51        1     0
#> 6:     8      16        8  FALSE           3               NA        1     1
#>           age      year surgery
#>         <num>     <num>   <num>
#> 1: -17.155373 0.1232033       0
#> 2:   6.297057 0.2655715       0
#> 3:  -7.737166 0.4900753       0
#> 4:  -7.737166 0.4900753       0
#> 5:   2.869268 0.7802875       0
#> 6:  -2.650240 0.8350445       0
```
