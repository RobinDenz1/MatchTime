# Time-Dependent Greedy Selection of Controls

This documentation page describes the matching process used when setting
`method="greedy"` in the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function and gives a explanation on which arguments may still be used in
this case. No additional arguments are currently supported when using
this method.

## Details

Due to the nature of this approach, almost none of the arguments of the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function may be used when using `method="greedy"`, because they do not
make sense in the context of greedy selection of controls. More
specifically, only the arguments `formula`, `data`, `id`, `inclusion`,
`start`, `stop`, `method`, `replace_cases` and `verbose` can be used
with this method.

***How it works***:

All other methods implemented in this package use some sort of matching
to select a pre-specified number of controls from the pool of available
data. When using `method="greedy"`, this is not the case. Instead of
performing a selection, every single individual that is eligible to be a
control at the point in time at which "matching" occurs is selected as
control. For example, suppose that at \\t = 10\\ there are 100
individuals in the dataset. Of these individuals, 20 receive the
treatment for the first time at \\t\\ and 10 already have received the
treatment before \\t\\. The remaining 70 individuals are all eligible to
be sampled as controls at this point in time (if they fufill the
`inclusion` criteria) and are thus all used as controls.

This process is repeated at each point in time at which at least one
individual switches from the control to the treated condition. The
selection of controls proceeds as if `replace_at_t=TRUE`, regardless of
the value supplied to the argument. Replacement of controls over time is
neccessary, because otherwise there woule be no controls left after the
first time at which an individual receives treatment.

This method might seem odd at first, but as Gran et al. (2010) have
shown, it does work quite well for some estimands of interest if the
analysis is done in an appropriate manner. These authors propose the
following three step analysis: (1) censoring the controls whenever they
later receive treatment (as implemented in the argument
`censor_at_treat` of the
[`add_outcome`](https://robindenz1.github.io/MatchTime/reference/add_outcome.md)
function), (2) calculating inverse probability of censoring weights for
each "trial" (each `.treat_time`), and (3) using a stratified weighted
Cox model to perform the actual confounder adjustment.

## References

Gran, Jon Michael, Kjetil Røysland, Marcel Wolbers, Vanessa Didelez,
Jonathan A. C. Sterne, Bruno Ledergerber, Hansjakob Furrer, Viktor von
Wyl, and Odd O. Aalen (2010). "A Sequential Cox Approach for Estimating
the Causal Effect of Treatment in the Presence of Time-Dependent
Confounding Applied to Data from the Swiss HIV Cohort Study". In:
Statistics in Medicine 29, pp. 2757-2768.

Karim, Mohammad Ehsanul, John Petkau, Paul Gustafson, Robert W. Platt,
and Helen Tremlett (2018). "Comparison of Statistical Approaches Dealing
with Time-Dependent Confounding in Drug Effectiveness Studies". In:
Statistical Methods in Medical Research 27.6, pp. 1709-1722.

## Author

Robin Denz

## Examples

``` r
library(data.table)
library(MatchTime)

if (requireNamespace("survival")) {

library(survival)

# load some example data from the survival package
data("heart", package="survival")

## time-dependent greedy matching
## there does not have to be anything on the RHS of formula, because no
## actual matching is performed
m.obj <- match_time(transplant ~ 1, data=heart, id="id",
                    method="greedy")
}
```
