
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MatchTime

<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://www.r-pkg.org/badges/version/MatchTime?color=green)](https://cran.r-project.org/package=MatchTime)
[![](http://cranlogs.r-pkg.org/badges/grand-total/MatchTime?color=blue)](https://cran.r-project.org/package=MatchTime)
[![R-CMD-check](https://github.com/RobinDenz1/MatchTime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RobinDenz1/MatchTime/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/RobinDenz1/MatchTime/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RobinDenz1/MatchTime?branch=main)
<!-- badges: end -->

`MatchTime` is an R-Package which can be used to perform different sorts
of time-dependent matching, which is also known as *sequential trial
emulation*. In this type of matching, the treatment, outcome, and
confounders are allowed to change over time. At each point in time where
individuals switch from “untreated” to “treated”, controls which at this
point in time are still “untreated” are matched to these individuals
using standard matching methods. The point in time that this matching
occurred is considered to be the “point of randomization” or “time-zero”
for both cases and their matched controls. The result is a dataset that
can be analyzed using standard time-to-event methods, such as simple Cox
proportional-hazards regression models, even when complex forms of
time-dependent confounding and treatment-confounder feedback are present
in the data.

The package is designed to be as similar as possible to the excellent
`MatchIt` package, and implements direct support for it when matching at
each separate point in time. It supports treatment and covariate changes
in discrete and continuous time by requiring the user to input
*start-stop* data. Because generating and dealing with start-stop data
can be difficult, it also implements various functions to make this
easier for the user. Large parts of `MatchTime` were originally
developed for the purpose of analyzing data from a large German
health-insurance provider. As such, the functions are designed to be
very fast and RAM efficient, relying almost entirely on the `data.table`
package internally. Input datasets with ~ 20 million rows can be used in
any function on a regular computer without any issues.

## Installation

The developmental version may be installed from github using the
`remotes` R-Package:

``` r
library(remotes)

remotes::install_github("RobinDenz1/MatchTime")
```

## Bug Reports and Feature Requests

If you encounter any bugs or have any specific feature requests, please
file an [Issue](https://github.com/RobinDenz1/MatchTime/issues).

## Example

A small example for time-dependent matching on continuous start-stop
data is given below, using the `heart` dataset from the `survival`
package:

``` r
library(data.table)
library(MatchTime)
library(MatchIt)
library(survival)

data("heart")

set.seed(1234)

m_obj <- match_time(transplant ~ age + surgery, data=heart, id="id",
                  match_method="nearest")
```

As in `MatchIt`, balance statistics (at “baseline”) can be calculated
using:

``` r
summary(m_obj)
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

The actual number of cases and matched controls over time, as well as
the number of potential controls at these points in time can
additionally be visualized using:

``` r
plot(m_obj)
```

<img src="man/figures/README-example_plot-1.png" width="100%" />

In this particular example, after around t = 45, there are no potential
controls anymore, because no replacement was used and there are more
cases than controls.

## Citation

Use `citation("MatchTime")` to get the relevant citation information.

## License

© 2024 Robin Denz

The contents of this repository are distributed under the GNU General
Public License. You can find the full text of this License in this
github repository. Alternatively, see <http://www.gnu.org/licenses/>.
