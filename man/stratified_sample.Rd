\name{stratified_sample}
\alias{stratified_sample}

\title{
Fast Stratified Sampling from a \code{data.table} like object
}
\description{
This function may be used to draw a stratified random sample (with or without replacement) of rows from a \code{data.table} or similar object. It additionally includes options on how to handle cases in which the number to be drawn is less than the number of actual rows when using \code{replace=FALSE}, which makes it useful in exact matching.
}
\usage{
stratified_sample(data, n, strata, replace=FALSE,
                  if_lt_n="stop")
}
\arguments{
  \item{data}{
A \code{data.table} like object that should be sampled from. May also be any object that can be coerced to be a \code{data.table}, such as a \code{data.frame} or a \code{tibble}.
  }
  \item{n}{
A named numeric vector containing values > 0, specifying the number of rows that should be sampled for each value in \code{strata}.
  }
  \item{strata}{
A single character string specifying the integer, character or factor variable representing the strata that should be sampled from in \code{data}. Only supports a single variable. If data should be sampled from strata of multiple variables, the user needs to first create a single variable from these multiple variables as shown in the examples.
  }
  \item{replace}{
Either \code{TRUE} or \code{FALSE} (default). Specifies whether the sampling should be performed with or without replacement.
  }
  \item{if_lt_n}{
Must be either \code{"stop"}, \code{"warn"} or \code{"nothing"}. Controls whether to throw an error, a warning or silently accept when the number of rows in a stratum specified by \code{strata} is smaller than the number that should be sampled from it and \code{replace=FALSE} is used.
  }
}
\details{
Internally splits the \code{data.table} into parts as defined by the \code{strata} variable and then uses the \code{\link{sample.int}} function to sample rows from these strata. By using only \code{data.table} functions and \code{\link{sample.int}} instead of \code{sample}, this function is very efficient even for very large \code{data.tables} and many possible strata.
}
\value{
Returns a single \code{data.table} containing the sampled rows.
}
\references{
Lohr, Sharon L. (2010). Sampling: Design and Analysis. Bd. 2. Boston: Cengage Learning.
}
\author{
Robin Denz
}
\seealso{
\code{\link{fast_exact_matching}}
}
\examples{
library(MatchTD)
library(data.table)
library(fastmatch)

set.seed(344)

data <- data.table(A=stats::rnorm(n=100),
                   B=sample(c("A", "B", "C"), size=100, replace=TRUE),
                   strat=sample(c("1", "2", "3")))

n <- c(10, 25, 31)
names(n) <- c("2", "1", "3")

# sample (without replacement):
# - 25 rows from strat=="1"
# - 10 rows from strat=="2"
# - 31 rows from strat=="3"
samp <- stratified_sample(data, n=n, strata="strat")

# take the same stratified sample with replacement
samp <- stratified_sample(data, n=n, strata="strat", replace=TRUE)
}
