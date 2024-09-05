\name{sample_dt_stratified}
\alias{sample_dt_stratified}

\title{
Fast Stratified Sampling from a \code{data.table}
}
\description{

}
\usage{
sample_dt_stratified(data, n, strata, replace=FALSE,
                     if_lt_n="stop")
}
\arguments{
  \item{data}{
A \code{data.table} that should be sampled from
  }
  \item{n}{
A named numeric vector containing values > 0, specifying the number of rows that should be sampled for each value in \code{strata}.
  }
  \item{strata}{
A single character string specifying the integer, character or factor variable representing the strata that should be sampled from in \code{data}.
  }
  \item{replace}{
Either \code{TRUE} or \code{FALSE} (default). Specifies whether the sampling should be performed with or without replacement.
  }
  \item{if_lt_n_at_t}{
Must be either \code{"stop"}, \code{"warn"} or \code{"nothing"}. Controls whether to throw an error, a warning or silently accept when the number of rows in a stratum specified by \code{strata} is smaller than the number that should be sampled from it and \code{replace=FALSE} is used.
  }
}
\details{

}
\value{

}
\references{

}
\author{
Robin Denz
}

\seealso{
\code{\link{sample_dt_stratified}}
}
\examples{

}
