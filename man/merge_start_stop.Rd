\name{merge_start_stop}
\alias{merge_start_stop}

\title{
Merge two or more datasets in the start-stop format
}
\description{
Given multiple \code{data.table}s (or similar objects) containing partial or complete information about time periods or intervals (start-stop data), this function creates a single complete \code{data.table} in the start-stop format containing all of this information. Single or recurrent events may be added. It is essentially a faster and more RAM efficient version of the classic \code{tmerge} function of the \pkg{survival} package with some additional functionality. It is not a generic function or \code{S3} method, because the input are \code{data.table}s.
}
\usage{
merge_start_stop(x, y, ..., dlist, by, start="start",
                 stop="stop", all=FALSE, all.x=all,
                 all.y=all, first_time=NULL, last_time=NULL,
                 remove_before_first=TRUE,
                 remove_after_last=TRUE,
                 center_on_first=FALSE, units="auto",
                 defaults=NULL, event_times=NULL,
                 time_to_first_event=FALSE,
                 status="status", constant_vars=NULL)
}
\arguments{
  \item{x, y, ...}{
\code{data.table} like objects (\code{tibble}, \code{data.frame}, ...) containing information about time periods. Those should contain information for one or more variables to be created. Each \code{data.table} should contain four or more columns: \code{by} (the individual identifier across all \code{data.table}s), \code{start} (the beginning of an interval), \code{stop} (the end of an interval) and at least one variable containing some values observed during this interval for the respective individual identified using \code{by}. More information is given under "Details". Users should define either \code{x} and \code{y} (and potentially parse more objects through \code{...}) or define the \code{dlist} argument instead.
  }
  \item{dlist}{
A list of \code{data.table} like objects. This argument may be used instead of parsing the \code{data.table}s directly through \code{x}, \code{y} and the three-dot syntax. If \code{dlist} is specified, \code{x}, \code{y} and all datasets parsed through \code{...} are ignored.
  }
  \item{by}{
A single character string specifying the name of the column which uniquely identifies individuals in the supplied datasets. The column identified by this string may contain any variable type. Contrary to the \code{by} argument of standard \code{merge} functions, this argument does not support multiple values. Since the goal is to merge on time-intervals, it is also necessary to additionaly define which columns identify them using the \code{start} and \code{stop} arguments.
  }
  \item{start}{
A single character string specifying the name of the \code{start} column in the supplied datasets. Defaults to \code{"start"}. The column identified by this string may contain and variable type that allows comparisons and that can be merged on (such as int, datetime, and floats).
  }
  \item{stop}{
A single character string specifying the name of the \code{stop} column in the supplied datasets. Defaults to \code{"stop"}. The column identified by this string may contain and variable type that allows comparisons and that can be merged on (such as int, datetime, and floats).
  }
  \item{all}{
Either \code{TRUE} or \code{FALSE} (default). \code{all=TRUE} is shorthand to save setting both \code{all.x=TRUE} and \code{all.y=TRUE}.
  }
  \item{all.x}{
Either \code{TRUE} or \code{FALSE}. If \code{TRUE}, cases from \code{x} (identified using the \code{by} column) which have no matching cases in \code{y} or any other dataset supplied afterwards through \code{...} are included in the output (if only \code{dlist} is specified, the first entry in \code{dlist} will be considered "x" and all other entries as "y"). These cases will have \code{NA}s in the columns that are usually filled with values from the other datasets. The default is \code{FALSE} so that only cases with data from both \code{x} and \code{y} (and potentially the other datasets) are included in the ouptut. This is analogous to the standard \code{merge} functions, with the only difference being that all datasets except \code{x} are counted as "y" datasets. These arguments only apply to cases identified using the \code{by} column, not to the time-intervals defined by \code{start} and \code{stop}.
  }
  \item{all.y}{
Either \code{TRUE} or \code{FALSE}, analogous to \code{all.x} above.
  }
  \item{first_time}{
An optional scalar value specifying the time at which the observation time starts for all individuals or \code{NULL} (default). If \code{NULL}, only the information given in the supplied data is used to construct the start-stop data. If specified, an additional time-period is added from \code{first_time} to the first event time of each case (if not present already). Note that this argument must be of the same type as the \code{start} and \code{stop} columns.
  }
  \item{last_time}{
Same as \code{first_time}, but for the end of the observation period.
  }
  \item{remove_before_first}{
Either \code{TRUE} (default) or \code{FALSE}. If \code{TRUE} and \code{first_time} is specified, all time intervals before \code{first_time} are removed for each individual. This is done by default if \code{first_time} is specified, but can be turned off by setting this argument to \code{FALSE}.
  }
  \item{remove_after_last}{
Either \code{TRUE} (default) or \code{FALSE}. If \code{TRUE} and \code{last_time} is specified, all time intervals after \code{last_time} are removed for each individual. This is done by default if \code{last_time} is specified, but can be turned off by setting this argument to \code{FALSE}.
  }
  \item{center_on_first}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether the \code{start} and \code{stop} columns should be centered on the \code{first_time} value. If \code{first_time} is specified and \code{center_on_first=TRUE}, the first time period of each case will start at 0 and all periods afterwards will correspond to time elapsed since then.
  }
  \item{units}{
Corresponds to the \code{units} argument of the \code{\link{difftime}} function. Only used when \code{start} and \code{stop} are of class \code{Date} (or something similar) and \code{center_on_first} is set to \code{TRUE}. Ignored otherwise.
  }
  \item{defaults}{
An optional named list containing default values for some or all variable or \code{NULL} (default). If \code{NULL}, missing information is kept as \code{NA}. This argument may be useful if the supplied datasets do not contain information for all of the time under observation for all individuals. The value in \code{defaults} will then be imputed for all such missing intervals. See details and examples.
  }
  \item{event_times}{
An optional \code{data.table} containing times at which an event occured for each case or \code{NULL} (default). If \code{NULL}, no events are added. If specified, the times included in this \code{data.table} are added to the resulting start-stop data in the form of a \code{status} variable. The data will then reflect what is usually needed to fit time-to-event or recurrent event models. See details and examples. The supplied \code{data.table} should include exactly two columns: \code{by} (see argument of the same name) and \code{time} (the time at which the event(s) occured). Multiple events per case are supported. Note that if \code{event_times} contains times before the first included time-period for a case, those are not be included in the output. Users should define the \code{first_time} argument accordingly, or provide full information on the required time-periods through the supplied datasets.
  }
  \item{time_to_first_event}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether the observation time after the first event per case should be removed or not. If \code{TRUE}, only time until the first event per person is kept. Event times should be defined using the \code{event_times} argument. This argument is ignored if \code{event_times} is \code{NULL}. Note that if \code{first_time} is not specified and the first event of a case happens before the first included time period, the case will be removed entirely from the output.
  }
  \item{status}{
A single character string specifying how the status variable should be named. Defaults to \code{"status"}. Ignored if \code{event_times=NULL}.
  }
  \item{constant_vars}{
An optional \code{data.table} containing values of variables that are constant over time for some or all cases in the supplied data or \code{NULL} (default). If specified, the constant variables are simply merged to the final start-stop data. The input to \code{constant_vars} should therefore contain a column named as the \code{by} argument.
  }
}
\details{
The start-stop format, also known as counting process or period format corresponds to a \code{data.table} containing one or multiple rows per \code{by}, where each row corresponds to a period of time in which no variables changed. These intervals are defined by the \code{start} and \code{stop} columns. The \code{start} column gives the time at which the period started, the \code{stop} column denotes the time when the period ended. This type of dataset is required for the \code{\link{match_time}} function and most of the existing time-to-event models with time-varying variables, such as \code{coxph}. Bringing existing data on period information into this format can become tricky and computationally expensive fast. The purpose of this function is to make this transformation as fast and easy as possible.

\strong{\emph{Interval Coding for Time-Varying Variables}}:

The intervals supplied to this function via \code{x, y, ...} or \code{dlist} and the output created is based entirely on overlapping intervals. More specifically, \emph{right-open} intervals \code{[start, stop)} are expected and used throughout it, which is the usual data format expected for time-to-event modelling and corresponds to the interval format of the \code{tmerge} function of the \pkg{survival} package. As a consequence, intervals of length 0 (where \code{start==stop}) are not supported and will result in an error message.

Note that this only applies to the time-varying variables themselves. Events added using \code{event_times} are coded differently, as described further below.

\strong{\emph{Specifying the input}}:

Each input \code{data.table} should include information about time-periods of one or more variables. For example, suppose that we want to include data for two time-varying variables for individual \code{id=1}. These variables are \code{BMI} and \code{Treatment} and have been measured in different time intervals. Suppose we have the following information for BMI:

\preformatted{
  d1 <- data.table(id=c(1, 1),
                   start=c(0, 10),
                   stop=c(10, 22),
                   BMI=c(32.1, 35))
}

In other words, the BMI has a value of 32.1 in the interval \code{[0, 10)} and a value of 35 ín the interval \code{[10, 22)}. Additionally, we know the days on which a treatment was given:

\preformatted{
  d2 <- data.table(id=c(1, 1, 1),
                   start=c(3, 6, 12),
                   stop=c(4, 7, 13),
                   Treatment=c(TRUE, TRUE, TRUE))
}

Here, only the actual days with a treatment are recorded, but it is known that no treatment was given at all other times. We could include this information directly into \code{d2} by adding the intervals inbetween, but there is no need to do that, as we will see soon. The two datasets could be merged into one start-stop dataset using:

\preformatted{
  data <- merge_start_stop(dl, d2, by="id", defaults=list("Treatment"=FALSE))
}

This example is also given below if you want to run it yourself. By specifying the \code{defaults} argument for "Treatment" to be \code{FALSE}, we get the desired start-stop dataset. By using \code{...} or the \code{dlist} argument, any number of \code{data.table}s with full information (such as \code{d1}) or partial information (such as \code{d2}) may be merged. More examples are given below.

\strong{\emph{Inclusion of Event Times}}:

The type of start-stop data required for most time-to-event models with time-varying variables or recurrent events requires that event times are coded differently than standard intervals. Instead of creating a new interval for an event time, existing intervals should end at the event times, effectively breaking up the existing intervals (see for example Chiou et al. 2023). This can be achieved in this function by supplying the event times separately using the \code{event_times} argument. Multiple events per case are supported. If only the time until the first event should be kept, users may set \code{time_to_first_event} to \code{TRUE}.

If \code{first_time} is specified and \code{remove_before_first=TRUE}, all events occuring before \code{first_time} will be removed as well. If \code{time_to_first_event=TRUE} is used additionally, all cases with events before \code{first_time} will be removed entirely. If users want to instead keep only the time until the first event \emph{after} \code{first_time}, they are required to remove all events before that point in time from the \code{event_times} dataset before calling this function.

\strong{\emph{Speed Considerations}}:

Contrary to similar functions such as the \code{tmerge} function from the \pkg{survival} package, this function does not rely on the \code{ivs} package to perform the data transformation. It instead relies only on the \code{data.table} package, which is highly optimized in terms of RAM usage and performance. As such, this function scales a lot better with large inputs (both large amount of cases and with many intervals). For small datasets there should be no discernible difference between the functions.

Note that supplying all datasets that should be merged at the same time is faster than calling this function multiple times with one \code{x} and \code{y} each, although it might be necessary to do the latter in certain situations

}
\value{
Returns a single \code{data.table} containing all supplied information about the intervals in the start-stop format.
}
\references{
Sy Han Chiou, Gongjun Xu, Jun Yan, and Chiung-Yu Huang (2023). "Regression Modeling for Recurrent Events Possibly with an Informative Terminal Event Using R Package reReg". In: Journal of Statistical Software. 105.5, pp. 1-34.
}
\author{
Robin Denz
}
\seealso{
\code{\link{match_time}}
}
\examples{
library(MatchTime)
library(data.table)
library(fastmatch)

## example from "Details" section
# NOTE: actual data may of course include more than one unique "id"
#       and more than two variables
d1 <- data.table(id=c(1, 1),
                 start=c(0, 10),
                 stop=c(10, 22),
                 BMI=c(32.1, 35))

d2 <- data.table(id=c(1, 1, 1),
                 start=c(3, 6, 12),
                 stop=c(4, 7, 13),
                 Treatment=c(TRUE, TRUE, TRUE))

# setting default for Treatment
data <- merge_start_stop(d1, d2, by="id", defaults=list("Treatment"=FALSE))
print(data)

# setting no such defaults
data <- merge_start_stop(d1, d2, by="id")
print(data)

# with a first_time and last_time argument
data <- merge_start_stop(d1, d2, by="id", first_time=-5, last_time=100)
print(data)

# last_time < actually observed last time
data <- merge_start_stop(d1, d2, by="id", last_time=17)
print(data)

# first_time > actually observed first time
data <- merge_start_stop(d1, d2, by="id", first_time=8)
print(data)

# adding a time-constant variable sex
d3 <- data.table(id=1, sex="female")
data <- merge_start_stop(d1, d2, by="id", constant_vars=d3)
print(data)

# adding event times
d4 <- data.table(id=c(1, 1), time=c(4, 31))
data <- merge_start_stop(d1, d2, by="id", event_times=d4)
print(data)
}
