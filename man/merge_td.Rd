\name{merge_td}
\alias{merge_td}

\title{
Merge Interval based Information Into a Single Start-Stop Dataset
}
\description{
Given multiple \code{data.table}s (or similar objects) containing partial or complete information about time periods or intervals (start-stop data), this function creates a single complete \code{data.table} in the start-stop format containing all of this information. Single or recurrent events may be added. It is essentially a faster and more RAM efficient version of the classic \code{tmerge} function of the \pkg{survival} package with some additional functionality.
}
\usage{
merge_td(dlist, first_time=NULL, last_time=NULL,
         remove_before_first=TRUE, remove_after_last=TRUE,
         center_on_first=FALSE, defaults=NULL,
         event_times=NULL, time_to_first_event=FALSE,
         constant_vars=NULL, id="id", start="start",
         stop="stop", value="value", status="status",
         check_inputs=TRUE)
}
\arguments{
  \item{dlist}{
A named list of two or more \code{data.table} like objects containing information about time periods. Each item of this list should contain information for one variable to be created. The list may also contain any object that can be coerced to be a \code{data.table}, such as \code{data.frame}s or \code{tibble}s. Each \code{data.table} inside \code{dlist} should contain exactly four columns: \code{id} (the individual identifier across all \code{data.table}s), \code{begin} (the beginning of an interval), \code{end} (the end of an interval) and \code{value} (the value of the variable observed during this interval for the respective \code{id}). The names of \code{dlist} are used as variable names. More information is given under "Details".
  }
  \item{first_time}{
An optional scalar value specifying the time at which the observation time starts for all individuals or \code{NULL} (default). If \code{NULL}, only the information given in \code{dlist} is used to construct the start-stop data. If specified, an additional time-period is added from \code{first_time} to the first event time of each \code{id} (if not present already). Note that this argument must be of the same type as the \code{start} and \code{stop} columns.
  }
  \item{last_time}{
Same as \code{first_time}, but for the end of the observation period.
  }
  \item{remove_before_first}{
Either \code{TRUE} or \code{FALSE}. If \code{TRUE} and \code{first_time} is specified, all time intervals before \code{first_time} are removed for each individual. This is done by default if \code{first_time} is specified, but can be turned off by setting this argument to \code{FALSE}.
  }
  \item{remove_after_last}{
Either \code{TRUE} or \code{FALSE}. If \code{TRUE} and \code{last_time} is specified, all time intervals after \code{last_time} are removed for each individual. This is done by default if \code{last_time} is specified, but can be turned off by setting this argument to \code{FALSE}.
  }
  \item{center_on_first}{
Either \code{TRUE} or \code{FALSE}, specifying whether the \code{start} and \code{stop} columns should be centered on the \code{first_time} value. If \code{first_time} is specified and \code{center_on_first=TRUE}, the first time period of each \code{id} will start at 0.
  }
  \item{defaults}{
An optional named list containing default values for some or all variable or \code{NULL} (default). If \code{NULL}, missing information is kept as \code{NA}. This argument may be useful if the objects in \code{dlist} do not contain information for all of the time under observation for all individuals. The value in \code{defaults} will then be imputed for all such missing intervals. See details and examples.
  }
  \item{event_times}{
An optional \code{data.table} containing times at which an event occured for each \code{id} or \code{NULL} (default). If \code{NULL}, no events are added. If specified, the times included in this \code{data.table} are added to the resulting start-stop data in the form of a \code{status} variable. The data will then reflect what is usually needed to fit time-to-event or recurrent event models. See details and examples. The supplied \code{data.table} should include exactly two columns: \code{id} (see argument of the same name) and \code{time} (the time at which the event(s) occured). Multiple events per \code{id} are supported. Note that if \code{event_times} contains times before the first included time-period for an \code{id}, those are not be included in the output. Users should define the \code{first_time} argument accordingly, or provide full information on the required time-periods through \code{dlist}.
  }
  \item{time_to_first_event}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether the observation time after the first event per \code{id} should be removed or not. If \code{TRUE}, only time until the first event per person is kept. Event times should be defined using the \code{event_times} argument. This argument is ignored if \code{event_times=NULL}. Note that if \code{first_time} is not specified and the first event of an \code{id} happens before the first included time period, the \code{id} will be removed entirely from the output.
  }
  \item{constant_vars}{
An optional \code{data.table} containing values of variables that are constant over time for some or all \code{id}s in \code{dlist} or \code{NULL} (default). If specified, the constant variables are simply merged to the final start-stop data. The input to \code{constant_vars} should therefore contain a column named as the \code{id} argument.
  }
  \item{id}{
A single character string specifying the name of the \code{id} column in the objects contained in \code{dlist}. Defaults to \code{".id"}. The column identified by this string may contain any variable type.
  }
  \item{start}{
A single character string specifying the name of the \code{start} column in the objects contained in \code{dlist}. Defaults to \code{"start"}. The column identified by this string may contain and variable type that allows comparisons and that can be merged on (such as int, datetime, and to some degree floats).
  }
  \item{stop}{
A single character string specifying the name of the \code{stop} column in the objects contained in \code{dlist}. Defaults to \code{"stop"}. The column identified by this string may contain and variable type that allows comparisons and that can be merged on (such as int, datetime, and to some degree floats).
  }
  \item{value}{
A single character string specifying the name of the \code{value} column in the objects contained in \code{dlist}. Defaults to \code{"value"}. The column identified by this string may contain and variable type.
  }
  \item{status}{
A single character string specifying how the status variable should be named. Defaults to \code{"status"}. Ignored if \code{event_times=NULL}.
  }
  \item{check_inputs}{
Either \code{TRUE} (default) or \code{FALSE}, specifying whether to perform input checks. May be turned off to save a little computational time.
  }
}
\details{
The start-stop format, also known as counting process or period format corresponds to a \code{data.table} containing multiple rows per \code{id}, where each row corresponds to a period of time in which no variables changed. These intervals are defined by the \code{start} and \code{stop} columns. The \code{start} column gives the time at which the period started, the \code{stop} column denotes the time when the period ended. This type of dataset is required for the \code{\link{match_td}} function and most of the existing time-to-event models with time-varying variables, such as \code{coxph}. Bringing existing data on period information into this format can become tricky and computationally expensive fast. The purpose of this function is to make this transformation as fast and easy as possible.

\strong{\emph{Interval Coding}}:

The intervals supplied to this function via \code{dlist} and the output created is based entirely on overlapping intervals. More specifically, \emph{right-open} intervals \code{[start, stop)} are expected and used throughout it. The \pkg{ivs} package introductory vignette contains a nice explanation of why this is the best way to code intervals. As a consequence, intervals of length 0 (where \code{start==stop}) are not supported and will be removed during the data wrangling process.

\strong{\emph{Specifying \code{dlist}}}:

Each \code{data.table} in \code{dlist} should include information about time-periods of a single variable. For example, suppose that we want to include data for two time-varying variables for individual \code{id=1}. These variables are \code{BMI} and \code{Treatment}. Suppose we have the following information for BMI:

\preformatted{
  d1 <- data.table(id=c(1, 1),
                   start=c(0, 10),
                   stop=c(10, 22),
                   value=c(32.1, 35))
}

In other words, the BMI has a value of 32.1 in the interval \code{[0, 10)} and a value of 35 ín the interval \code{[10, 22)}. Additionally, we know the days on which a treatment was given:

\preformatted{
  d2 <- data.table(id=c(1, 1, 1),
                   start=c(3, 6, 12),
                   stop=c(4, 7, 13),
                   value=c(TRUE, TRUE, TRUE))
}

Here, only the actual days with a treatment are recorded, but it is known that no treatment was given at all other times. We could include this information directly into \code{d2} by adding the intervals inbetween, but there is no need to do that, as we will see soon. The two datasets could be merged into one start-stop dataset using:

\preformatted{
  dlist <- list("BMI"=d1, "Treatment"=d2)
  data <- merge_td(dlist, defaults=list("Treatment"=FALSE))
}

This example is also given below if you want to run it yourself. By specifying the \code{defaults} argument for "Treatment" to be \code{FALSE}, we get the desired start-stop dataset. \code{dlist} may contain any number of \code{data.table}s with full information (such as \code{d1}) or partial information (such as \code{d2}). More examples are given below.

\strong{\emph{Inclusion of Event Times}}:

The type of start-stop data required for most time-to-event models with time-varying variables or recurrent events requires that event times are coded differently than standard intervals. Instead of creating a new interval for an event time, existing intervals should end at the event times, effectively breaking up the existing intervals (see for example Chiou et al. 2023). This can be achieved in this function by supplying the event times separately using the \code{event_times} argument. Multiple events per \code{id} are supported. If only the time until the first event should be kept, users may set \code{time_to_first_event} to \code{TRUE}.

If \code{first_time} is specified and \code{remove_before_first=TRUE}, all events occuring before \code{first_time} will be removed as well. If \code{time_to_first_event=TRUE} is used additionally, all \code{id}s with events before \code{first_time} will be removed entirely. If users want to instead keep only the time until the first event \emph{after} \code{first_time}, they are required to remove all events before that point in time from the \code{event_times} dataset before calling this function.

\strong{\emph{Speed Considerations}}:

Contrary to similar functions such as the \code{tmerge} function from the \pkg{survival} package, this function does not rely on the \code{ivs} package to perform the data transformation. It instead relies only on the \code{data.table} package, which is highly optimized in terms of RAM usage and performance. As such, this function scales a lot better with large inputs (both large amount of \code{id}s and with many intervals). For small datasets there should be no discernible difference between the functions.

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
\code{\link{match_td}}
}
\examples{
library(MatchTD)
library(data.table)
library(fastmatch)

## example from "Details" section
# NOTE: actual data may of course include more than one unique "id"
#       and more than two variables
d1 <- data.table(id=c(1, 1),
                 start=c(0, 10),
                 stop=c(10, 22),
                 value=c(32.1, 35))

d2 <- data.table(id=c(1, 1, 1),
                 start=c(3, 6, 12),
                 stop=c(4, 7, 13),
                 value=c(TRUE, TRUE, TRUE))

dlist <- list("BMI"=d1, "Treatment"=d2)

# setting default for Treatment
data <- merge_td(dlist, defaults=list("Treatment"=FALSE))
print(data)

# setting no such defaults
data <- merge_td(dlist)
print(data)

# with a first_time and last_time argument
data <- merge_td(dlist, first_time=-5, last_time=100)
print(data)

# last_time < actually observed last time
data <- merge_td(dlist, last_time=17)
print(data)

# first_time > actually observed first time
data <- merge_td(dlist, first_time=8)
print(data)

# adding a time-constant variable sex
d3 <- data.table(id=1, sex="female")
data <- merge_td(dlist, constant_vars=d3)
print(data)

# adding event times
d4 <- data.table(id=c(1, 1), time=c(4, 31))
data <- merge_td(dlist, event_times=d4)
print(data)
}
