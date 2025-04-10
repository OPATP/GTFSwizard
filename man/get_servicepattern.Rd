% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_servicepattern.R
\name{get_servicepattern}
\alias{get_servicepattern}
\title{Identify Service Patterns in GTFS Data}
\usage{
get_servicepattern(gtfs)
}
\arguments{
\item{gtfs}{A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.}
}
\value{
A data frame containing unique service patterns with the following columns:
  \describe{
    \item{`service_id`}{Unique identifier(s) for each service.}
    \item{`service_pattern`}{An identifier for each distinct service pattern based on operational dates, in the format "servicepattern-N".}
    \item{`pattern_frequency`}{The frequency of each service pattern, indicating the number of dates associated with that pattern.}
  }
}
\description{
The `get_servicepattern` function identifies and organizes unique service patterns within a `wizardgtfs` object. It groups services by common dates of operation and assigns each a frequency-based pattern identifier.
}
\details{
The function first checks if the input `gtfs` object is of class `wizardgtfs`. If not, it converts it using `as_wizardgtfs()`. It then groups services by common dates of operation, assigns a frequency to each unique pattern, and organizes these into service pattern identifiers, ordered by their frequency.
}
\examples{
# Generate service patterns for a GTFS object
service_patterns <- get_servicepattern(gtfs = for_rail_gtfs)

}
\seealso{
[GTFSwizard::as_wizardgtfs()]
}
