% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_frequency.R
\name{get_frequency}
\alias{get_frequency}
\title{Calculate Route Frequency in GTFS Data}
\usage{
get_frequency(gtfs, method = "by.route")
}
\arguments{
\item{gtfs}{A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.}

\item{method}{A character string specifying the calculation method. Options include:
\describe{
  \item{"by.route"}{Calculates the total daily frequency for each route.}
  \item{"by.shape"}{Calculates the total daily frequency for each shape.}
  \item{"by.stop"}{Calculates the total daily frequency for each stop.}
  \item{"detailed"}{Calculates the hourly frequency for each route.}
}}
}
\value{
A data frame containing route frequencies based on the specified method:
  \describe{
    \item{If `method = "by.route"`}{Returns a data frame with columns: `route_id`, `direction_id`, `daily.frequency`, `service_pattern`, and `pattern_frequency`.}
    \item{If `method = "by.shape"`}{Returns a data frame with columns: `shape_id`, `direction_id`, `daily.frequency`, `service_pattern`, and `pattern_frequency`.}
    \item{If `method = "by.stop"`}{Returns a data frame with columns: `stop_id`, `direction_id`, `daily.frequency`, `service_pattern`, and `pattern_frequency`.}
    \item{If `method = "detailed"`}{Returns a data frame with columns: `route_id`, `direction_id`, `hour`, `frequency`, `service_pattern`, and `pattern_frequency`.}
  }
}
\description{
The `get_frequency` function calculates route frequency within a `wizardgtfs` object using different methods. Depending on the selected `method`, it can provide daily frequencies by route, shape, stop or detailed hourly frequencies.
}
\details{
This function calls specific sub-functions based on the selected method:

- "by.route": Calculates the total daily frequency for each route.

- "by.shape": Calculates the total daily frequency for each shape.

- "by.stop": Calculates the total daily frequency for each stop.

- "detailed": Provides an hourly breakdown of frequency, showing the number of departures per hour for each route and direction.

If an invalid `method` is specified, the function defaults to `"by.route"` and provides a warning.
}
\examples{
# Calculate daily route frequency
frequency_by_route <- get_frequency(gtfs = for_rail_gtfs, method = "by.route")

# Calculate daily shape frequency
frequency_by_shape <- get_frequency(gtfs = for_rail_gtfs, method = "by.shape")

# Calculate daily stop frequency
frequency_by_stop <- get_frequency(gtfs = for_rail_gtfs, method = "by.stop")

# Calculate detailed hourly frequency
detailed_frequency <- get_frequency(gtfs = for_rail_gtfs, method = "detailed")

}
\seealso{
[GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
}
