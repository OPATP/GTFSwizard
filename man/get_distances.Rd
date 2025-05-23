% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_distances.R
\name{get_distances}
\alias{get_distances}
\title{Calculate Distances in GTFS Data}
\usage{
get_distances(gtfs, method = "by.route", trips = "all")
}
\arguments{
\item{gtfs}{A GTFS object, ideally of class `wizardgtfs`. If it is not of this class, it will be converted.}

\item{method}{A character string indicating the calculation method. Choices are:
\describe{
  \item{"by.route"}{Calculates average distances for each route.}
  \item{"by.trip"}{Calculates distances for each trip, associating each trip ID with its total distance.}
  \item{"detailed"}{Calculates detailed distances between each consecutive stop for all trips. This is the most computationally intensive option and may take several minutes to complete.}
}}

\item{trips}{A character vector of trip IDs to consider. When set to `all`, includes all trips.}
}
\value{
A data frame with calculated distances based on the specified method:
  \describe{
    \item{If `method = "by.route"`}{Returns a summary with columns: `route_id`, `trips`, `average.distance`, `service_pattern`, and `pattern_frequency`.}
    \item{If `method = "by.trip"`}{Returns a data frame with columns: `route_id`, `trip_id`, `distance`, `service_pattern`, and `pattern_frequency`.}
    \item{If `method = "detailed"`}{Returns a data frame with columns: `shape_id`, `from_stop_id`, `to_stop_id`, and `distance`.}
  }
}
\description{
The `get_distances` function calculates distances within a `wizardgtfs` object based on various methods.
Depending on the `method` chosen, it can calculate average route distances, trip-specific distances, or detailed distances between stops.
}
\details{
The function calls specific sub-functions based on the selected method:

- "by.route": Calculates average distances per route.

- "by.trip": Calculate distances per trip.

- "detailed": Calculates detailed stop-to-stop distances within each route. Note that this method may be slow for large datasets.

If an invalid `method` is provided, the function defaults to `"by.route"` and issues a warning.
}
\examples{
# Calculate average route distances
distances_by_route <- get_distances(gtfs = for_rail_gtfs, method = "by.route", trips = 'all')

# Calculate distances by trip
distances_by_trip <- get_distances(gtfs = for_rail_gtfs, method = "by.trip", trips = 'all')

\donttest{
# Calculate detailed distances between stops
detailed_distances <- get_distances(gtfs = for_rail_gtfs, method = "detailed", trips = 'all')
}

}
\seealso{
[GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
}
