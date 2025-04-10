% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_corridor.R
\name{get_corridor}
\alias{get_corridor}
\title{Identify and Extract Transit Corridors}
\usage{
get_corridor(gtfs, i = 0.01, min.length = 1500)
}
\arguments{
\item{gtfs}{A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.}

\item{i}{A numeric value representing the percentile threshold for selecting high-density segments. Defaults to `0.01` (top 1\% of segments by trip frequency).}

\item{min.length}{A numeric value specifying the minimum corridor length (in meters) to retain. Defaults to `1500`.}
}
\value{
An `sf` object containing the following columns:
\describe{
  \item{corridor}{A unique identifier for each corridor, prefixed with "corridor-".}
  \item{stops}{A list of stop IDs included in each corridor.}
  \item{trip_id}{A list of trip IDs included in each corridor.}
  \item{length}{The total length of the corridor, in meters.}
  \item{geometry}{The spatial representation of the corridor as an `sf` linestring object.}
}
}
\description{
The `get_corridor` function identifies and extracts high-density transit corridors based on trip frequency between stops. It groups segments into connected corridors, and filters them based on a minimum length criterion.
}
\details{
The function performs the following steps:
\enumerate{
  \item Filters and orders `stop_times` data to identify consecutive stops (`stop_from` and `stop_to`) for each trip.
  \item Counts the number of trips between each stop pair and selects the top `i` percentile of segments by trip frequency.
  \item Groups spatially connected segments into corridors using graph theory and adjacency matrices.
  \item Filters corridors by the minimum length (`min.length`).
  \item Returns the resulting corridors with their metadata and geometry.
  }
}
\note{
The function uses `sf` and `igraph` for spatial and graph-based computations. Ensure the `gtfs` object includes `stop_times` table.
}
\examples{
corridors <- get_corridor(for_bus_gtfs, i = 0.02, min.length = 2000)

}
\seealso{
[GTFSwizard::as_wizardgtfs()]
}
