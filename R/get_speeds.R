#' Calculate Scheduled Speeds
#'
#' Combines distance in meters and duration in seconds to calculate kilometers
#' per hour.
#'
#' @param gtfs A GTFS object.
#' @param method One of `"by.route"`, `"by.trip"`, or `"detailed"`.
#' @param trips Character trip IDs or `"all"`.
#'
#' @return A tibble with `average.speed` for route and trip methods, or
#'   segment-level `speed` for the detailed method.
#'
#' @details
#' Detailed distances are straight-line geodesic distances between stops.
#' Route and trip distances follow `shapes.txt`, or inferred straight-line
#' shapes if the feed has none.
#'
#' @examples
#' get_speeds(for_rail_gtfs, "by.route")
#' get_speeds(for_rail_gtfs, "by.trip")
#'
#' @seealso [GTFSwizard::get_distances()], [GTFSwizard::get_durations()]
#' @export
get_speeds <- function(gtfs, method = "by.route", trips = "all"){
  choices <- c("by.route", "by.trip", "detailed")
  if(!method %in% choices){
    gw_warn_invalid_method(method, choices, "by.route")
    method <- "by.route"
  }
  if(!(length(trips) == 1L && identical(trips, "all"))){
    gtfs <- filter_trip(gtfs, trips)
  }
  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))
  if(method == "by.route"){
    durations <- get_durations(gtfs, "by.route")
    distances <- get_distances(gtfs, "by.route")
    return(dplyr::left_join(
      durations, distances,
      by = c("route_id", "service_pattern", "pattern_frequency"),
      suffix = c(".duration", ".distance")
    ) |>
      dplyr::transmute(
        route_id,
        trips = trips.duration,
        average.speed = (average.distance / 1000) / (average.duration / 3600),
        service_pattern, pattern_frequency
      ))
  }
  if(method == "by.trip"){
    durations <- get_durations(gtfs, "by.trip")
    distances <- get_distances(gtfs, "by.trip")
    return(dplyr::left_join(
      durations, distances,
      by = c("route_id", "trip_id", "service_pattern", "pattern_frequency")
    ) |>
      dplyr::transmute(
        route_id, trip_id,
        average.speed = (distance / 1000) / (duration / 3600),
        service_pattern, pattern_frequency
      ))
  }
  durations <- get_durations(gtfs, "detailed")
  distances <- get_distances(gtfs, "detailed") |>
    dplyr::select(from_stop_id, to_stop_id, distance) |>
    dplyr::distinct()
  dplyr::left_join(
    durations, distances, by = c("from_stop_id", "to_stop_id")
  ) |>
    dplyr::mutate(speed = (distance / 1000) / (duration / 3600)) |>
    dplyr::select(
      route_id, trip_id, hour, from_stop_id, to_stop_id, speed,
      service_pattern, pattern_frequency
    )
}
