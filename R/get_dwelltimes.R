#' Calculate Dwell Times in GTFS Data
#'
#' The `get_dwelltimes` function calculates dwell times within a `wizardgtfs` object using different methods. Depending on the selected `method`, it can provide average dwell times per route, per trip, by hour, or detailed dwell times at each stop.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param max.dwelltime Numeric. The maximum allowable dwell time (in seconds). Dwell times exceeding this value are excluded from the calculations. Defaults to 90 seconds.
#' @param method A character string specifying the calculation method. Options include:
#'   \describe{
#'     \item{"by.hour"}{Calculates the average dwell time per hour of the day across all trips.}
#'     \item{"by.route"}{Calculates the average dwell time for each route.}
#'     \item{"by.trip"}{Calculates the average dwell time for each trip.}
#'     \item{"detailed"}{Calculates detailed dwell times at each stop within every trip.}
#'   }
#'
#' @return A data frame containing dwell times based on the specified method:
#'   \describe{
#'     \item{If `method = "by.hour"`}{Returns a data frame with columns: `hour`, `trips`, `average.dwelltime`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by.route"`}{Returns a data frame with columns: `route_id`, `trips`, `average.dwelltime`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by.trip"`}{Returns a data frame with columns: `route_id`, `trip_id`, `average.dwelltime`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "detailed"`}{Returns a data frame with columns: `route_id`, `trip_id`, `stop_id`, `hour`, `dwell_time`, `service_pattern`, and `pattern_frequency`.}
#'   }
#'
#' @details
#' This function calls specific sub-functions based on the selected method:
#'
#' - "by.hour": Calculates the average dwell time for each hour of the day.
#'
#' - "by.route": Calculates average dwell times across each route.
#'
#' - "by.trip": Calculates the mean dwell time for each trip.
#'
#' - "detailed": Calculates departure minus arrival at each stop call.
#'
#' If an invalid `method` is specified, the function defaults to `"by.route"` and provides a warning.
#'
#' @examples
#' # Calculate dwell times by hour
#' dwelltimes_by_hour <- get_dwelltimes(gtfs = for_rail_gtfs, max.dwelltime = 120, method = "by.hour")
#'
#' # Calculate dwell times by route
#' dwelltimes_by_route <- get_dwelltimes(gtfs = for_rail_gtfs, max.dwelltime = 90, method = "by.route")
#'
#' # Calculate dwell times by trip
#' dwelltimes_by_trip <- get_dwelltimes(gtfs = for_rail_gtfs, max.dwelltime = 45, method = "by.trip")
#'
#' # Calculate detailed dwell times between stops
#' detailed_dwelltimes <- get_dwelltimes(gtfs = for_rail_gtfs, max.dwelltime = 60, method = "detailed")
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @importFrom dplyr mutate group_by reframe select left_join filter
#' @export
get_dwelltimes <- function(gtfs, max.dwelltime = 90, method = 'by.route'){
  if(!is.numeric(max.dwelltime) || length(max.dwelltime) != 1L ||
     is.na(max.dwelltime) || max.dwelltime < 0){
    gw_stop("`max.dwelltime` must be one non-negative number of seconds.")
  }

  if (!method %in% c('by.hour', 'by.route', 'detailed', 'by.trip')) {
    gw_warn_invalid_method(
      method,
      c('by.hour', 'by.route', 'by.trip', 'detailed'),
      'by.route'
    )
    method <- 'by.route'
  }

  switch(
    method,
    by.hour = get_dwelltime_byhour(gtfs, max.dwelltime),
    by.route = get_dwelltime_byroute(gtfs, max.dwelltime),
    by.trip = get_dwelltime_bytrip(gtfs, max.dwelltime),
    detailed = get_dwelltime_detailed(gtfs, max.dwelltime)
  )

}

get_dwelltime_byhour <- function(gtfs, max.dwelltime = 90){

  dwell_time_data(gtfs, max.dwelltime) %>%
    dplyr::group_by(hour, service_pattern, pattern_frequency) %>%
    dplyr::reframe(
      average.dwelltime = mean(dwell_time),
      trips = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::select(hour, trips, average.dwelltime, service_pattern, pattern_frequency)

}

get_dwelltime_byroute <- function(gtfs, max.dwelltime = 90){

  dwell_time_data(gtfs, max.dwelltime) %>%
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>%
    dplyr::reframe(
      average.dwelltime = mean(dwell_time),
      trips = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::select(route_id, trips, average.dwelltime, service_pattern, pattern_frequency)

}

get_dwelltime_bytrip <- function(gtfs, max.dwelltime = 90){

  dwell_time_data(gtfs, max.dwelltime) %>%
    dplyr::group_by(route_id, trip_id, service_pattern, pattern_frequency) %>%
    dplyr::reframe(
      average.dwelltime = mean(dwell_time),
      .groups = "drop"
    ) %>%
    dplyr::select(route_id, trip_id, average.dwelltime, service_pattern, pattern_frequency)

}

get_dwelltime_detailed <- function(gtfs, max.dwelltime = 90){

  dwell_time_data(gtfs, max.dwelltime) %>%
    dplyr::select(route_id, trip_id, stop_id, hour, dwell_time, service_pattern, pattern_frequency)

}

dwell_time_data <- function(gtfs, max.dwelltime){
  gtfs <- ensure_wizardgtfs(gtfs)
  service_pattern <- GTFSwizard::get_servicepattern(gtfs)

  gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>%
    dplyr::mutate(
      arrival_seconds = gtfs_time_to_seconds(arrival_time),
      departure_seconds = gtfs_time_to_seconds(departure_time),
      hour = floor(arrival_seconds / 3600),
      dwell_time = departure_seconds - arrival_seconds
    ) %>%
    dplyr::filter(
      !is.na(dwell_time),
      dwell_time >= 0,
      dwell_time <= max.dwelltime
    ) %>%
    dplyr::left_join(gtfs$trips, by = "trip_id") %>%
    dplyr::left_join(
      service_pattern,
      by = "service_id",
      relationship = "many-to-many"
    )
}
