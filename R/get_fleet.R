#' Estimate Simultaneous Vehicles
#'
#' Estimates the number of active trip instances from their first departure to
#' final arrival. Frequency-based services are expanded from `frequencies.txt`.
#'
#' @param gtfs A GTFS object.
#' @param method One of `"by.route"`, `"by.hour"`, `"peak"`, or `"detailed"`.
#'
#' @return A tibble containing fleet estimates by the selected grouping and
#' service pattern. GTFS times beyond 24 hours remain in the following service
#' day rather than being wrapped.
#'
#' @examples
#' get_fleet(for_rail_gtfs, "by.route")
#' get_fleet(for_rail_gtfs, "by.hour")
#'
#' @export
get_fleet <- function(gtfs, method = "by.route"){
  choices <- c("by.route", "by.hour", "peak", "detailed")
  if(!method %in% choices){
    gw_warn_invalid_method(method, choices, "by.route")
    method <- "by.route"
  }
  switch(
    method,
    by.route = get_fleet_byroute(gtfs),
    by.hour = get_fleet_byhour(gtfs),
    peak = get_fleet_peak(gtfs),
    detailed = get_fleet_detailed(gtfs)
  )
}

get_fleet_byroute <- function(gtfs){
  fleet_event_table(gtfs, by_route = TRUE) |>
    dplyr::group_by(route_id, service_pattern, pattern_frequency) |>
    dplyr::summarise(fleet = max(fleet), .groups = "drop")
}

get_fleet_byhour <- function(gtfs){
  fleet_event_table(gtfs, by_route = FALSE) |>
    dplyr::mutate(hour = floor(time / 3600)) |>
    dplyr::group_by(service_pattern, pattern_frequency, hour) |>
    dplyr::summarise(fleet = max(fleet), .groups = "drop")
}

get_fleet_peak <- function(gtfs){
  get_fleet_byhour(gtfs) |>
    dplyr::group_by(service_pattern) |>
    dplyr::slice_max(fleet, n = 3L, with_ties = FALSE) |>
    dplyr::ungroup()
}

get_fleet_detailed <- function(gtfs){
  fleet_event_table(gtfs, by_route = TRUE) |>
    dplyr::select(
      route_id, net.fleet, fleet, time,
      service_pattern, pattern_frequency
    )
}

fleet_event_table <- function(gtfs, by_route = FALSE){
  gtfs <- ensure_wizardgtfs(gtfs)
  ordered <- gtfs$stop_times |>
    dplyr::arrange(trip_id, stop_sequence) |>
    dplyr::mutate(
      .arrival = gtfs_time_to_seconds(arrival_time),
      .departure = gtfs_time_to_seconds(departure_time)
    ) |>
    dplyr::group_by(trip_id) |>
    dplyr::summarise(
      .base_start = dplyr::first(.departure),
      .duration = dplyr::last(.arrival) - dplyr::first(.departure),
      .groups = "drop"
    )
  intervals <- trip_instance_starts(gtfs) |>
    dplyr::left_join(
      ordered[, c("trip_id", ".duration")], by = "trip_id"
    ) |>
    dplyr::mutate(starts = start_seconds, ends = start_seconds + .duration) |>
    dplyr::left_join(gtfs$trips, by = "trip_id") |>
    dplyr::left_join(get_servicepattern(gtfs), by = "service_id")
  group_cols <- c(
    if(by_route) "route_id",
    "service_pattern", "pattern_frequency"
  )
  starts <- intervals[group_cols]
  starts$time <- intervals$starts
  starts$net.fleet <- 1L
  ends <- intervals[group_cols]
  ends$time <- intervals$ends
  ends$net.fleet <- -1L
  dplyr::bind_rows(starts, ends) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "time")))) |>
    dplyr::summarise(net.fleet = sum(net.fleet), .groups = "drop") |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, "time")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(fleet = cumsum(net.fleet)) |>
    dplyr::ungroup()
}
