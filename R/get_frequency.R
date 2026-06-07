#' Calculate Scheduled Service Frequency
#'
#' Counts trip departures by route, shape, stop, or hour. Trips referenced by
#' `frequencies.txt` are expanded using the period's inclusive `start_time`,
#' exclusive `end_time`, and `headway_secs`, as required by GTFS.
#'
#' @param gtfs A GTFS object.
#' @param method One of `"by.route"`, `"by.shape"`, `"by.stop"`, or
#'   `"detailed"`.
#'
#' @return A tibble containing the selected identifiers, frequency, service
#'   pattern, and number of dates represented by the pattern.
#'
#' @examples
#' get_frequency(for_rail_gtfs, "by.route")
#' get_frequency(for_rail_gtfs, "detailed")
#'
#' @references
#' [GTFS Schedule Reference](https://gtfs.org/documentation/schedule/reference/#frequenciestxt)
#' @seealso [GTFSwizard::get_headways()]
#' @export
get_frequency <- function(gtfs, method = "by.route"){
  choices <- c("by.route", "by.shape", "by.stop", "detailed")
  if(!method %in% choices){
    gw_warn_invalid_method(method, choices, "by.route")
    method <- "by.route"
  }
  gtfs <- ensure_wizardgtfs(gtfs)
  instances <- trip_instance_starts(gtfs) |>
    dplyr::left_join(gtfs$trips, by = "trip_id") |>
    dplyr::left_join(get_servicepattern(gtfs), by = "service_id")
  direction <- if("direction_id" %in% names(instances)) "direction_id" else character()

  if(method == "by.stop"){
    calls <- gtfs$stop_times |>
      dplyr::distinct(trip_id, stop_id)
    data <- dplyr::left_join(calls, instances, by = "trip_id")
    groups <- c("stop_id", direction, "service_pattern", "pattern_frequency")
    return(data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(daily.frequency = dplyr::n(), .groups = "drop"))
  }
  if(method == "by.shape"){
    if(!"shape_id" %in% names(instances)){
      gw_stop("`by.shape` requires `trips$shape_id`.")
    }
    groups <- c("shape_id", direction, "service_pattern", "pattern_frequency")
    return(instances |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(daily.frequency = dplyr::n(), .groups = "drop"))
  }
  if(method == "detailed"){
    instances$hour <- floor(instances$start_seconds / 3600)
    groups <- c(
      "route_id", direction, "hour", "service_pattern", "pattern_frequency"
    )
    return(instances |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(frequency = dplyr::n(), .groups = "drop"))
  }
  groups <- c("route_id", direction, "service_pattern", "pattern_frequency")
  instances |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::summarise(daily.frequency = dplyr::n(), .groups = "drop")
}
