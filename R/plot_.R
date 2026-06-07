#' Plot System Frequency by Hour
#'
#' Shows the distribution and weighted mean of scheduled trip departures by
#' hour. Service-pattern date counts provide the weights.
#'
#' @param gtfs A GTFS object.
#' @return A `ggplot` object.
#'
#' @examples
#' plot_frequency(for_rail_gtfs)
#'
#' @seealso [GTFSwizard::get_frequency()]
#' @export
plot_frequency <- function(gtfs){
  colors <- gtfswizard_colors()
  data <- GTFSwizard::get_frequency(gtfs, method = "detailed") |>
    dplyr::mutate(hour = as.numeric(hour)) |>
    dplyr::filter(is.finite(hour), is.finite(frequency))
  if(!nrow(data)){
    gw_stop("no frequency data are available to plot.")
  }
  hourly <- data |>
    dplyr::group_by(hour) |>
    dplyr::summarise(
      frequency = stats::weighted.mean(
        frequency, pattern_frequency, na.rm = TRUE
      ),
      .groups = "drop"
    )
  overall <- stats::weighted.mean(
    data$frequency, data$pattern_frequency, na.rm = TRUE
  )

  ggplot2::ggplot(data, ggplot2::aes(hour, frequency)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = hour),
      width = 0.65, fill = colors[["light"]], color = colors[["gray"]],
      outlier.alpha = 0.35
    ) +
    ggplot2::geom_line(
      data = hourly, color = colors[["teal"]], linewidth = 1
    ) +
    ggplot2::geom_point(
      data = hourly, color = colors[["teal"]], size = 1.8
    ) +
    ggplot2::geom_hline(
      yintercept = overall, color = colors[["coral"]],
      linetype = "dashed", linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "System Frequency",
      subtitle = paste("Overall weighted mean:", round(overall, 1), "trips per hour"),
      x = "Hour", y = "Scheduled trips"
    ) +
    hour_scale(data$hour) +
    ggplot2::expand_limits(y = 0) +
    theme_gtfswizard()
}

#' Plot Route Frequency by Hour
#'
#' @param gtfs A GTFS object.
#' @param route Optional character vector of route IDs. `NULL` includes all
#'   routes.
#' @return A `ggplot` object.
#'
#' @examples
#' plot_routefrequency(
#'   for_rail_gtfs,
#'   route = for_rail_gtfs$routes$route_id[1:2]
#' )
#'
#' @seealso [GTFSwizard::get_frequency()]
#' @export
plot_routefrequency <- function(gtfs, route = NULL){
  requested_routes <- route
  if(!is.null(route)){
    gtfs <- GTFSwizard::filter_route(gtfs, route)
  } else {
    gtfs <- ensure_wizardgtfs(gtfs)
  }
  data <- GTFSwizard::get_frequency(gtfs, method = "detailed") |>
    dplyr::mutate(hour = as.numeric(hour)) |>
    dplyr::filter(is.finite(hour), is.finite(frequency))
  if(!nrow(data)){
    gw_stop("no route-frequency data are available to plot.")
  }
  primary <- get_servicepattern(gtfs)$service_pattern[1L]
  data$.emphasis <- ifelse(data$service_pattern == primary, "Primary pattern", "Other patterns")
  route_values <- stats::setNames(
    gtfswizard_palette(length(unique(data$route_id))),
    unique(data$route_id)
  )
  show_route_legend <- !is.null(requested_routes) &&
    length(unique(data$route_id)) <= 12L

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      hour, frequency, color = route_id,
      group = interaction(route_id, service_pattern),
      alpha = .emphasis
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_alpha_manual(
      values = c("Primary pattern" = 1, "Other patterns" = 0.28)
    ) +
    ggplot2::scale_color_manual(values = route_values) +
    ggplot2::labs(
      title = "Route Frequency",
      subtitle = "Scheduled departures by hour and service pattern",
      x = "Hour", y = "Scheduled trips", color = "Route", alpha = NULL
    ) +
    hour_scale(data$hour) +
    ggplot2::expand_limits(y = 0) +
    theme_gtfswizard()
  if(show_route_legend){
    plot + ggplot2::guides(
      color = ggplot2::guide_legend(
        title = "Route", nrow = min(2L, length(route_values)),
        override.aes = list(alpha = 1, linewidth = 1.2)
      ),
      alpha = ggplot2::guide_legend(title = "Service pattern")
    )
  } else {
    plot + ggplot2::theme(legend.position = "none")
  }
}

#' Plot System Headway by Hour
#'
#' @param gtfs A GTFS object.
#' @return A `ggplot` object with headway in minutes.
#'
#' @examples
#' plot_headways(for_rail_gtfs)
#'
#' @seealso [GTFSwizard::get_headways()]
#' @export
plot_headways <- function(gtfs){
  colors <- gtfswizard_colors()
  data <- GTFSwizard::get_headways(gtfs, method = "by.hour") |>
    dplyr::mutate(
      weight = .data$pattern_frequency * .data$valid_trips,
      hour = as.numeric(.data$hour)
    ) |>
    dplyr::filter(
      is.finite(.data$hour),
      is.finite(.data$headway_minutes),
      is.finite(.data$weight)
    )
  if(!nrow(data)){
    gw_stop("no headway data are available to plot.")
  }
  overall <- stats::weighted.mean(
    data$headway_minutes, data$weight, na.rm = TRUE
  )
  pattern_values <- stats::setNames(
    gtfswizard_palette(length(unique(data$service_pattern))),
    unique(data$service_pattern)
  )
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      hour, headway_minutes, color = service_pattern,
      group = service_pattern
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_hline(
      yintercept = overall, color = colors[["coral"]],
      linetype = "dashed", linewidth = 0.8
    ) +
    ggplot2::scale_color_manual(values = pattern_values) +
    ggplot2::labs(
      title = "System Headway",
      subtitle = paste("Overall weighted mean:", round(overall, 1), "minutes"),
      x = "Hour", y = "Average headway (minutes)", color = "Service pattern"
    ) +
    hour_scale(data$hour) +
    ggplot2::expand_limits(y = 0) +
    theme_gtfswizard()
}

#' Plot High-Frequency Corridors
#'
#' @param gtfs A GTFS object.
#' @param i Proportion of highest-frequency segments to retain.
#' @param min.length Minimum corridor length in meters.
#' @return A `ggplot` map.
#'
#' @seealso [GTFSwizard::get_corridor()]
#' @export
plot_corridor <- function(gtfs, i = 0.01, min.length = 1500){
  colors <- gtfswizard_colors()
  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))
  shapes <- get_shapes_sf(gtfs$shapes)
  corridors <- get_corridor(gtfs, i = i, min.length = min.length)
  corridor_values <- stats::setNames(
    gtfswizard_palette(nrow(corridors)), corridors$corridor
  )
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapes, linewidth = 0.45, color = colors[["light"]]) +
    ggplot2::geom_sf(
      data = corridors,
      ggplot2::aes(color = corridor),
      linewidth = 1.4, lineend = "round"
    ) +
    ggplot2::scale_color_manual(values = corridor_values) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = "High-Frequency Corridors",
      subtitle = "Connected high-service stop pairs",
      color = "Corridor"
    ) +
    theme_gtfswizard_map()
}

#' Plot Transit Hubs
#'
#' @param gtfs A GTFS object.
#' @param i Proportion of stops with the most routes to retain.
#' @return A `ggplot` map.
#'
#' @details To keep dense networks readable, at most 40 of the highest-ranked
#'   stops are drawn. Ranking uses distinct route count and then trip count.
#'
#' @seealso [GTFSwizard::get_hubs()]
#' @export
plot_hubs <- function(gtfs, i = 0.05){
  if(!is.numeric(i) || length(i) != 1L || is.na(i) || i <= 0 || i > 1){
    gw_stop("`i` must be one number greater than 0 and no greater than 1.")
  }
  colors <- gtfswizard_colors()
  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))
  hubs <- get_hubs(gtfs)
  requested <- max(1L, ceiling(nrow(hubs) * i))
  shown <- min(40L, requested)
  hubs <- utils::head(hubs, shown)
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = get_shapes_sf(gtfs$shapes),
      linewidth = 0.45, color = colors[["light"]]
    ) +
    ggplot2::geom_sf(
      data = hubs,
      ggplot2::aes(size = n_routes, fill = n_routes),
      shape = 21, color = "white", stroke = 0.5, alpha = 0.92
    ) +
    ggplot2::scale_fill_gradient(
      low = colors[["gold"]], high = colors[["coral"]]
    ) +
    ggplot2::scale_size(range = c(2.2, 7.5)) +
    ggplot2::guides(size = "none") +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = "Transit Hubs",
      subtitle = paste(
        "Top", shown, "stops ranked by distinct routes",
        if(requested > shown) "(display capped for readability)" else ""
      ),
      fill = "Routes", size = "Routes"
    ) +
    theme_gtfswizard_map()
}
