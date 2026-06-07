#' Explore GTFS Data in an Interactive Shiny Dashboard
#'
#' Opens a lightweight Shiny dashboard for exploring a GTFS feed. The dashboard
#' shows system summary cards, route and stop maps, service calendar, and key
#' operational charts.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. When omitted
#'   or `NULL` in an interactive session, a file-selection window opens so the
#'   user can choose a GTFS `.zip` archive.
#'
#' @return A Shiny app object.
#'
#' @examples
#' if (interactive()) {
#'   explore_gtfs()
#'   explore_gtfs(GTFSwizard::for_rail_gtfs)
#' }
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_shapes()], [GTFSwizard::plot_calendar()]
#'
#' @export
explore_gtfs <- function(gtfs = NULL){
  require_pkg("shiny", "`explore_gtfs()`")
  require_pkg("leaflet", "`explore_gtfs()`")
  if(is.null(gtfs)){
    if(!interactive()){
      gw_stop("`gtfs` is required in non-interactive sessions.")
    }
    selected_file <- tryCatch(
      file.choose(),
      error = function(error){
        gw_stop("no GTFS file was selected.")
      }
    )
    gtfs <- GTFSwizard::read_gtfs(selected_file)
  }
  UseMethod('explore_gtfs')
}

#' @exportS3Method GTFSwizard::explore_gtfs list
explore_gtfs.list <- function(gtfs){
  gtfs <- ensure_wizardgtfs(gtfs)
  explore_gtfs.wizardgtfs(gtfs)
}

#' @exportS3Method GTFSwizard::explore_gtfs wizardgtfs
explore_gtfs.wizardgtfs <- function(gtfs){
  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))

  shapes_sf <- GTFSwizard::get_shapes_sf(gtfs$shapes)
  stops_sf <- GTFSwizard::get_stops_sf(gtfs$stops)
  route_shapes_sf <- gtfs$trips |>
    dplyr::select(route_id, shape_id) |>
    dplyr::filter(!is.na(shape_id), nzchar(as.character(shape_id))) |>
    dplyr::distinct() |>
    dplyr::left_join(shapes_sf, by = "shape_id") |>
    sf::st_as_sf()

  stop_counts <- gtfs$stop_times %>%
    dplyr::group_by(stop_id) %>%
    dplyr::summarise(trips = dplyr::n(), .groups = "drop")

  stops_sf <- stops_sf %>%
    dplyr::left_join(stop_counts, by = "stop_id") %>%
    dplyr::mutate(trips = tidyr::replace_na(trips, 0))

  routes_summary <- gtfs$routes %>%
    dplyr::left_join(
      gtfs$trips %>%
        dplyr::group_by(route_id) %>%
        dplyr::summarise(trips = dplyr::n(), .groups = "drop"),
      by = "route_id"
    ) %>%
    dplyr::mutate(trips = tidyr::replace_na(trips, 0))

  agency_count <- if("agency_id" %in% names(gtfs$agency)){
    length(unique(gtfs$agency$agency_id))
  }else{
    nrow(gtfs$agency)
  }
  css <- "
    body { background: #f6f8fb; }
    .navbar { border: 0; border-radius: 0; margin-bottom: 0; }
    .gw-page { padding: 18px; }
    .gw-card {
      background: #fff; border: 1px solid #d9e1ea; border-radius: 6px;
      padding: 14px; margin-bottom: 14px;
    }
    .gw-stat { font-size: 28px; font-weight: 700; line-height: 1; color: #17456b; }
    .gw-label { color: #5f6f7d; font-size: 12px; text-transform: uppercase; }
    .gw-map { border: 1px solid #d9e1ea; border-radius: 6px; overflow: hidden; }
    .gw-table-scroll { overflow-x: auto; }
    .form-group { margin-bottom: 10px; }
    @media (max-width: 767px) {
      .gw-page { padding: 12px; }
      .gw-stats .col-sm-2 { float: left; width: 50%; }
      .gw-stat { font-size: 24px; }
    }
  "

  stat_card <- function(label, value){
    shiny::div(class = "gw-card", shiny::div(class = "gw-stat", value), shiny::div(class = "gw-label", label))
  }

  ui <- shiny::navbarPage(
    title = "GTFSwizard Explorer",
    header = shiny::tags$head(shiny::tags$style(css)),
    shiny::tabPanel(
      "System",
      shiny::fluidPage(
        class = "gw-page",
        shiny::fluidRow(
          class = "gw-stats",
          shiny::column(2, stat_card("Routes", nrow(gtfs$routes))),
          shiny::column(2, stat_card("Trips", nrow(gtfs$trips))),
          shiny::column(2, stat_card("Stops", nrow(gtfs$stops))),
          shiny::column(2, stat_card("Shapes", length(unique(gtfs$shapes$shape_id)))),
          shiny::column(2, stat_card("Service days", length(unique(gtfs$dates_services$date)))),
          shiny::column(2, stat_card("Agencies", agency_count))
        ),
        shiny::tabsetPanel(
          id = "system_view",
          shiny::tabPanel(
            "Network",
            shiny::fluidRow(
              shiny::column(
                9,
                shiny::div(
                  class = "gw-map",
                  leaflet::leafletOutput("system_map", height = "66vh")
                )
              ),
              shiny::column(
                3,
                shiny::div(
                  class = "gw-card",
                  shiny::h4("Agency"),
                  shiny::div(
                    class = "gw-table-scroll",
                    shiny::tableOutput("agency_table")
                  )
                )
              )
            )
          ),
          shiny::tabPanel(
            "Service",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Frequency",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("frequency_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Headways",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("headway_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Service span",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("service_span_plot", height = "720px")
                )
              ),
              shiny::tabPanel(
                "Weekday and hour",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("service_heatmap_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Fleet",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("fleet_plot", height = "520px")
                )
              )
            )
          ),
          shiny::tabPanel(
            "Performance",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Speed",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("speed_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Dwell time",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("dwell_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Trip duration",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("route_duration_plot", height = "680px")
                )
              ),
              shiny::tabPanel(
                "Service supply",
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("service_supply_plot", height = "680px")
                )
              )
            )
          ),
          shiny::tabPanel(
            "Calendar",
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::div(
                  class = "gw-card",
                  shiny::plotOutput("calendar_plot", height = "520px")
                )
              )
            )
          )
        )
      )
    ),
    shiny::tabPanel(
      "Routes",
      shiny::fluidPage(
        class = "gw-page",
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::div(
              class = "gw-card",
              shiny::selectizeInput(
                "selected_routes",
                "Routes",
                choices = sort(unique(gtfs$routes$route_id)),
                selected = sort(unique(gtfs$routes$route_id))[1],
                multiple = TRUE,
                options = list(plugins = list("remove_button"))
              ),
              shiny::div(class = "gw-table-scroll", shiny::tableOutput("route_table"))
            )
          ),
          shiny::column(9, shiny::div(class = "gw-map", leaflet::leafletOutput("route_map", height = "72vh")))
        ),
        shiny::fluidRow(
          shiny::column(12, shiny::div(class = "gw-card", shiny::plotOutput("route_frequency_plot", height = "330px")))
        )
      )
    )
  )

  server <- function(input, output, session){
    output$agency_table <- shiny::renderTable({
      gtfs$agency
    }, striped = TRUE, spacing = "xs")

    output$system_map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Light") %>%
        leaflet::addTiles(group = "OSM") %>%
        leaflet::addLayersControl(baseGroups = c("Light", "OSM")) %>%
        leaflet::addPolylines(data = shapes_sf, color = "#2364aa", weight = 3, opacity = 0.8) %>%
        leaflet::addCircleMarkers(
          data = stops_sf,
          radius = 2,
          stroke = FALSE,
          fillOpacity = 0.7,
          color = "#20a39e",
          popup = ~paste0(stop_name, "<br>Trips: ", trips)
        )
    })

    output$frequency_plot <- shiny::renderPlot({
      GTFSwizard::plot_frequency(gtfs)
    })

    output$fleet_plot <- shiny::renderPlot({
      fleet <- GTFSwizard::get_fleet(gtfs, method = "by.hour") %>%
        dplyr::mutate(hour = as.numeric(hour)) %>%
        dplyr::filter(is.finite(hour), is.finite(fleet))

      ggplot2::ggplot(fleet, ggplot2::aes(hour, fleet, color = service_pattern, group = service_pattern)) +
        ggplot2::geom_line(linewidth = 1) +
        hour_scale(fleet$hour) +
        ggplot2::labs(
          title = "Scheduled Fleet by Hour",
          subtitle = "Each line is one service pattern",
          x = "Scheduled clock hour",
          y = "Simultaneously active scheduled trips (vehicles)",
          color = "Service pattern"
        ) +
        theme_gtfswizard()
    })

    output$speed_plot <- shiny::renderPlot({
      speed <- GTFSwizard::get_speeds(gtfs, method = "by.route") %>%
        dplyr::filter(
          is.finite(average.speed), is.finite(trips),
          is.finite(pattern_frequency)
        )
      ggplot2::ggplot(speed, ggplot2::aes(average.speed, weight = trips * pattern_frequency)) +
        ggplot2::geom_histogram(fill = gtfswizard_colors()[["blue"]], color = "white", bins = 25) +
        ggplot2::labs(
          title = "Scheduled Route Speed Distribution",
          subtitle = "Each observation is a route-service pattern summary",
          x = "Average scheduled speed (km/h)",
          y = "Weighted route-pattern observations"
        ) +
        theme_gtfswizard()
    })

    output$headway_plot <- shiny::renderPlot({
      GTFSwizard::plot_headways(gtfs)
    })

    output$service_heatmap_plot <- shiny::renderPlot({
      GTFSwizard::plot_serviceheatmap(gtfs)
    })

    output$service_span_plot <- shiny::renderPlot({
      GTFSwizard::plot_servicespan(gtfs, top_n = 25L)
    })

    output$dwell_plot <- shiny::renderPlot({
      dwell_time <- GTFSwizard::get_dwelltimes(gtfs, method = "by.hour") %>%
        dplyr::filter(
          is.finite(average.dwelltime), is.finite(trips),
          is.finite(pattern_frequency)
        )
      ggplot2::ggplot(dwell_time, ggplot2::aes(average.dwelltime, weight = trips * pattern_frequency)) +
        ggplot2::geom_histogram(fill = gtfswizard_colors()[["coral"]], color = "white", bins = 25) +
        ggplot2::labs(
          title = "Scheduled Dwell Time Distribution",
          subtitle = "Each observation is a route-hour-service pattern summary",
          x = "Average scheduled dwell time (seconds)",
          y = "Weighted route-hour observations"
        ) +
        theme_gtfswizard()
    })

    output$route_duration_plot <- shiny::renderPlot({
      GTFSwizard::plot_routeduration(gtfs, top_n = 15L)
    })

    output$service_supply_plot <- shiny::renderPlot({
      GTFSwizard::plot_servicesupply(gtfs, top_n = 15L)
    })

    output$calendar_plot <- shiny::renderPlot({
      GTFSwizard::plot_calendar(gtfs, facet_by_year = TRUE)
    })

    selected_routes <- shiny::reactive({
      selected <- input$selected_routes
      if(is.null(selected) || length(selected) == 0){
        return(character())
      }
      selected
    })

    output$route_table <- shiny::renderTable({
      routes_summary %>%
        dplyr::filter(route_id %in% selected_routes()) %>%
        dplyr::select(dplyr::any_of(c(
          "route_id", "route_short_name", "route_long_name", "trips"
        )))
    }, striped = TRUE, spacing = "xs")

    output$route_map <- leaflet::renderLeaflet({
      route_shapes <- route_shapes_sf %>%
        dplyr::filter(route_id %in% selected_routes())
      routes <- sort(unique(route_shapes$route_id))
      route_colors <- stats::setNames(gtfswizard_palette(length(routes)), routes)

      route_stop_ids <- gtfs$stop_times %>%
        dplyr::filter(trip_id %in% gtfs$trips$trip_id[gtfs$trips$route_id %in% selected_routes()]) %>%
        dplyr::select(stop_id) %>%
        unique()

      route_stops <- stops_sf %>% dplyr::filter(stop_id %in% route_stop_ids$stop_id)

      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Light") %>%
        leaflet::addTiles(group = "OSM") %>%
        leaflet::addLayersControl(baseGroups = c("Light", "OSM")) %>%
        leaflet::addPolylines(
          data = route_shapes,
          color = unname(route_colors[as.character(route_shapes$route_id)]),
          weight = 4,
          opacity = 0.88,
          popup = ~paste0("Route: ", route_id)
        ) %>%
        leaflet::addCircleMarkers(
          data = route_stops,
          radius = 2.5,
          stroke = FALSE,
          fillOpacity = 0.8,
          color = "#20a39e",
          popup = ~paste0(stop_name, "<br>Trips: ", trips)
        )
    })

    output$route_frequency_plot <- shiny::renderPlot({
      req_routes <- selected_routes()
      if(length(req_routes) == 0){
        return(invisible(NULL))
      }
      GTFSwizard::plot_routefrequency(gtfs, req_routes)
    })
  }

  shiny::shinyApp(ui, server)
}
