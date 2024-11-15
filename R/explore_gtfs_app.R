library(dplyr)
library(GTFSwizard)
library(shiny)
library(future)
library(shinyjs)
library(promises)
library(kableExtra)

wizardbox <- function(...){
  htmltools::tags$div(
    style = 'border: solid #E76F0D 1px;border-radius:10px;',
    htmltools::tags$div(
      style = 'border: solid #ffffff00 2px; border-radius:11px;',
      ...
    )
  )
}

agency_table <- function(gtfs){
  agency <- gtfs$agency %>%
    dplyr::filter(agency_id %in% c(gtfs$routes$agency_id %>% unique)) %>%
    t %>%
    data.frame %>%
    tibble::rownames_to_column() %>%
    stats::setNames(c('Agency attribute', ''))

  agency <- gtfs$agency %>%
    dplyr::filter(agency_id %in% c(gtfs$routes$agency_id %>% unique))
  if('agency_url'%in%names(agency)){
    agency <- agency %>%
      dplyr::mutate(agency_url = htmltools::tags$a( href = agency_url, target = '_blank') %>% as.character())
  }
  agency <- agency %>%
    t %>%
    data.frame %>%
    tibble::rownames_to_column() %>%
    stats::setNames(c('Agency attribute', 'Value')) %>%
    mutate(`Agency attribute` = stringr::str_remove(`Agency attribute`,'agency_'))

  ftbl <- kableExtra::kbl(agency, format = 'html', escape = F) %>%
    kableExtra::kable_styling(full_width = FALSE, font = "Times New Roman") %>%
    kableExtra::row_spec(0, background = "#065F2890") %>%
    kableExtra::row_spec(1:nrow(agency), background = "#009C9190")


  return(shiny::HTML(ftbl))
}

options(future.globals.maxSize = 850*1024^2)

plan(multisession)

gtfs <- GTFSwizard::for_bus_gtfs


# ui ----
ui <- shiny::navbarPage(
  #theme = bs_theme(bootswatch = 'cosmo'),
  title = "GTFSwizard::explore_gtfs()",
  useShinyjs(),
  tags$body(
    tags$style(
      'body{background-color: #F3F7FD;}
      svg.main-svg{border-radius:10px;}'
    )
  ),
  # overview ----
  shiny::tabPanel('Overview',
                  shiny::fluidRow(
                    shiny::column(
                      width = 7,
                      leaflet::leafletOutput('overview_map1', height = '45vh'),
                      shiny::tags$style(
                        'div#overview_map1{
          width:100%;
          heigth:45vh;
          border:solid green;
          border-radius:10px;
          }')
                    ),
                    shiny::column(
                      width = 5,
                      htmltools::tags$div(
                        style = 'background-color:white;',
                        shiny::uiOutput('agency_table')
                      ) ,
                    ),
                  ),
                  shiny::hr(),
                  shiny::fluidRow(
                    shiny::column(
                      width = 8,
                      wizardbox(
                        plotly::plotlyOutput('freq.sparkline', height = '350px')
                      )
                    ),
                    shiny::column(
                      width = 4,
                      wizardbox(shiny::uiOutput('fleet.sparkline.ui'))
                    )
                  ),
                  shiny::hr(),
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      wizardbox(
                        plotly::plotlyOutput('hist.speed', height = '300px')
                      )
                    ),
                    shiny::column(
                      width = 4,
                      wizardbox(
                        plotly::plotlyOutput('hist.hw', height = '300px')
                      )
                    ),
                    shiny::column(
                      width = 4,
                      wizardbox(
                        plotly::plotlyOutput('hist.dt', height = '300px')
                      )
                    )
                  ),
                  shiny::hr(),
                  shiny::fluidRow(
                    shiny::column(
                      width = 6,
                      wizardbox(
                        plotly::plotlyOutput('hist.dist', height = '300px')
                      )
                    ),
                    shiny::column(
                      width = 6,
                      wizardbox(
                        plotly::plotlyOutput('hist.dur', height = '300px')
                      )
                    )
                  ),
                  shiny::hr(),
                  shiny::fluidRow(
                    shiny::column(
                      shiny::plotOutput('p.calendar',
                                                    height = paste0(as.numeric(max(lubridate::year(gtfs$dates_services$date)) - as.numeric(min(lubridate::year(gtfs$dates_services$date)))  + 5) * 75, "px")
                    ) %>% wizardbox(),
                    width = 12)
                  ),
                  shiny::hr()
  ),
  # BY ROUTE ----
  shiny::tabPanel('By Route',
                  shiny::fluidRow(
                    shiny::selectizeInput(inputId = 'selected.routes',
                                          label = 'Choose routes of interest:',
                                          choices = sort(unique(gtfs$routes$route_id)),
                                          multiple = TRUE)
                  ),
                  shiny::hr(),
                  shiny::fluidRow(
                    shiny::column(
                      width = 7,
                      leaflet::leafletOutput('byroute_map1',
                                             height = '75vh'),
                      shiny::tags$style(
                        'div#overview_map1{
                          width:100%;
                          heigth:60vh;
                          border:solid red;
                          border-radius:10px;
                          }')
                    ),
                    shiny::column(
                      width = 5,
                      shiny::fluidRow(plotly::plotlyOutput('freq.sparkline.byroute',
                                                           #shiny::plotOutput('freq.sparkline.byroute',
                                                           height = '350px'))
                    )
                  ),
                  shiny::hr()
  ),
)

# server ----
server <- function(input, output, session) {

  # general ----

  shinyjs::runjs('setTimeout(Shiny.setInputValue("exec_plots",0),5000)')

  # agency ----


  output$agency_table <-
    renderUI({
      agency_table(gtfs)
    })

  # maps ----
  trips.shp <-
    tidytransit::shapes_as_sf(gtfs$shapes)

  stops.shp <-
    tidytransit::gtfs_as_sf(gtfs) %>%
    .$stops %>%
    dplyr::left_join(
      gtfs$stop_times %>%
        dplyr::group_by(stop_id) %>%
        dplyr::reframe(`# trips` = n())
    )

  output$overview_map1 <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(group = "OSM") %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,group = 'Carto-Light') %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter, group = 'Carto - Dark') %>%
      leaflet::addLayersControl(baseGroups = c('Carto - Light','Carto - Dark','OSM')) %>%
      leaflet::addPolylines(data = trips.shp) %>%
      leaflet::addAwesomeMarkers(data = stops.shp,
                                 popup = ~paste0('# trips ', `# trips`, '\n', stop_name),
                                 clusterOptions = leaflet::markerClusterOptions()
      ) %>%
      leaflet.extras::addFullscreenControl() %>%
      leaflet.extras::addResetMapButton() %>%
      leaflet.extras::addControlGPS() %>%
      leaflet.extras::addSearchOSM()

  })

  # frequency ----
  output$freq.sparkline <- plotly::renderPlotly({GTFSwizard::plot_frequency(gtfs)})

  # fleet ----

  plot_fleet_future <- function(){
    fleet <-
      GTFSwizard::get_fleet(gtfs, method = 'by.hour') %>%
      dplyr::mutate(hour = as.numeric(hour)) %>% suppressMessages()

    fleet.hline <-
      weighted.mean(fleet$fleet, fleet$pattern_frequency, na.rm = TRUE)

    p.fleet.sparkline <-
      ggplot2::ggplot() +
      ggplot2::geom_vline(xintercept = c(0, 6, 12, 18, 24), color = 'gray', alpha = .25, linetype = 'dashed') +
      ggplot2::geom_hline(ggplot2::aes(yintercept = fleet.hline, linetype = 'Overall\nAverage\nFleet\n'), linewidth = .75) +
      ggplot2::geom_line(data = fleet, ggplot2::aes(hour, fleet, color = service_pattern, group = service_pattern), linewidth = 1) +
      ggplot2::labs(x = 'Hour of the day', y = 'Fleet (# vehicles)', title = 'System Fleet') +
      hrbrthemes::theme_ipsum() +
      hrbrthemes::scale_y_comma(big.mark = " ") +
      ggplot2::scale_linetype_manual(values = 'dashed') +
      ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
      ggplot2::theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none'
      )
    return(p.fleet.sparkline)
  }

  output$fleet.sparkline.ui <- shiny::renderUI({
    tags$div(
      style = 'font-size:24px; text-align:center;margin: auto; color: gray;',
      icon('solid fa-spinner fa-spin'),
      'Processing...'
    )
  })

  observeEvent(input$exec_plots,{

    output$fleet.sparkline.ui <- shiny::renderUI({
      plotly::plotlyOutput('fleet.sparkline', height = '350px')
    })

    output$fleet.sparkline <- plotly::renderPlotly({

      future_promise({
        plot_fleet_future()
      }) %...>%
        plotly::ggplotly(tooltip = c('x', 'y', 'color')) %...>%
        suppressWarnings()

    })
  })



  # headway ----

  observeEvent(input$exec_plots,{
    output$hist.hw <- plotly::renderPlotly({
      GTFSwizard::plot_headways(gtfs)
    })
  })



  # speed ----

  plot_speed_future <- function(){
    speed <-
      GTFSwizard::get_speeds(gtfs, method = 'by.route')
    p.hist.speed <-
      ggplot2::ggplot() +
      ggplot2::geom_histogram(data = speed, ggplot2::aes(x = average.speed, weight = trips * pattern_frequency)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(speed$average.speed, na.rm = TRUE), color = paste('Overall\naverage\nhourly\nSpeed of\n', mean(speed$average.speed, na.rm = TRUE) %>% round, 'km/h')), linetype = 'dashed', linewidth = .75) +
      ggplot2::labs(title = 'Speeds Distribution (for all dates)', x = 'Speed (km/h)', y = 'Frequency (# route)', colour = '') +
      hrbrthemes::scale_x_comma(big.mark = " ") +
      hrbrthemes::scale_y_comma(big.mark = " ") +
      hrbrthemes::theme_ipsum() +
      ggplot2::theme(
        axis.ticks.x = element_blank()
      ) +
      ggplot2::scale_color_manual(values = 'red')
    return(p.hist.speed)
  }

  observeEvent(input$exec_plots,{
    output$hist.speed <- plotly::renderPlotly({

      future_promise({
        plot_speed_future()
      }) %...>%
        plotly::ggplotly()

    })
  })


  # dwell time ----
  dwell_time <-
    get_dwelltimes(gtfs, method = 'by.hour')
  #GTFSwizard::get_dwelltimes(gtfs, method = 'by.hour')

  output$hist.dt <- plotly::renderPlotly({

    p.hist.dt <-
      ggplot2::ggplot() +
      ggplot2::geom_histogram(data = dwell_time, ggplot2::aes(x = average.dwelltime, weight = (trips * pattern_frequency))) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = weighted.mean(dwell_time$average.dwelltime, dwell_time$pattern_frequency, na.rm = TRUE), color = paste('Overall\nAverage\nDwell Time\n', weighted.mean(dwell_time$average.dwelltime, dwell_time$pattern_frequency, na.rm = TRUE) %>% round, 'seconds\n')), linetype = 'dashed', linewidth = .75) +
      ggplot2::labs(title = 'Dwell Time Distribution (for all dates)', x = 'Dwell time (s)', y = 'Frequency (# trips.days)', colour = '') +
      hrbrthemes::scale_x_comma(big.mark = " ") +
      hrbrthemes::scale_y_comma(big.mark = " ") +
      hrbrthemes::theme_ipsum() +
      ggplot2::theme(
        axis.ticks.x = element_blank()
      ) +
      ggplot2::scale_color_manual(values = 'red')

    suppressMessages({
      plotly::ggplotly(p.hist.dt)
    })

  })

  # dist ----
  distances <-
    GTFSwizard::get_distances(gtfs, method = 'by.route') %>%
    dplyr::mutate(average.distance = as.numeric(average.distance))

  output$hist.dist <- plotly::renderPlotly({

    p.hist.dist <-
      ggplot2::ggplot() +
      ggplot2::geom_histogram(data = distances, ggplot2::aes(x = average.distance, weight = (trips * pattern_frequency))) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = weighted.mean(distances$average.distance, distances$pattern_frequency, na.rm = TRUE), color = paste('Overall\nAverage\nDistance\n', weighted.mean(distances$average.distance, distances$pattern_frequency, na.rm = TRUE) %>% round, 'm\n')), linetype = 'dashed', linewidth = .75) +
      ggplot2::labs(title = 'Distance Distribution (for all dates)', x = 'Distance (m)', y = 'Frequency (# trips.days)', colour = '') +
      hrbrthemes::scale_x_comma(big.mark = " ") +
      hrbrthemes::scale_y_comma(big.mark = " ") +
      hrbrthemes::theme_ipsum() +
      ggplot2::theme(
        axis.ticks.x = element_blank()
      ) +
      ggplot2::scale_color_manual(values = 'red')

    suppressMessages({
      plotly::ggplotly(p.hist.dist)
    })

  })

  # dur ----

  plot_durations_future <- function(){
    durations <-
      GTFSwizard::get_durations(gtfs, method = 'by.route')
    p.hist.dur <-
      ggplot2::ggplot() +
      ggplot2::geom_histogram(data = durations, ggplot2::aes(x = average.duration, weight = (trips * pattern_frequency))) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = weighted.mean(durations$average.duration, durations$pattern_frequency, na.rm = TRUE), color = paste('Overall\nAverage\nDuration\n', weighted.mean(durations$average.duration, durations$pattern_frequency, na.rm = TRUE) %>% round, 'seconds\n')), linetype = 'dashed', linewidth = .75) +
      ggplot2::labs(title = 'Duration Distribution (for all dates)', x = 'Duration (s)', y = 'Frequency (# trips.days)', colour = '') +
      hrbrthemes::scale_x_comma(big.mark = " ") +
      hrbrthemes::scale_y_comma(big.mark = " ") +
      hrbrthemes::theme_ipsum() +
      ggplot2::theme(
        axis.ticks.x = element_blank()
      ) +
      ggplot2::scale_color_manual(values = 'red')
  }

  observeEvent(input$exec_plots,{
    output$hist.dur <- plotly::renderPlotly({

      future_promise({
        plot_durations_future()
      }) %...>%
        ggplotly() %...>%
        suppressMessages()

    })
  })



  # calendar ----
  output$p.calendar <- shiny::renderPlot({

    suppressMessages({
      GTFSwizard::plot_calendar(gtfs, facet_by_year = TRUE)
    })

  })

  # BY ROOOOOOUTE -------
  # map by route ----
  gtfs.filtered <- shiny::reactive({
    GTFSwizard::filter_route(gtfs, route = input$selected.routes)
  })

  gtfs.filtered.trips.shp <- shiny::reactive({
    tidytransit::shapes_as_sf(gtfs.filtered() %>% .$shapes)
  })

  gtfs.filtered.stops.shp <- shiny::reactive({
    tidytransit::gtfs_as_sf(gtfs.filtered()) %>%
      .$stops %>%
      dplyr::left_join(
        gtfs.filtered() %>%
          .$stop_times %>%
          dplyr::group_by(stop_id) %>%
          dplyr::reframe(`# trips` = n())
      )
  })

  output$byroute_map1 <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(group = "OSM") %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,group = 'Carto-Light') %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter, group = 'Carto - Dark') %>%
      leaflet::addLayersControl(baseGroups = c('Carto - Light','Carto - Dark','OSM')) %>%
      leaflet::addPolylines(data = gtfs.filtered.trips.shp()) %>%
      leaflet::addAwesomeMarkers(data = gtfs.filtered.stops.shp(),
                                 popup = ~paste0('# trips ', `# trips`),
                                 clusterOptions = leaflet::markerClusterOptions()
      ) %>%
      leaflet.extras::addFullscreenControl() %>%
      leaflet.extras::addResetMapButton() %>%
      leaflet.extras::addControlGPS() %>%
      leaflet.extras::addSearchOSM()

  })

  # frequency by route ----
  route <- shiny::reactive({input$selected.routes})

  output$freq.sparkline.byroute <- plotly::renderPlotly({plot_routefrequency(gtfs, route())})

  # headway by route ----
  headway.byroute <- shiny::reactive({
    GTFSwizard::get_headways(gtfs.filtered(), method = 'detailed')
  })


}

shiny::shinyApp(ui,server)

#return(shiny::shinyApp(ui, server))


