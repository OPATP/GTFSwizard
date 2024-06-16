# corrigir os by dos left_join

explore_gtfs <- 
  function(gtfs){
    
    if(!"wizardgtfs" %in% class(gtfs)){
      
      gtfs <- GTFSwizard::gtfs_to_wizard(gtfs)
      
      warning('\nThis gtfs object is not of wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
      
    }
    
    if(is_null(gtfs$shapes)){
      
      gtfs <- GTFSwizard::get_shapes(gtfs)
      
      warning('\nThis gtfs object does not contain a shapes table.\nUsing get_shapes().')
    }
    
    # ui ----
    ui <- shiny::navbarPage(
      #theme = bs_theme(bootswatch = 'cosmo'),
      title = "GTFSwizard::explore_gtfs()",
      # overview ----
      shiny::tabPanel('Overview',
                      shiny::fluidRow(
                        shiny::column(
                          width = 7,
                          leaflet::leafletOutput('overview_map1', height = '50vh'),
                          shiny::tags$style(
                            'div#overview_map1{
          width:100%;
          heigth:50vh;
          border:solid green;
          border-radius:10px;
          }')
                        ),
                        shiny::column(
                          width = 5,
                          shiny::tableOutput('agency_table'),
                        ),
                      ),
                      shiny::hr(),
                      shiny::fluidRow(
                        shiny::column(
                          width = 8,
                          plotly::plotlyOutput('freq.sparkline', height = '350px')
                        ),
                        shiny::column(
                          width = 4,
                          plotly::plotlyOutput('fleet.sparkline', height = '350px')
                        )
                      ),
                      shiny::hr(),
                      shiny::fluidRow(
                        shiny::column(
                          width = 4,
                          plotly::plotlyOutput('hist.speed', height = '300px')
                        ),
                        shiny::column(
                          width = 4,
                          plotly::plotlyOutput('hist.hw', height = '300px')
                        ),
                        shiny::column(
                          width = 4,
                          plotly::plotlyOutput('hist.dt', height = '300px')
                        )
                      ),
                      shiny::hr(),
                      shiny::fluidRow(
                        shiny::column(
                          width = 6,
                          plotly::plotlyOutput('hist.dist', height = '300px')
                        ),
                        shiny::column(
                          width = 6,
                          plotly::plotlyOutput('hist.dur', height = '300px')
                        )
                      ),
                      shiny::hr(),
                      shiny::fluidRow(
                        shiny::column(shiny::plotOutput('p.calendar',
                                                        height = paste0(as.numeric(max(lubridate::year(gtfs$dates_services$date)) - as.numeric(min(lubridate::year(gtfs$dates_services$date)))  + 5) * 75, "px")
                        ),
                        width = 12)
                      ),
                      shiny::hr()
      ),
      # BY ROUTE ----
      shiny::tabPanel('By Route',
                      fluidRow(
                        shiny::selectizeInput(inputId = 'selected.routes',
                                              label = 'Choose routes of interest:',
                                              choices = sort(unique(gtfs$routes$route_id)),
                                              multiple = T)
                      ),
                      shiny::hr(),
                      fluidRow(
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
                                               height = '350px')),
                          shiny::fluidRow(plotly::plotlyOutput('headway.byroute.sparkline',
                                               #shiny::plotOutput('freq.sparkline.byroute',
                                               height = '350px'))
                          )
                      ),
                      shiny::hr()
      ),
    )
    
    # server ----
    server <- function(input, output, session) {
      
      # agency ----
      agency <-
        gtfs$agency %>% 
        dplyr::filter(agency_id %in% c(gtfs$routes$agency_id %>% unique)) %>% 
        t %>% 
        data.frame %>% 
        tibble::rownames_to_column() %>% 
        stats::setNames(c('', ''))
      
      output$agency_table <-
        renderTable({agency})
      
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
      overall.freq <-
        GTFSwizard::get_frequency(gtfs, method = 'detailed') %>% 
        mutate(hour = as.numeric(hour))
      
      output$freq.sparkline <- plotly::renderPlotly({
        
        hline <-
          weighted.mean(overall.freq$frequency, overall.freq$pattern_frequency, na.rm = T)
        
        p.freq.sparkline <- 
          ggplot2::ggplot() +
          ggplot2::geom_vline(xintercept = c(0, 6, 12, 18, 24), color = 'gray', alpha = .25, linetype = 'dashed') +
          ggplot2::geom_violin(data = overall.freq, ggplot2::aes(x = hour, y = frequency, color = 'Hourly\nDistribution\n', group = hour, weight = pattern_frequency), fill = 'grey', alpha = .65, scale = 'width') +
          ggplot2::geom_hline(ggplot2::aes(yintercept = hline, color = 'Overall\nAverage\nFrequency\n'), linetype = 'dashed', linewidth = .75) +
          ggplot2::geom_line(data = dplyr::group_by(overall.freq, hour) %>% dplyr::reframe(frequency = weighted.mean(frequency, pattern_frequency)), ggplot2::aes(hour, frequency, color = 'Hourly\nAverage\nFrequency\n', group = NA), linewidth = 1) +
          ggplot2::labs(x = 'Hour of the day', y = 'Hourly Frequency', colour = '', title = 'System Frequency') +
          hrbrthemes::theme_ipsum() +
          ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
          ggplot2::theme(
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.ticks.x = element_blank()
          ) +
          ggplot2::scale_color_manual(values = c('blue4', 'white', 'red'))
        
        suppressWarnings({
          plotly::ggplotly(p.freq.sparkline)
        })
        
      })
      
      # fleet ----
      fleet <-
        GTFSwizard::get_fleet(gtfs, method = 'by.hour') %>% 
        mutate(hour = as.numeric(hour))
      
      output$fleet.sparkline <- plotly::renderPlotly({
        
        fleet.hline <-
          weighted.mean(fleet$fleet, fleet$pattern_frequency, na.rm = T)
        
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
        
        suppressWarnings({
          plotly::ggplotly(p.fleet.sparkline,
                           tooltip = c('x', 'y', 'color'))
        })
        
      })
      
      # headway ----    
      hw <- 
        GTFSwizard::get_headways(gtfs, method = 'by.route')
      
      output$hist.hw <- plotly::renderPlotly({
        
        p.hist.hw <-
          ggplot2::ggplot() +
          ggplot2::geom_histogram(data = hw, ggplot2::aes(x = average.headway, weight = trips * pattern_frequency)) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = weighted.mean(hw$average.headway, hw$pattern_frequency, na.rm = T), color = paste('Overall\nAverage\nHeadway\n', weighted.mean(hw$average.headway, hw$pattern_frequency, na.rm = T) %>% round, 'seconds\n')), linetype = 'dashed', linewidth = .75) +
          ggplot2::labs(title = 'Headway Distribution (for all dates)', x = 'Headway (s)', y = 'Frequency (# arrival)', colour = '') +
          hrbrthemes::scale_x_comma(big.mark = " ") +
          hrbrthemes::scale_y_comma(big.mark = " ") +
          hrbrthemes::theme_ipsum() +
          ggplot2::scale_color_manual(values = 'red') +
          ggplot2::theme(
            axis.ticks.x = element_blank()
          )
        
        plotly::ggplotly(p.hist.hw)
        
      })
      
      # speed ----
      speed <-
        GTFSwizard::get_speeds(gtfs, method = 'by.route')
      
      output$hist.speed <- plotly::renderPlotly({
        
        p.hist.speed <-
          ggplot2::ggplot() +
          ggplot2::geom_histogram(data = speed, ggplot2::aes(x = average.speed, weight = trips * pattern_frequency)) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = mean(speed$average.speed, na.rm = T), color = paste('Overall\naverage\nhourly\nSpeed of\n', mean(speed$average.speed, na.rm = T) %>% round, 'km/h')), linetype = 'dashed', linewidth = .75) +
          ggplot2::labs(title = 'Speeds Distribution (for all dates)', x = 'Speed (km/h)', y = 'Frequency (# route)', colour = '') +
          hrbrthemes::scale_x_comma(big.mark = " ") +
          hrbrthemes::scale_y_comma(big.mark = " ") +
          hrbrthemes::theme_ipsum() +
          ggplot2::theme(
            axis.ticks.x = element_blank()
          ) +
          ggplot2::scale_color_manual(values = 'red')
        
        plotly::ggplotly(p.hist.speed)
        
      })
      
      # dwell time ----
      dwell_time <- 
        GTFSwizard::get_dwelltimes(gtfs, method = 'by.hour')
      
      output$hist.dt <- plotly::renderPlotly({
        
        p.hist.dt <-
          ggplot2::ggplot() +
          ggplot2::geom_histogram(data = dwell_time, ggplot2::aes(x = average.dwelltime, weight = (trips * pattern_frequency))) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = weighted.mean(dwell_time$average.dwelltime, dwell_time$pattern_frequency, na.rm = T), color = paste('Overall\nAverage\nDwell Time\n', weighted.mean(dwell_time$average.dwelltime, dwell_time$pattern_frequency, na.rm = T) %>% round, 'seconds\n')), linetype = 'dashed', linewidth = .75) +
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
          ggplot2::geom_vline(ggplot2::aes(xintercept = weighted.mean(distances$average.distance, distances$pattern_frequency, na.rm = T), color = paste('Overall\nAverage\nDistance\n', weighted.mean(distances$average.distance, distances$pattern_frequency, na.rm = T) %>% round, 'm\n')), linetype = 'dashed', linewidth = .75) +
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
      durations <- 
        GTFSwizard::get_durations(gtfs, method = 'by.route')
      
      output$hist.dur <- plotly::renderPlotly({
        
        p.hist.dur <-
          ggplot2::ggplot() +
          ggplot2::geom_histogram(data = durations, ggplot2::aes(x = average.duration, weight = (trips * pattern_frequency))) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = weighted.mean(durations$average.duration, durations$pattern_frequency, na.rm = T), color = paste('Overall\nAverage\nDuration\n', weighted.mean(durations$average.duration, durations$pattern_frequency, na.rm = T) %>% round, 'seconds\n')), linetype = 'dashed', linewidth = .75) +
          ggplot2::labs(title = 'Duration Distribution (for all dates)', x = 'Duration (s)', y = 'Frequency (# trips.days)', colour = '') +
          hrbrthemes::scale_x_comma(big.mark = " ") +
          hrbrthemes::scale_y_comma(big.mark = " ") +
          hrbrthemes::theme_ipsum() +
          ggplot2::theme(
            axis.ticks.x = element_blank()
          ) +
          ggplot2::scale_color_manual(values = 'red')
        
        suppressMessages({
          plotly::ggplotly(p.hist.dur)
        })
        
      })
      
      # calendar ----
      output$p.calendar <- shiny::renderPlot({
        
        suppressMessages({
          GTFSwizard::get_calendar(gtfs, facet_by_year = T)
        })
        
      })
      
      # BY ROOOOOOUTE -------
      # map by route ----
      gtfs.filtered <- reactive({
        GTFSwizard::filter_route(gtfs, route = c(input$selected.routes))
      })
      
      gtfs.filtered.trips.shp <- reactive({
        tidytransit::shapes_as_sf(gtfs.filtered() %>% .$shapes)
      })
      
      gtfs.filtered.stops.shp <- reactive({
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
      overall.freq.byroute <- reactive({
        GTFSwizard::get_frequency(gtfs.filtered(), method = 'detailed') %>% 
          mutate(hour = as.numeric(hour))
      })
      
      output$freq.sparkline.byroute <- plotly::renderPlotly({
        overall.freq <- overall.freq.byroute()
        
        hline <-
          weighted.mean(overall.freq$frequency, overall.freq$pattern_frequency, na.rm = T)
        
        p.freq.sparkline <- 
          ggplot2::ggplot() +
          ggplot2::geom_vline(xintercept = c(0, 6, 12, 18, 24), color = 'gray', alpha = .25, linetype = 'dashed') +
          ggplot2::geom_violin(data = overall.freq, ggplot2::aes(x = hour, y = frequency, color = 'Hourly\nDistribution\n', group = hour, weight = pattern_frequency), fill = 'grey', alpha = .65, scale = 'width') +
          ggplot2::geom_hline(ggplot2::aes(yintercept = hline, color = 'Overall\nAverage\nFrequency\n'), linetype = 'dashed', linewidth = .75) +
          ggplot2::geom_line(data = dplyr::group_by(overall.freq, hour) %>% dplyr::reframe(frequency = weighted.mean(frequency, pattern_frequency)), ggplot2::aes(hour, frequency, color = 'Hourly\nAverage\nFrequency\n', group = NA), linewidth = 1) +
          ggplot2::labs(x = 'Hour of the day', y = 'Hourly Frequency', colour = '', title = 'Route(s) Frequency') +
          hrbrthemes::theme_ipsum() +
          ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
          ggplot2::theme(
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.ticks.x = element_blank()
          ) +
          ggplot2::scale_color_manual(values = c('blue4', 'white', 'red'))
        
        suppressWarnings({
          plotly::ggplotly(p.freq.sparkline)
        })
        
      })
      
      # headway by route ----    
      headway.byroute <- reactive({
        GTFSwizard::get_headways(gtfs.filtered(), method = 'detailed')
      })
      
      output$headway.byroute.sparkline <- plotly::renderPlotly({
        headway.byroute <- headway.byroute()
        
        hline <-
          weighted.mean(headway.byroute$headway, headway.byroute$pattern_frequency, na.rm = T)
        
        p.headway.byroute.sparkline <- 
          ggplot2::ggplot() +
          ggplot2::geom_vline(xintercept = c(0, 6, 12, 18, 24), color = 'gray', alpha = .25, linetype = 'dashed') +
          ggplot2::geom_violin(data = headway.byroute, ggplot2::aes(x = hour, y = headway, color = 'Hourly\nDistribution\n', group = hour, weight = pattern_frequency), fill = 'grey', alpha = .65, scale = 'width') +
          ggplot2::geom_hline(ggplot2::aes(yintercept = hline, color = 'Overall\nAverage\nHeadway\n'), linetype = 'dashed', linewidth = .75) +
          ggplot2::geom_line(data = dplyr::group_by(headway.byroute, hour) %>% dplyr::reframe(headway = weighted.mean(headway, pattern_frequency)), ggplot2::aes(hour, headway, color = 'Hourly\nAverage\nHeadway\n', group = NA), linewidth = 1) +
          ggplot2::labs(x = 'Hour of the day', y = 'Headway', colour = '', title = 'Route(s) Headway') +
          hrbrthemes::theme_ipsum() +
          ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
          ggplot2::theme(
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.ticks.x = element_blank()
          ) +
          ggplot2::scale_color_manual(values = c('blue4', 'white', 'red'))
        
        suppressWarnings({
          plotly::ggplotly(p.headway.byroute.sparkline)
        })
        
      })
      
      
    }
    
    return(shiny::shinyApp(ui, server))
    
  }


explore_gtfs(gtfs)
