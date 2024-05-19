explore_gtfs <- 
  function(gtfs){
    if(!"wizardgtfs" %in% class(gtfs))(gtfs <- GTFSwizard::gtfs_to_wizard(gtfs))
    # ui ----
  ui <- shiny::navbarPage(
    #theme = bs_theme(bootswatch = 'cosmo'),
    title = "GTFSwizard",
    shiny::h1('GTFS Exploration Dashboard'),
    shiny::hr(),
    shiny::tabPanel(
      'Overview',
      shiny::tableOutput('agency_table'),
      shiny::hr(),
      shiny::column(
        width = 12,
        leaflet::leafletOutput('overview_map1', height = '75vh'),
        shiny::tags$style(
          'div#overview_map1{
          width:100%;
          heigth:60vh;
          border:solid red;
          border-radius:10px;
          }'
        ),
        shiny::hr()
        
      ),
      shiny::column(
        width = 7,
        plotly::plotlyOutput('freq.sparkline')
      ),
      shiny::column(
        width = 5,
        plotly::plotlyOutput('hist.freq')
      ),
      shiny::column(
        width = 7,
        plotly::plotlyOutput('dt.sparkline')
      ),
      shiny::column(
        width = 5,
        plotly::plotlyOutput('hist.dt')
      )
    ),
    shiny::column(
      width = 7,
      plotly::plotlyOutput('hw.sparkline')
    ),
    shiny::column(
      width = 5,
      plotly::plotlyOutput('hist.hw')
  ),
    shiny::tabPanel('By Route')
    )
  
  # server ----
  server <- function(input, output, session) {
    
    # agency ----
    agency <-
      gtfs$agency %>% 
      dplyr::filter(agency_id %in% c(gtfs$routes$agency_id %>% unique))
    
    output$agency_table <-
      renderTable({agency})
    
    # map ----
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
                                   popup = ~paste0('# trips ', `# trips`),
                                   clusterOptions = leaflet::markerClusterOptions()
        ) %>% 
        leaflet.extras::addFullscreenControl() %>% 
        leaflet.extras::addResetMapButton() %>% 
        leaflet.extras::addControlGPS() %>% 
        leaflet.extras::addSearchOSM()
      
    })
    
    # frequency ----
    overall.freq <-
      get_frequency(gtfs) %>% 
      dplyr::group_by(route_id, hour) %>% 
      dplyr::reframe(average.frequency = weighted.mean(frequency, service_frequency))
    
    output$freq.sparkline <- plotly::renderPlotly({
      
      freq.hline <-
        mean(overall.freq$average.frequency, na.rm = T)
      
      p.freq.sparkline <- 
        ggplot2::ggplot() +
        ggplot2::geom_vline(xintercept = c(0, 6, 12, 18, 24), color = 'gray', alpha = .25, linetype = 'dashed') +
        ggplot2::geom_boxplot(data = overall.freq, ggplot2::aes(x = hour, y = average.frequency, color = 'Hourly\nDistribution\n', group = hour), fill = NA) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = freq.hline, color = 'Overall\nAverage\nFrequency\n'), linetype = 'dashed', linewidth = .75) +
        ggplot2::geom_line(data = dplyr::group_by(overall.freq, hour) %>% dplyr::reframe(frequency = mean(average.frequency)), ggplot2::aes(hour, frequency, color = 'Hourly\nAverage\nFrequency\n'), linewidth = 1) +
        ggplot2::labs(x = 'Hour of the day', y = 'Hourly Frequency', colour = '', title = 'System Frequency') +
        ggplot2::theme_linedraw() +
        ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
        ggplot2::theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        ggplot2::scale_color_manual(values = c('blue4', 'gray', 'red'))
      
      plotly::ggplotly(p.freq.sparkline)
      
    })
    
    output$hist.freq <- plotly::renderPlotly({
      
      p.hist.freq <-
        ggplot2::ggplot() +
        ggplot2::geom_histogram(data = overall.freq, ggplot2::aes(x = average.frequency)) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = mean(overall.freq$average.frequency, na.rm = T), color = paste('Overall\naverage\nhourly\nfrequency of\n', mean(overall.freq$average.frequency, na.rm = T) %>% round, 'departure \nper hour')), linetype = 'dashed', linewidth = .75) +
        ggplot2::labs(title = 'Hourly Frequency Distribution', x = 'Departures', y = 'Frequency (# route.hour)', colour = '') +
        ggplot2::theme_linedraw() +
        ggplot2::scale_color_manual(values = 'red')
      
      
      plotly::ggplotly(p.hist.freq)
      
    })
    
    # dwell time ----
    dwell_time <- 
      get_dwelltime(gtfs) %>% 
      dplyr::group_by(route_id, hour, stop_id) %>% 
      dplyr::reframe(average.dt = weighted.mean(dwell_time, service_frequency, na.rm = T))
    
    output$dt.sparkline <- plotly::renderPlotly({
      
      p.dt.sparkline <- 
        ggplot2::ggplot() +
        ggplot2::geom_vline(xintercept = c(0, 6, 12, 18, 24), color = 'gray', alpha = .25, linetype = 'dashed') +
        ggplot2::geom_boxplot(data = dwell_time, ggplot2::aes(x = hour, y = average.dt, color = 'Hourly\nDistribution\n', group = hour)) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = mean(dwell_time$average.dt, na.rm = T), color = 'Overall\nAverage\nDwell Time\n'), linetype = 'dashed', linewidth = .75) +
        ggplot2::geom_line(data = dplyr::group_by(dwell_time, hour) %>% dplyr::reframe(dwell_time = mean(average.dt, na.rm = T)), ggplot2::aes(hour, dwell_time, color = 'Hourly\nAverage\nDwell Time\n'), linewidth = 1) +
        ggplot2::labs(x = 'Hour of the day', y = 'Hourly dwell time', colour = '', title = 'System Dwell Time') +
        ggplot2::theme_linedraw() +
        ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
        ggplot2::theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        ggplot2::scale_color_manual(values = c('blue4', 'gray', 'red'))
      
      plotly::ggplotly(p.dt.sparkline)
      
    })
    
    output$hist.dt <- plotly::renderPlotly({
      
      p.hist.dt <-
        ggplot2::ggplot() +
        ggplot2::geom_histogram(data = dwell_time, ggplot2::aes(x = average.dt)) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = mean(dwell_time$average.dt, na.rm = T), color = paste('Overall\nAverage\nDwell Time\n', mean(dwell_time$average.dt, na.rm = T) %>% round, 'seconds\n')), linetype = 'dashed', linewidth = .75) +
        ggplot2::labs(title = 'Dwell Time Distribution', x = 'Dwell time (s)', y = 'Frequency (# stops)', colour = '') +
        ggplot2::theme_linedraw() +
        ggplot2::scale_color_manual(values = 'red')
    
    
      plotly::ggplotly(p.hist.dt)
      
      })

    # headway ----    
    hw <- 
      get_headway(gtfs) %>% 
      dplyr::group_by(route_id, hour, stop_id) %>% 
      dplyr::reframe(average.hw = weighted.mean(average.headway.minutes, service_frequency, na.rm = T))
    
    output$hw.sparkline <- plotly::renderPlotly({
      
      p.hw.sparkline <- 
        ggplot2::ggplot() +
        ggplot2::geom_vline(xintercept = c(0, 6, 12, 18, 24), color = 'gray', alpha = .25, linetype = 'dashed') +
        ggplot2::geom_boxplot(data = hw, ggplot2::aes(x = hour, y = average.hw, color = 'Hourly\nDistribution\n', group = hour)) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = mean(hw$average.hw, na.rm = T), color = 'Overall\nAverage\nHeadway\n'), linetype = 'dashed', linewidth = .75) +
        ggplot2::geom_line(data = dplyr::group_by(hw, hour) %>% dplyr::reframe(hw = mean(average.hw, na.rm = T)), ggplot2::aes(hour, hw, color = 'Hourly\nAverage\nHeadway\n'), linewidth = 1) +
        ggplot2::labs(x = 'Hour of the day', y = 'Hourly headway', colour = '', title = 'System Headway') +
        ggplot2::theme_linedraw() +
        ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
        ggplot2::theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        ggplot2::scale_color_manual(values = c('blue4', 'gray', 'red'))
      
      plotly::ggplotly(p.hw.sparkline)
      
    })
    
    output$hist.hw <- plotly::renderPlotly({
      
      p.hist.hw <-
        ggplot2::ggplot() +
        ggplot2::geom_histogram(data = hw, ggplot2::aes(x = average.headway.minutes)) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = mean(hw$average.headway.minutes, na.rm = T), color = paste('Overall\nAverage\nDwell Time\n', mean(hw$average.headway.minutes, na.rm = T) %>% round, 'minutes\n')), linetype = 'dashed', linewidth = .75) +
        ggplot2::labs(title = 'Headway Distribution', x = 'Headway (min)', y = 'Frequency (# arrival)', colour = '') +
        ggplot2::theme_linedraw() +
        ggplot2::scale_color_manual(values = 'red')
      
      
      plotly::ggplotly(p.hist.hw)
      
    })
    
  }

  return(shiny::shinyApp(ui, server))
  
}

#explore_gtfs(gtfs)
