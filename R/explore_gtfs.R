explore_gtfs <- 
  function(gtfs){
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
        width = 12,
        plotly::plotlyOutput('freq.sparkline')
      ),
      shiny::column(
        width = 12,
        plotly::plotlyOutput('dt.sparkline')
      )
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
    output$freq.sparkline <- plotly::renderPlotly({
      overall.freq <-
        get_frequency(gtfs) %>% 
        dplyr::group_by(route_id, hour) %>% 
        dplyr::reframe(average.frequency = weighted.mean(frequency, service_frequency))
      
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
    
    
    # dwell time ----
    output$dt.sparkline <- plotly::renderPlotly({
      
      dwell_time <- 
        get_dwelltime(gtfs)
      
      dt.hline <-
        mean(dwell_time$dwell_time)
      
      p.dt.sparkline <- 
        ggplot2::ggplot() +
        ggplot2::geom_vline(xintercept = c(0, 6, 12, 18, 24), color = 'gray', alpha = .25, linetype = 'dashed') +
        ggplot2::geom_boxplot(data = dwell_time, ggplot2::aes(x = hour, y = dwell_time, color = 'Hourly\nDistribution\n', group = hour)) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = dt.hline, color = 'Overall\nAverage\nDwell Time\n'), linetype = 'dashed', linewidth = .75) +
        ggplot2::geom_line(data = dplyr::group_by(dwell_time, hour) %>% dplyr::reframe(dwell_time = mean(dwell_time)), ggplot2::aes(hour, dwell_time, color = 'Hourly\nAverage\nDwell Time\n'), linewidth = 1) +
        ggplot2::labs(x = 'Hour of the day', y = 'Hourly dwell time', colour = '', title = 'System Dwell Time') +
        ggplot2::theme_linedraw() +
        ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
        ggplot2::theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        scale_color_manual(values = c('blue4', 'gray', 'red'))
      
      plotly::ggplotly(p.dt.sparkline)
      
    })
    
    
  }

  return(shiny::shinyApp(ui, server))
  
}

# explore_gtfsgtfs)

