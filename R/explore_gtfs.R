#library(bslib)
#library(GTFSwizard)
explore_gtfs <- 
#explore_gtfs_dev <-
  function(gtfs){
  # ui ----
  ui <- shiny::navbarPage(
    #theme = bs_theme(bootswatch = 'cosmo'),
    title = "GTFS wizard",
    #img(height = 150, src = 'logobolero.jpg'),
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
    
    trips.shp <-
      trips.shp %>% 
      dplyr::bind_cols(.,
                       `# trips` = sf::st_intersects(trips.shp, trips.shp) %>% 
                         lapply(., length) %>% 
                         unlist()
      )
    
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
        gtfs$stop_times %>% 
        dplyr::group_by(trip_id) %>% 
        dplyr::reframe(departure = arrival_time[1]) %>% 
        dplyr::left_join(
          gtfs$trips %>% 
            dplyr::select(route_id, service_id, trip_id, shape_id)
        ) %>% 
        # filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
        dplyr::mutate(hour = str_extract(departure, '\\d+') %>% as.numeric()) %>% 
        #group_by(route_id, hour, shape_id, service_id) %>% 
        dplyr::group_by(route_id, hour) %>% 
        dplyr::reframe(frequency = n()) %>% 
        dplyr::arrange(hour) %>%  
        tidyr::pivot_wider(., names_from = hour, values_from = frequency, values_fill = 0) %>% 
        dplyr::arrange(route_id) %>%
        tidyr::pivot_longer(., cols = 2:ncol(.), names_to = "hour", values_to = "frequency") %>% 
        dplyr::mutate(hour = as.numeric(hour))
      
      freq.hline <-
        mean(dplyr::group_by(overall.freq, hour) %>%
               dplyr::reframe(frequency = mean(frequency)) %>%
               .$frequency)
      
      p.freq.sparkline <- 
        ggplot2::ggplot() +
        ggplot2::geom_vline(xintercept = c(0, 6, 12, 18, 24), color = 'gray', alpha = .25, linetype = 'dashed') +
        ggplot2::geom_boxplot(data = overall.freq, ggplot2::aes(x = hour, y = frequency, color = 'Hourly\nDistribution\n', group = hour), fill = NA) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = freq.hline, color = 'Overall\nAverage\nFrequency\n'), linetype = 'dashed', linewidth = .75) +
        ggplot2::geom_line(data = dplyr::group_by(overall.freq, hour) %>% dplyr::reframe(frequency = mean(frequency)), ggplot2::aes(hour, frequency, color = 'Hourly\nAverage\nFrequency\n'), linewidth = 1) +
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
    
    
  }

  return(shiny::shinyApp(ui, server))
  
}

#explore_gtfs(gtfs)

