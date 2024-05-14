explore_gtfs <-  function(gtfs){
  # ui ----
  ui <- shiny::navbarPage(
    #theme = bs_theme(bootswatch = 'cosmo'),
    title = "GTFS wizard",
    #img(height = 150, src = 'logobolero.jpg'),
    h1('GTFS Exploration Dashboard'),
    hr(),
    tabPanel(
      'Overview',
      tableOutput('agency_table'),
      hr(),
      column(
        width = 7,
        leafletOutput('overview_map1', height = '75vh'),
        tags$style(
          'div#overview_map1{
          width:100%;
          heigth:60vh;
          border:solid red;
          border-radius:10px;
          }'
        ),
        
      ),
      column(
        width = 5,
        plotlyOutput('freq.sparkline')
      ),
      column(
        width = 7,
        hr(),
        plotlyOutput('dwell.time')
      )
    ),
    tabPanel('By Route')
    )
  
  # server ----
  server <- function(input, output, session) {
    
    agency <-
      gtfs %>% .$agency
    
    output$agency_table <- renderTable({agency})
    
    trips.shp <- 
      tidytransit::shapes_as_sf(gtfs$shapes)
    
    trips.shp <-
      trips.shp %>% 
      dplyr::bind_cols(.,
                       `# trips` = st_intersects(trips.shp, trips.shp) %>% 
                         lapply(., length) %>% 
                         unlist()
      )
    
    stops.shp <- 
      tidytransit::gtfs_as_sf(gtfs) %>% 
      .$stops %>% 
      dplyr::left_join(
        gtfs$stop_times %>%
          group_by(stop_id) %>%
          reframe(`# trips` = n())
      )
    
    output$overview_map1 <- renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles(group = "OSM") %>% 
        leaflet::addProviderTiles(providers$CartoDB.Positron,group = 'Carto-Light') %>% 
        leaflet::addProviderTiles(providers$CartoDB.DarkMatter, group = 'Carto - Dark') %>% 
        leaflet::addLayersControl(baseGroups = c('Carto - Light','Carto - Dark','OSM')) %>% 
        leaflet::addPolylines(data = trips.shp) %>%
        leaflet::addAwesomeMarkers(data = stops.shp,
                                   popup = ~paste0('# trips ', `# trips`),
                                   clusterOptions = markerClusterOptions()
        ) %>% 
        leaflet.extras::addFullscreenControl() %>% 
        leaflet.extras::addResetMapButton() %>% 
        leaflet.extras::addControlGPS() %>% 
        leaflet.extras::addSearchOSM()
      
    })
    
    output$freq.sparkline <- renderPlotly({
      p.freq.sparkline <- 
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
        dplyr::mutate(hour = as.numeric(hour)) %>% #.[1:300, ] %>% 
        ggplot() +
        geom_vline(color = 'red', xintercept = c(0, 6, 12, 18, 24)) +
        geom_line(aes(hour, frequency), linewidth = 1) +
        facet_grid(route_id~.,
                   #scales = 'free_y'
        ) +
        theme_linedraw() +
        labs(x = "Time of the day (hour)", y = "Frequency (trips)") +
        scale_x_continuous(breaks = seq(0, 24, 6)) +
        #scale_y_continuous(breaks = seq(0, 20, 3)) +
        theme(legend.position = 'none',
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.ticks = element_blank()
        )
      
      plotly::ggplotly(p.freq.sparkline, height = 50 * length(unique(gtfs$routes$route_id)))
      
    })
    
    dwell_time <- 
      gtfs$stop_times %>% 
      dplyr::select(arrival_time, departure_time, stop_id)  %>% 
      dplyr::mutate(stop_id = as_factor(stop_id)) %>% 
      dplyr::filter(!arrival_time == '' & !departure_time == "") %>% 
      dplyr::mutate(arrival_time = gtfs$stop_times$arrival_time %>% 
                      stringr::str_split(":") %>% 
                      lapply(FUN = as.numeric) %>% 
                      lapply(FUN = function(x){
                        x[1]*60*60+x[2]*60+x[3]
                      }) %>% 
                      unlist() %>% 
                      na.omit(),
                    departure_time = gtfs$stop_times$departure_time %>% 
                      stringr::str_split(":") %>% 
                      lapply(FUN = as.numeric) %>% 
                      lapply(FUN = function(x){
                        x[1]*60*60+x[2]*60+x[3]
                      }) %>% 
                      unlist() %>% 
                      na.omit(),
                    dwell_time = departure_time - arrival_time
      ) %>% 
      dplyr::filter(!dwell_time <= 300 | !dwell_time == 0)
    
    output$dwell.time <- renderPlotly({
      p.dwell.time <- 
        ggplot() +
        geom_jitter(data = dwell_time, aes(x = stop_id, y = dwell_time), alpha = .5) +
        geom_point(data = dwell_time %>% group_by(stop_id) %>% reframe(y = mean(dwell_time)), aes(x = stop_id, y = y, color = 'Average\ndwell time'), size = 2) +
        labs(x = 'Stop id', y = 'Dwell time (s)', colour = "") +
        theme_linedraw() +
        theme(axis.text.x = element_text(angle = 90))
      
      plotly::ggplotly(p.dwell.time)
      
    })
    
  }

  return(shiny::shinyApp(ui, server))
  
}
