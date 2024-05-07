explore_gtfs <- function(gtfs){
  
  ui <- shiny::navbarPage(
    title = "GTFS wizard",
    tabPanel(
      'Overview',
      column(
        width = 7,
        leafletOutput('overview_map',height = '75vh'),
        tags$style(
          'div#overview_map{
          width:100%;
          heigth:60vh;
          border:solid orange;
          border-radius:10px;
          }'
        )
        
      ),
      column(
        width = 5,
        plotlyOutput('freq.sparkline')
      )
    ),
    tabPanel('By Route')
  )
  
  server <- function(input, output, session) {
    
    trips.shp <- 
      tidytransit::shapes_as_sf(gtfs$shapes)
    
    trips.shp <-
      trips.shp %>% 
      dplyr::bind_cols(.,
                       `# trips` = st_intersects(trips.shp, trips.shp) %>% 
                         lapply(., length) %>% 
                         unlist()
      )
    
    output$overview_map <- renderLeaflet({
      leaflet() %>%
        addPolylines(data = trips.shp) %>%
        addTiles()
    })
    
    output$freq.sparkline <- renderPlotly({
      p <- 
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
        scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
        theme(legend.position = 'none',
              axis.title = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.ticks = element_blank()
        )
      
      ggplotly(p, height = 50 * length(unique(gtfs$routes$route_id)))
      
    })
    
  }
  
  return(shiny::shinyApp(ui, server))
  
}

#explore_gtfs(gtfs)
