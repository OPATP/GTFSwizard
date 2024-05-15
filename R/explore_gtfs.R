

explore_gtfs <- function(gtfs){
  
  ui <- shiny::navbarPage(
    title = "Explore GTFS",
    shiny::tabPanel(
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
        plotlyOutput('overview_plot_freq')
      )
    ),
    shiny::tabPanel('By Route')
  )
  
  server <- function(input, output, session) {
    
    output$overview_map <- renderLeaflet({
      leaflet() %>% 
        addPolylines(data = geom_shapes(gtfs$shapes)) %>% 
        addTiles()
    })
    
    
  }
  
  return(shiny::shinyApp(ui, server))
  
  
}

exploreGTFS(gtfs_list)



