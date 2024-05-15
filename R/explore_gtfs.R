

explore_gtfs <- function(gtfs){
  
  ui <- shiny::navbarPage(
    title = "Explore GTFS",
    shiny::tabPanel(
      'Overview',
      column(
        width = 7,
        leaflet::leafletOutput('overview_map',height = '75vh'),
        shiny::tags$style(
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
        plotly::plotlyOutput('overview_plot_freq')
      )
    ),
    shiny::tabPanel('By Route')
  )
  
  server <- function(input, output, session) {
    
    output$overview_map <- renderLeaflet({
      leaflet::leaflet() %>% 
        leaflet::addPolylines(data = geom_shapes(gtfs$shapes)) %>% 
        leaflet::addTiles()
    })
    
    
  }
  
  return(shiny::shinyApp(ui, server))
  
  
}





