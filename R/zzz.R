

.onLoad <- function(libname,pkgname){
  invisible(
    suppressMessages(
      suppressWarnings(
        
        lapply(
          c('sf', 'data.table', 'shiny', 'plotly', 'leaflet', 'leaflet.extras','tidyverse', 'hrbrthemes', 'hms'),
          function(x){
            invisible(
              suppressPackageStartupMessages(
                library(x,quietly = TRUE,character.only = TRUE)
              )
            )
          })
        
      )
    )
  )
  
  
}
