

.onLoad <- function(libname,pkgname){
  invisible(
    suppressMessages(
      suppressWarnings(
        
        lapply(
          c('sf', 'data.table', 'shiny', 'plotly', 'leaflet', 'leaflet.extras','tidyverse', 'hrbrthemes'),
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
