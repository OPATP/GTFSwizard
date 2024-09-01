merge_gtfs <- function(gtfs.x, gtfs.y){
  
  # checa a classe ----
  if(!"wizardgtfs" %in% class(gtfs.x)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThe first gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  if(!"wizardgtfs" %in% class(gtfs.y)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThe first gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  # ajusta os id's para evitar duplicados ----
  gtfs.x$agency$agency_id <- paste0(gtfs.x$agency$agency_id, '.x')
  gtfs.y$agency$agency_id <- paste0(gtfs.y$agency$agency_id, '.y')
  
  gtfs.x$routes$agency_id <- paste0(gtfs.x$routes$agency_id, '.x')
  gtfs.y$routes$agency_id <- paste0(gtfs.y$routes$agency_id, '.y')
  
  gtfs.x$routes$route_id <- paste0(gtfs.x$routes$route_id, '.x')
  gtfs.y$routes$route_id <- paste0(gtfs.y$routes$route_id, '.y')
  
  gtfs.x$trips$route_id <- paste0(gtfs.x$trips$route_id, '.x')
  gtfs.y$trips$route_id <- paste0(gtfs.y$trips$route_id, '.y')
  
  gtfs.x$trips$trip_id <- paste0(gtfs.x$trips$trip_id, '.x')
  gtfs.y$trips$trip_id <- paste0(gtfs.y$trips$trip_id, '.y')
  
  gtfs.x$trips$shape_id <- paste0(gtfs.x$trips$shape_id, '.x')
  gtfs.y$trips$shape_id <- paste0(gtfs.y$trips$shape_id, '.y')
  
  gtfs.x$trips$service_id <- paste0(gtfs.x$trips$service_id, '.x')
  gtfs.y$trips$service_id <- paste0(gtfs.y$trips$service_id, '.y')
  
  gtfs.x$stop_times$trip_id <- paste0(gtfs.x$stop_times$trip_id, '.x')
  gtfs.y$stop_times$trip_id <- paste0(gtfs.y$stop_times$trip_id, '.y')
  
  gtfs.x$stop_times$stop_id <- paste0(gtfs.x$stop_times$stop_id, '.x')
  gtfs.y$stop_times$stop_id <- paste0(gtfs.y$stop_times$stop_id, '.y')
  
  gtfs.x$stops$stop_id <- paste0(gtfs.x$stops$stop_id, '.x')
  gtfs.y$stops$stop_id <- paste0(gtfs.y$stops$stop_id, '.y')
  
  if(!is_null(gtfs.x$fare_rules$route_id)){
    gtfs.x$fare_rules$route_id <- paste0(gtfs.x$fare_rules$route_id, '.x')
    gtfs.x$fare_rules$fare_id <- paste0(gtfs.x$fare_rules$fare_id, '.x')
  }
  
  if(!is_null(gtfs.y$fare_rules$route_id)){
    gtfs.y$fare_rules$route_id <- paste0(gtfs.y$fare_rules$route_id, '.y')
    gtfs.y$fare_rules$fare_id <- paste0(gtfs.y$fare_rules$fare_id, '.y')
  }
  
  if(!is_null(gtfs.x$fare_attributes)){
    gtfs.x$fare_attributes$fare_id <- paste0(gtfs.x$fare_attributes$fare_id, '.x')
  }
  
  if(!is_null(gtfs.y$fare_attributes)){
    gtfs.y$fare_attributes$fare_id <- paste0(gtfs.y$fare_attributes$fare_id, '.y')
  }
  
  if(!is_null(gtfs.x$shapes)){
    gtfs.x$shapes$shape_id <- paste0(gtfs.x$shapes$shape_id, '.x')
  }  
  
  if(!is_null(gtfs.y$shapes)){
    gtfs.y$shapes$shape_id <- paste0(gtfs.y$shapes$shape_id, '.y')
  }
  
  if(!is_null(gtfs.x$calendar)){
    gtfs.x$calendar$service_id <- paste0(gtfs.x$calendar$service_id, '.x')
  }
  
  if(!is_null(gtfs.y$calendar)){
    gtfs.y$calendar$service_id <- paste0(gtfs.y$calendar$service_id, '.y')
  }
  
  if(!is_null(gtfs.x$calendar_dates)){
    gtfs.x$calendar_dates$service_id <- paste0(gtfs.x$calendar_dates$service_id, '.x')
  }
  
  if(!is_null(gtfs.y$calendar_dates)){
    gtfs.y$calendar_dates$service_id <- paste0(gtfs.y$calendar_dates$service_id, '.y')
  }  
  
  if(!is_null(gtfs.x$frequencies)){
    gtfs.x$frequencies$trip_id <- paste0(gtfs.x$frequencies$trip_id, '.x')
  }
  
  if(!is_null(gtfs.y$frequencies)){
    gtfs.y$frequencies$trip_id <- paste0(gtfs.y$frequencies$trip_id, '.y')
  }
  
  if(!is_null(gtfs.x$transfers)){
    gtfs.x$transfers$trip_id <- paste0(gtfs.x$transfers$trip_id, '.x')
    gtfs.x$transfers$stop_id <- paste0(gtfs.x$transfers$stop_id, '.x')
  }
  
  if(!is_null(gtfs.y$transfers)){
    gtfs.y$transfers$trip_id <- paste0(gtfs.y$transfers$trip_id, '.y')
    gtfs.y$transfers$stop_id <- paste0(gtfs.y$transfers$stop_id, '.y')
  }
  
  # bind rows ----
  gtfs <- list()
  
  gtfs$agency <- bind_rows(gtfs.x$agency, gtfs.y$agency)
  
  gtfs$routes <- bind_rows(gtfs.x$routes, gtfs.y$routes)
  
  gtfs$trips <- bind_rows(gtfs.x$trips, gtfs.y$trips)
  
  gtfs$stop_times <- bind_rows(gtfs.x$stop_times, gtfs.y$stop_times)
  
  gtfs$stops <- bind_rows(gtfs.x$stops, gtfs.y$stops)
  
  gtfs$fare_attributes <- bind_rows(gtfs.x$fare_attributes, gtfs.y$fare_attributes)
  
  gtfs$fare_rules <- bind_rows(gtfs.x$fare_rules, gtfs.y$fare_rules)
  
  gtfs$shapes <- bind_rows(gtfs.x$shapes, gtfs.y$shapes)
  
  gtfs$calendar <- bind_rows(gtfs.x$calendar, gtfs.y$calendar)
  
  gtfs$calendar_dates <- bind_rows(gtfs.x$calendar_dates, gtfs.y$calendar_dates)
  
  gtfs$frequencies <- bind_rows(gtfs.x$frequencies, gtfs.y$frequencies)
  
  gtfs$transfers <- bind_rows(gtfs.x$transfers, gtfs.y$transfers)
  
  gtfs$feed_info <- bind_rows(gtfs.x$feed_info, gtfs.y$feed_info)
  
  # convertendo para 'wizardgts' ----
  gtfs <- as_wizardgtfs(gtfs)
  
  # retornando gtfs ----
  return(gtfs)
  
}