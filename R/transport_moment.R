

transport_moment <- function(gtfs, dates = Sys.Date(), routes = NULL, by_route = FALSE, simplify = FALSE){
  UseMethod('transport_moment')
}

transport_moment.list <- function(gtfs, dates = Sys.Date(), routes = NULL, by_route = FALSE, simplify = FALSE){
  gtfs <- tryCatch(gtfs_to_wizard(gtfs),error = function(e){
    stop('Object cannot be converted to wizardgtfs')
  })
  transport_moment.wizardgtfs(gtfs,dates,routes,by_route,simplify)
}
transport_moment.gtfs <- function(gtfs, dates = Sys.Date(), routes = NULL, by_route = FALSE, simplify = FALSE){
  gtfs <- tryCatch(gtfs_to_wizard(gtfs),error = function(e){
    stop('Object cannot be converted to wizardgtfs')
  })
  transport_moment.wizardgtfs(gtfs,dates,routes,by_route,simplify)
}
transport_moment.default <- function(gtfs, dates = Sys.Date(), routes = NULL, by_route = FALSE, simplify = FALSE){
  gtfs <- tryCatch(gtfs_to_wizard(gtfs),error = function(e){
    stop('Object cannot be converted to wizardgtfs')
  })
  transport_moment.wizardgtfs(gtfs,dates,routes,by_route,simplify)
}

transport_moment.wizardgtfs <- function(gtfs, dates = Sys.Date(), routes = NULL, by_route = FALSE, simplify = FALSE){
  
  if(is.null(routes)){
    routes <- gtfs$routes$route_id
  }
  if(is.character(dates)){
    dates = as.POSIXct(dates)
  }
  checkmate::assert_string(routes)
  checkmate::assert_posixct(dates)
  checkmate::assert_logical(by_route, len = 1, any.missing = F)
  checkmate::assert_logical(simplify, len = 1, any.missing = F)
  
  
  
  
  services_dates_routes <- 
    gtfs$dates_services %>% 
    filter(date %in% dates)
  
  services_dates_routes$trips <- lapply(services_dates_routes$service_id,function(x){
    gtfs$trips %>% 
      filter(service_id %in% unlist(x)) %>% 
      group_by(route_id,shape_id) %>% 
      reframe(n_trips = n())
  })
  
  shapes_length <- gtfs$shapes %>% 
    group_by(shape_id) %>% 
    reframe(wkt = paste0(shape_pt_lon,' ',shape_pt_lat)) %>% 
    group_by(shape_id) %>% 
    reframe(wkt = paste0('LINESTRING(',paste0(wkt,collapse = ', '), ')')) %>% 
    st_as_sf(wkt = 'wkt',crs=4326) %>% 
    mutate(length = st_length(.)) %>% 
    st_drop_geometry() %>% as_tibble()
  if(by_route){
    resp <- services_dates_routes[,'date']
    resp$transport_moment <- lapply(
      services_dates_routes$trips,
      function(x){
        left_join(
          x,
          shapes_length,
          by = 'shape_id'
        ) %>% 
          group_by(route_id) %>% 
          reframe(transport_moment = as.numeric(sum(n_trips*length,na.rm = T)))
      }
    )
    if(simplify){
      resp <- unnest(resp,'transport_moment')
    }
    return(resp)
    
  }else{
    tibble(
      date = services_dates_routes$date,
      transport_moment = lapply(
        services_dates_routes$trips,
        function(x){
          left_join(
            x,
            shapes_length,
            by = 'shape_id'
          ) %>% 
            mutate(moment = n_trips*length) %>% 
            .$moment %>% sum(na.rm = T)
        }
      ) %>% unlist()
    ) %>% return()
  }
}


