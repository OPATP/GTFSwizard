split_trip <- function(gtfs, trip, method = 'equal.size', split = 1){

  # checa os argumentos -------------------------------------------------------------------------
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThe first gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  checkmate::assert_int(split)
  
  checkmate::assert_subset(trip, choices = gtfs$trips$trip_id)
  
  checkmate::assert_choice(method, choices = c('equal.size'))
  
  # identifica trips -------------------------------------------------------------------------
  groups <- (split + 1)
  
  split_data <- 
    gtfs$stop_times %>% 
    mutate(split = trip_id %in% trip) %>% 
    group_by(trip_id) %>% 
    mutate(subtrip = if_else(split == T, ceiling(1:n()/n() * groups), NA),
           dupe = split == T & !subtrip == lead(subtrip)) %>% 
    ungroup() %>% 
    bind_rows(slice(., .$dupe %>% which()) %>% mutate(subtrip = subtrip + 1))
  
  trip.dic <- 
    split_data %>% 
    select(trip_id, subtrip) %>% 
    na.omit() %>% 
    setNames(c('trip_id', 'new.trip_id')) %>% 
    distinct() %>% 
    mutate(new.trip_id = paste0(trip_id, '.', LETTERS[new.trip_id]))
  
  # stop times ----------------------------------------------------------------------------------
  gtfs$stop_times <- 
    split_data %>% 
    select(-dupe) %>% 
    mutate(trip_id = paste0(trip_id, '.', LETTERS[subtrip])) %>% 
    group_by(trip_id) %>% 
    select(-subtrip)
  
  if (!is_null(gtfs$stop_times$shape_dist_traveled)) {
    
    gtfs$stop_times <-
      gtfs$stop_times %>% 
      mutate(shape_dist_traveled = if_else(split, shape_dist_traveled - shape_dist_traveled[1], shape_dist_traveled))
  
      }
  
  gtfs$stop_times <- select(gtfs$stop_times, -split) %>% 
    ungroup

  # trips --------------------------------------------------------------------------------------
  gtfs$trips <- 
    left_join(gtfs$trips, trip.dic, by = 'trip_id') %>% 
    mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id)) 
  
  routes_dic <-  # verificar se precisa disso tudo
    gtfs$trips %>% 
    select(trip_id, new.trip_id, route_id) %>% 
    na.omit()
  
  routes <- unique(routes_dic$route_id)
  
  gtfs$trips <- 
    gtfs$trips %>% 
    select(-new.trip_id)

  # frequencies ---------------------------------------------------------------------------------
  if (!is_null(gtfs$frequencies)) {
    
    gtfs$frequencies <- 
      left_join(gtfs$frequencies, trip.dic) %>% 
      mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id))%>% 
      select(-new.trip_id)
    
  }
  
  # transfers -----------------------------------------------------------------------------------
  if (!is_null(gtfs$transfers)) {
    
    gtfs$transfers <- 
      left_join(gtfs$transfers, trip.dic) %>% 
      mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id)) %>% 
      select(-new.trip_id)
    
  }  
  
  # corrigindo shapes ---------------------------------------------------------------------------
  gtfs.x <- 
    filter_trip(gtfs, trip.dic$new.trip_id, keep = FALSE)
  
  gtfs.y <- 
    filter_trip(gtfs, trip.dic$new.trip_id, keep = TRUE) %>% 
    get_shapes()
  
  gtfs <- merge(gtfs.x, gtfs.y, suffix = FALSE)

  # retornando gtfs -----------------------------------------------------------------------------
  return(gtfs)
  
}
