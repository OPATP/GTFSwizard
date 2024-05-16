get_frequency <- function(gtfs){
  freq <-
    gtfs$stop_times %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::reframe(departure = arrival_time[1]) %>% 
    dplyr::left_join(
      gtfs$trips %>% 
        dplyr::select(route_id, service_id, trip_id, shape_id)
    ) %>% 
    # filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::mutate(hour = str_extract(as.character(departure), '\\d+') %>% as.numeric()) %>% 
    dplyr::group_by(route_id, hour) %>% 
    dplyr::reframe(frequency = n())
  
  return(freq)
}

#get_frequency(gtfs)
