get_frequency <- function(gtfs){
  freq <-
    gtfs$stop_times %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::reframe(departure = arrival_time[1]) %>% 
    dplyr::left_join(
      gtfs$trips %>% 
        dplyr::select(route_id, service_id, trip_id)
    ) %>% 
    # filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::mutate(hour = str_extract(as.character(departure), '\\d+') %>% as.numeric()) %>% 
    dplyr::group_by(route_id, hour, service_id) %>% 
    dplyr::reframe(frequency = n()) %>% 
    left_join(
      gtfs$dates_services$service_id %>%
        unlist %>%
        table %>%
        as_tibble %>% 
        stats::setNames(c('service_id', 'service_frequency'))
    )
  
  return(freq)
}

get_frequency(gtfs)
