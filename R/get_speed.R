get_speed <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs))(gtfs <- GTFSwizard::gtfs_to_wizard(gtfs))
  
  distance.matrix <- 
    tidytransit::stop_distances(gtfs_stops = gtfs$stops) %>% 
    dplyr::filter(distance != 0)
  
  speed <-
    gtfs$stop_times %>% 
    dplyr::select(trip_id, arrival_time, departure_time, stop_id)  %>% 
    dplyr::filter(!arrival_time == '') %>% 
    dplyr::mutate(stop_id = as_factor(stop_id)) %>% 
    dplyr::mutate(hour = str_extract(arrival_time, "\\d+"),
                  arrival_time = gtfs$stop_times$arrival_time %>% 
                    stringr::str_split(":") %>% 
                    lapply(FUN = as.numeric) %>% 
                    lapply(FUN = function(x){x[1]*60*60+x[2]*60+x[3]}) %>% 
                    unlist() %>% 
                    na.omit(),
                  departure_time = gtfs$stop_times$departure_time %>% 
                    stringr::str_split(":") %>% 
                    lapply(FUN = as.numeric) %>% 
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>% 
                    unlist() %>% 
                    na.omit()
    ) %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::mutate(from_stop_id = stop_id,
                  to_stop_id = lead(stop_id),
                  duration = lead(arrival_time)  - departure_time) %>% 
    ungroup() %>% 
    na.omit() %>% 
    dplyr::left_join(distance.matrix) %>% 
    dplyr::mutate(speed = (distance/1000) / (duration/3600)) %>% 
    dplyr::left_join(gtfs$trips) %>% 
    dplyr::select(route_id, hour, service_id, from_stop_id, to_stop_id, duration, distance, speed) %>% 
    dplyr::mutate(route_id = forcats::as_factor(route_id),
                  hour = as.numeric(hour)) %>% 
    dplyr::left_join(
      gtfs$dates_services$service_id %>%
        unlist %>%
        table %>%
        as_tibble %>% 
        stats::setNames(c('service_id', 'service_frequency'))
    )
  
  return(speed)
}

#get_speed(gtfs)
