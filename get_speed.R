get_speed <- function(gtfs, detailed.){
  
  if(!"wizardgtfs" %in% class(gtfs))(gtfs <- GTFSwizard::gtfs_to_wizard(gtfs))
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  service_pattern_freq <- 
    gtfs$dates_services$service_id %>% 
    unlist %>%
    tibble(service_id = .) %>% 
    dplyr::left_join(service_pattern) %>% 
    dplyr::group_by(service_pattern) %>% 
    dplyr::reframe(pattern_frequency= n())
  
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
    dplyr::left_join(distance.matrix, by = join_by(from_stop_id, to_stop_id)) %>% 
    dplyr::mutate(speed = (distance/1000) / (duration/3600)) %>% 
    filter(speed > 0) %>% 
    dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
    dplyr::left_join(service_pattern, by = 'service_id') %>%
    dplyr::select(route_id, hour, service_pattern, from_stop_id, to_stop_id, duration, distance, speed) %>% 
    dplyr::mutate(route_id = forcats::as_factor(route_id),
                  hour = as.numeric(hour)) %>% 
    dplyr::left_join(service_pattern_freq,
                     by = 'service_pattern') %>%
    select(route_id, from_stop_id, to_stop_id, hour, duration, distance, speed, service_pattern, pattern_frequency)
  
  return(speed)
}