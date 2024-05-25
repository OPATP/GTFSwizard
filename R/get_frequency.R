get_frequency <- function(gtfs, simplify = T){
  
  get_frequency_byroute <- function(gtfs){
  
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
  
  freq <-
    gtfs$stop_times %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::reframe(departure = arrival_time[1]) %>% 
    dplyr::left_join(gtfs$trips %>% 
                       dplyr::select(route_id, service_id, trip_id),
                     by = 'trip_id') %>% 
    dplyr::left_join(service_pattern, by = 'service_id') %>% # aqui
    #dplyr::mutate(hour = str_extract(as.character(departure), '\\d+') %>% as.numeric()) %>% 
    dplyr::group_by(route_id, service_pattern) %>% # aqui
    dplyr::reframe(frequency = n()) %>% 
    dplyr::left_join(service_pattern_freq, # aqui
                     by = 'service_pattern') %>%
    #filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::select(route_id, frequency, service_pattern, pattern_frequency)
  
  return(freq)
  
}

  get_frequency_detailed <- function(gtfs){
  
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
  
  freq <-
    gtfs$stop_times %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::reframe(departure = arrival_time[1]) %>% 
    dplyr::left_join(gtfs$trips %>% 
                       dplyr::select(route_id, service_id, trip_id),
                     by = 'trip_id') %>% 
    dplyr::left_join(service_pattern, by = 'service_id') %>% # aqui
    dplyr::mutate(hour = str_extract(as.character(departure), '\\d+') %>% as.numeric()) %>% 
    dplyr::group_by(route_id, hour, service_pattern) %>% # aqui
    dplyr::reframe(frequency = n()) %>% 
    dplyr::left_join(service_pattern_freq, # aqui
                     by = 'service_pattern') %>%
    #filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::select(route_id, hour, frequency, service_pattern, pattern_frequency)
  
  return(freq)
  
  }
  
  if (simplify == T) {
    hw <- get_frequency_byroute(gtfs)
  }
  
  if (simplify == F) {
    hw <- get_frequency_detailed(gtfs)
  }
  
  if (!simplify %in% c(T, F)) {
    hw <- warning('"simplify" should be one of TRUE or FALSE')
  }
  
  return(hw)
  
}

#get_frequency(gtfs, simplify = F)