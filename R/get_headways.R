get_headways <- function(gtfs, simplify = T){
  
  get_headway_byroute <- function(gtfs){
    
    if(!"wizardgtfs" %in% class(gtfs)){
      gtfs <- GTFSwizard::gtfs_to_wizard(gtfs)
      warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
    }
  
    service_pattern <- 
      GTFSwizard::get_servicepattern(gtfs)
    
    service_pattern_freq <- 
      gtfs$dates_services$service_id %>% 
      unlist %>%
      tibble(service_id = .) %>% 
      dplyr::left_join(service_pattern, by = 'service_id', relationship = "many-to-many") %>% 
      dplyr::group_by(service_pattern) %>% 
      reframe(pattern_frequency= n())
    
    hw <-
      gtfs$stop_times %>% 
      dplyr::select(trip_id, arrival_time, stop_id) %>% 
      dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
      dplyr::filter(!arrival_time == '') %>%
      dplyr::mutate(hour = str_extract(arrival_time, "\\d+")) %>% 
      dplyr::left_join(service_pattern, by = 'service_id') %>%
      group_by(route_id, hour, service_pattern) %>%
      reframe(arrival_time = arrival_time[1]) %>% 
      dplyr::mutate(arrival_time = arrival_time %>% 
                      stringr::str_split(":") %>% 
                      lapply(FUN = as.numeric) %>% 
                      lapply(FUN = function(x){
                        x[1]*60*60+x[2]*60+x[3]
                      }) %>% 
                      unlist() %>% 
                      na.omit(),
      ) %>% 
        dplyr::group_by(route_id, service_pattern) %>% 
        dplyr::mutate(headway.minutes = (lead(arrival_time) - arrival_time)/60) %>%
        dplyr::filter(headway.minutes >= 0) %>% 
        dplyr::group_by(route_id, service_pattern, hour) %>% 
        dplyr::reframe(average.headway = mean(headway.minutes, na.rm = T)) %>%
        # filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
        dplyr::mutate(hour = as.numeric(hour)) %>% 
        dplyr::left_join(service_pattern_freq,
                         by = 'service_pattern') %>% 
        dplyr::select(route_id, hour, average.headway, service_pattern, pattern_frequency) %>% 
      na.omit()
    
    return(hw)
    
  }

  get_headway_detailed <- function(gtfs){
    
    if(!"wizardgtfs" %in% class(gtfs)){
      gtfs <- GTFSwizard::gtfs_to_wizard(gtfs)
      warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
    }
    
    service_pattern <- 
      GTFSwizard::get_servicepattern(gtfs)
    
    service_pattern_freq <- 
      gtfs$dates_services$service_id %>% 
      unlist %>%
      tibble(service_id = .) %>% 
      dplyr::left_join(service_pattern, by = 'service_id', relationship = "many-to-many") %>% 
      dplyr::group_by(service_pattern) %>% 
      reframe(pattern_frequency= n())
    
    hw <-
      gtfs$stop_times %>% 
      dplyr::select(trip_id, arrival_time, stop_id) %>% 
      dplyr::mutate(stop_id = as_factor(stop_id)) %>% 
      dplyr::filter(!arrival_time == '') %>% 
      dplyr::mutate(hour = str_extract(arrival_time, "\\d+"),
                    arrival_time = arrival_time %>% 
                      stringr::str_split(":") %>% 
                      lapply(FUN = as.numeric) %>% 
                      lapply(FUN = function(x){
                        x[1]*60*60+x[2]*60+x[3]
                      }) %>% 
                      unlist() %>% 
                      na.omit(),
      ) %>%
      dplyr::left_join(gtfs$trips, by = 'trip_id') %>% 
      dplyr::left_join(service_pattern, by = 'service_id') %>%
      dplyr::group_by(route_id, stop_id, service_pattern) %>% 
      dplyr::mutate(headway.minutes = (lead(arrival_time) - arrival_time)/60) %>%
      dplyr::filter(headway.minutes >= 0) %>% 
      dplyr::group_by(route_id, stop_id, service_pattern, hour) %>% 
      dplyr::reframe(average.headway = mean(headway.minutes, na.rm = T)) %>%
      # filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
      dplyr::mutate(hour = as.numeric(hour)) %>% 
      dplyr::left_join(service_pattern_freq,
                       by = 'service_pattern') %>% 
      dplyr::select(route_id, stop_id, hour, average.headway, service_pattern, pattern_frequency) %>% 
      na.omit()
    
    return(hw)
  }
  
  if (simplify == T) {
    hw <- get_headway_byroute(gtfs)
  }
  
  if (simplify == F) {
    hw <- get_headway_detailed(gtfs)
  }
  
  if (!simplify %in% c(T, F)) {
    hw <- get_headway_byroute(gtfs)
    warn <- warning('\n"simplify" should be one of TRUE or FALSE\nReturning "simplify = TRUE"')
  }
  
  return(hw)
  return(warn)
  
}

#get_headways(gtfs, simplify = 'yes')
