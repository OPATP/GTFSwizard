get_dwelltimes <- function(gtfs, max.dwelltime = 90, simplify = T){
  
  get_dwelltime_byhour <- function(gtfs){
  
    if(!"wizardgtfs" %in% class(gtfs)){
      gtfs <- GTFSwizard::gtfs_to_wizard(gtfs)
      warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
    }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  dwell_time <- 
    gtfs$stop_times %>% 
    dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>% 
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>% 
    dplyr::group_by(arrival_time, departure_time, service_pattern) %>% 
    dplyr::reframe(n = n()) %>% 
    dplyr::mutate(hour = str_extract(arrival_time, "\\d+") %>% as.numeric(),
                  arrival_time = arrival_time %>% 
                    stringr::str_split(":") %>% 
                    lapply(FUN = as.numeric) %>% 
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>% 
                    unlist() %>% 
                    na.omit(),
                  departure_time = departure_time %>% 
                    stringr::str_split(":") %>% 
                    lapply(FUN = as.numeric) %>% 
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>% 
                    unlist() %>% 
                    na.omit(),
                  dwell_time = departure_time - arrival_time
    ) %>% 
    dplyr::filter(dwell_time <= max.dwelltime) %>%
    dplyr::select(hour, dwell_time, service_pattern, n) %>% 
    dplyr::left_join(service_pattern %>% dplyr::select(-service_id) %>% unique(),
                     by = 'service_pattern') %>% 
      dplyr::group_by(hour, dwell_time, service_pattern) %>% 
      dplyr::reframe(frequency = sum(n * pattern_frequency))
  
  return(dwell_time)
  
}

  get_dwelltime_detailed <- function(gtfs){
  
    if(!"wizardgtfs" %in% class(gtfs)){
      gtfs <- GTFSwizard::gtfs_to_wizard(gtfs)
      warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
    }
 
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  dwell_time <- 
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>% 
    dplyr::mutate(hour = str_extract(arrival_time, "\\d+") %>% as.numeric(),
                  arrival_time = arrival_time %>% 
                    stringr::str_split(":") %>% 
                    lapply(FUN = as.numeric) %>% 
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>% 
                    unlist() %>% 
                    na.omit(),
                  departure_time = departure_time %>% 
                    stringr::str_split(":") %>% 
                    lapply(FUN = as.numeric) %>% 
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>% 
                    unlist() %>% 
                    na.omit(),
                  dwell_time = departure_time - arrival_time
    ) %>% 
    dplyr::filter(dwell_time <= max.dwelltime) %>%
    dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>% 
    dplyr::select(route_id, stop_id, hour, dwell_time, service_pattern, pattern_frequency)
  
  return(dwell_time)
  
  }
  
  if (simplify == T) {
    dwell_time <- get_dwelltime_byhour(gtfs)
  }
  
  if (simplify == F) {
    dwell_time <- get_dwelltime_detailed(gtfs)
  }
  
  if (!simplify %in% c(T, F)) {
    dwell_time <- get_dwelltime_byhour(gtfs)
    warning('\n"simplify" should be one of TRUE or FALSE\nReturning "simplify = TRUE"')
  }
  
  return(dwell_time)
  
}
