get_dwelltime <- function(gtfs, max.dwelltime = 90){
  
  dwell_time <- 
    gtfs$stop_times %>% 
    dplyr::select(arrival_time, departure_time, stop_id, trip_id)  %>% 
    dplyr::mutate(stop_id = as_factor(stop_id)) %>% 
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>% 
    dplyr::mutate(hour = str_extract(arrival_time, "\\d+") %>% as.numeric(),
                  arrival_time = gtfs$stop_times$arrival_time %>% 
                    stringr::str_split(":") %>% 
                    lapply(FUN = as.numeric) %>% 
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>% 
                    unlist() %>% 
                    na.omit(),
                  departure_time = gtfs$stop_times$departure_time %>% 
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
    dplyr::left_join(gtfs$trips) %>% 
    dplyr::select(route_id, stop_id, hour, dwell_time)
  
  return(dwell_time)
  
}

#get_dwelltime(gtfs)
