get_headway <- function(gtfs){
  
    if(!"wizardgtfs" %in% class(gtfs))(gtfs <- GTFSwizard::gtfs_to_wizard(gtfs))

    hw <-
      gtfs$stop_times %>% 
      dplyr::select(trip_id, arrival_time, stop_id) %>% 
      dplyr::mutate(stop_id = as_factor(stop_id)) %>% 
      dplyr::filter(!arrival_time == '') %>% 
      dplyr::mutate(hour = str_extract(arrival_time, "\\d+"),
                    arrival_time = gtfs$stop_times$arrival_time %>% 
                      stringr::str_split(":") %>% 
                      lapply(FUN = as.numeric) %>% 
                      lapply(FUN = function(x){
                        x[1]*60*60+x[2]*60+x[3]
                      }) %>% 
                      unlist() %>% 
                      na.omit(),
      ) %>%
      dplyr::left_join(gtfs$trips) %>% 
      dplyr::group_by(route_id, stop_id, service_id) %>% 
      dplyr::mutate(headway.minutes = (lead(arrival_time) - arrival_time)/60) %>%
      dplyr::filter(headway.minutes >= 0) %>% 
      dplyr::group_by(route_id, stop_id, service_id, hour) %>% 
      dplyr::reframe(average.headway.minutes = mean(headway.minutes)) # %>% filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
  
  return(hw)
}

#get_headway(gtfs)
