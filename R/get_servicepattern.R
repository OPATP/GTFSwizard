get_servicepattern <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::gtfs_to_wizard(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    gtfs$dates_services %>% 
    group_by(service_id) %>% 
    reframe(dates = list(as.character(date)),
            pattern_frequency = n()) %>% 
    mutate(service_pattern = paste0('servicepattern-', 1:n())) %>% 
    select(service_id, service_pattern, pattern_frequency)
  
  
  while(is_list(service_pattern$service_id)) {
    service_pattern <- service_pattern %>% unnest(., cols = c(service_id))
  }
  
  return(service_pattern)
  
}
