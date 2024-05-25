get_servicepattern <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::gtfs_to_wizard(gtfs)
    warning('\nGTFS is not a wizardgtfs object.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    gtfs$dates_services %>% 
    group_by(service_id) %>% 
    reframe(dates = list(as.character(date)),
            n = n()) %>% 
    mutate(service_pattern = paste0('servicepattern-', 1:n())) %>% 
    select(-dates)
    
  
  while(is_list(service_pattern$service_id)) {
    service_pattern <- service_pattern %>% unnest(service_id)
  }
  
  return(service_pattern)
  
}
