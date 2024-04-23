file.path = 'ber_gtfs.zip'
gtfs <- read_gtfs(file.path)
transport_moment <- function( gtfs,route_id = 'all', week_day=c("monday", "tuesday", "wednesday", "thursday", "friday", 
                                                   "saturday", "sunday"), by_wd = FALSE ){
  checkmate::assert_class(gtfs,'gtfs_obj')
  checkmate::assert_names(week_day,subset.of = c("monday", "tuesday", "wednesday", "thursday", "friday", 
                                                 "saturday", "sunday"))
  checkmate::assert_string(route_id)
  checkmate::assert_logical(by_wd, len = 1, any.missing = F)
  
  if('calendar'%in%names(gtfs)){
    relevant_service <- lapply(week_day, function(x){
      gtfs$calendar[eval(parse(text = paste0(x,"==","1")))]
    })
    names(relevant_service) <- week_day
    relevant_trips <- lapply(relevant_service, function(x){
      gtfs$trips %>% 
        filter(service_id%in%x$service_id) %>% 
        .$trip_id
    })
    if(by_wd){
      moments <- lapply(relevant_trips, function(x))
    }else{
      trips <- unlist(relevant_trips) %>% unique()
      
    }
    
  }else{
    warning('There are no days of the week in calendar.txt')
  }
  
  
  
}


gtfstools::filter_by_weekday
