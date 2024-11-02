

# Roxygen help ------------------------------------------------------------




# Auxiliar functions ------------------------------------------------------------------



do_keep_duration <- function(x){
  if('set'%in%names(x)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


as.character.Period <- function(x){
  x <- lubridate::hms(x,roll = T)
  paste0(ifelse(hour(x)<10,paste0('0',hour(x)),hour(x)),":",minute(x),":",second(x))
}

verify_keepduration <- function(x){
  if('wizardgtfs_selected' %in% class(x)){
    stops <- x$stop_times$stop_id[x$stop_times$trip_id %in% attr(x,'selection')$trips]
    
    if(any(stops %nin% attr(x,'selection')$stops)){
      return(FALSE)
    }else{
      return(TRUE)
    }
    
  }else{
    return(TRUE)
  }
}


# Main functions ----------------------------------------------------------


edit_dwelltime <- function(gtfs,value){
  if(missing(value)){
    stop('Missing “value” argument with no pattern')
  }
  checkmate::assert_vector(value,len = 1)
  if(!checkmate::test_named(value)){
    cat('The "value" parameter has been assigned as "set"')
    warning('The “value” parameter has been assigned as “set”')
    names(value) <- 'set'
  }
  if(names(value) %in% c('set','to','add','mult') == FALSE){
    stop('The "value" must be one of "set", "to", "add" or "mult"')
  }
  
  UseMethod('edit_dwelltime')
}


edit_dwelltime.list<- function(gtfs,...){
  gtfs <- as_wizardgtfs(gtfs)
  edit_dwelltime.wizardgtfs(gtfs,...)
}

edit_dwelltime.wizardgtfs_selected <- function(gtfs,value){
  
  
  if(!checkmate::test_named(value)){
    names(value) <- 'set'
  }
  
  if(lubridate::is.duration(value)){
    value = as.numeric(dseconds(value))
  }
  
  keep_duration <- do_keep_duration(value)
  
  if('set'%in%names(value)){
    if(!verify_keepduration(gtfs)){
      warning(crayon::red("When 'value' is designated as 'set,' the change applies to the entire trip, regardless of specific stop selections. Use 'to,' 'add,' or 'mult' to adjust dwell time at specific stops.\nTo don't see this, select an entire trip or route.\nSee details using help('edit_dwelltime')\n"))
    }
  }
  
  trips <- gtfs$stop_times %>% 
    group_by(trip_id) %>% 
    arrange(stop_sequence) %>% 
    ungroup() %>%
    dplyr::left_join(
      gtfs$trips %>% select(trip_id,direction_id),
      by  = 'trip_id'
    )
  
  selection_trips <- attr(gtfs,'selection')$trips
  
  trips <- trips %>% 
    group_by(trip_id) %>% 
    group_split()
  
  selection_trips <- sapply(trips, function(x){
    ind <- parent.frame()$i
    if(x$trip_id[1]%in%selection_trips){
      return(ind)
    }else{return(NULL)}
  }) %>% unlist()
  
  if(names(value) == 'set'){
    
    trips[selection_trips] <- lapply(
      trips[selection_trips],
      edit_trips_dwelltime.trips_set,
      value = value
    )
    
  }else{
    
    selection_stops <- attr(gtfs,'selection')$stops
    
    if(names(value) == 'to'){
      trips[selection_trips] <- lapply(
        trips[selection_trips],
        edit_trips_dwelltime.trips_to,
        value = value,
        stops = selection_stops
      )
    }
    
    if(names(value) == 'add'){
      trips[selection_trips] <- lapply(
        trips[selection_trips],
        edit_trips_dwelltime.trips_add,
        value = value,
        stops = selection_stops
      )
    }
    
    if(names(value) == 'mult'){
      trips[selection_trips] <- lapply(
        trips[selection_trips],
        edit_trips_dwelltime.trips_mult,
        value = value,
        stops = selection_stops
      )
    }
    
  }
  
  gtfs$stop_times <- data.table::rbindlist(trips) 
  
  if(is.null(attr(gtfs,'editions'))){
    editions <- list(
      `1` = list(
        `function` = 'edit_dwellime',
        params = list(value=value),
        selection = attr(gtfs,'selection')
      )
    )
  }else{
    editions <- append(
      attr(gtfs,'editions'),
      list(value = list(
        `function` = 'edit_dwellime',
        params = list(value=value),
        selection = attr(gtfs,'selection')
      ))
    )
    
    names(editions) <- as.character(1:length(editions))
    
  }
  
  attr(gtfs,'editions') <- editions
  
  
  return(gtfs)
  
}

edit_dwelltime.wizardgtfs <- function(gtfs,value){
  
  cat('This operation will change all GTFS trips. Consider using a "selection" before the operation. See help("selection")')
  
  if(!checkmate::test_named(value)){
    names(value) <- 'set'
  }
  
  if(lubridate::is.duration(value)){
    value = as.numeric(dseconds(value))
  }
  
  keep_duration <- do_keep_duration(value)
  
  if('set'%in%names(value)){
    if(!verify_keepduration(gtfs)){
      warning(crayon::red("When 'value' is designated as 'set,' the change applies to the entire trip, regardless of specific stop selections. Use 'to,' 'add,' or 'mult' to adjust dwell time at specific stops.\nTo don't see this, select an entire trip or route.\nSee details using help('edit_dwelltime')\n"))
    }
  }
  
  trips <- gtfs$stop_times %>% 
    group_by(trip_id) %>% 
    arrange(stop_sequence) %>% 
    ungroup() %>%
    dplyr::left_join(
      gtfs$trips %>% select(trip_id,direction_id),
      by  = 'trip_id'
    )
  
  trips <- trips %>% 
    group_by(trip_id) %>% 
    group_split()
  
  
  if(names(value) == 'set'){
    
    trips <- lapply(
      trips,
      edit_trips_dwelltime.trips_set,
      value = value
    )
    
  }else{
    
    if(names(value) == 'to'){
      trips <- lapply(
        trips,
        edit_trips_dwelltime.trips_to,
        value = value,
        stops = selection_stops
      )
    }
    
    if(names(value) == 'add'){
      trips <- lapply(
        trips,
        edit_trips_dwelltime.trips_add,
        value = value,
        stops = selection_stops
      )
    }
    
    if(names(value) == 'mult'){
      trips <- lapply(
        trips,
        edit_trips_dwelltime.trips_mult,
        value = value,
        stops = selection_stops
      )
    }
    
  }
  
  gtfs$stop_times <- data.table::rbindlist(trips) 
  
  if(is.null(attr(gtfs,'editions'))){
    editions <- list(
      `1` = list(
        `function` = 'edit_dwellime',
        params = list(value=value),
        selection = 'all'
      )
    )
  }else{
    editions <- append(
      attr(gtfs,'editions'),
      list(value = list(
        `function` = 'edit_dwellime',
        params = list(value=value),
        selection = 'all'
      ))
    )
    
    names(editions) <- as.character(1:length(editions))
    
  }
  
  attr(gtfs,'editions') <- editions
  
  
  return(gtfs)
  
}


# edit_trips_dwelltime <- function(trip,value,stops){
#   UseMethod('edit_trips_dwelltime')
# }


edit_trips_dwelltime.trips_set <- function(trip,value){
  
  trip_na_dwell <- trip %>% 
    filter(arrival_time==""|departure_time=="")
  trip <- trip %>% 
    filter(arrival_time!=""&departure_time!="")
  
  trip <- trip %>% 
    mutate(arrival_time = lubridate::hms(arrival_time) %>%
             suppressWarnings()) %>% 
    mutate(departure_time = lubridate::hms(departure_time) %>%
             suppressWarnings()) %>% 
    mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>% 
    mutate(max_dweeltime_ant = as.numeric(arrival_time-lag(departure_time))/1.5) %>% 
    mutate(max_dweeltime_post = as.numeric(lead(arrival_time)-departure_time)/1.5) %>% 
    mutate(max_dweeltime = apply(tibble(max_dweeltime_ant,max_dweeltime_post),1,min,na.rm = T)) %>% 
    select(-max_dweeltime_post,-max_dweeltime_ant) %>% 
    mutate(change_dweeltime = ifelse(value>max_dweeltime,max_dweeltime-actual_dwelltime,value-actual_dweeltime))
  
  if(any(trip$max_dweeltime<value)){
    warning('Some dwelltimes cannot be ',value,' the maximum dwelltime has been allocated')
  }
  
  index <- 2:(nrow(trip)-1)
  trip$arrival_time[index] <- trip$arrival_time[index]-trip$change_dweeltime[2:(nrow(trip)-1)]/2
  trip$departure_time[index] <- trip$departure_time[index]+trip$change_dweeltime[2:(nrow(trip)-1)]/2
  trip$departure_time[1] <- trip$departure_time[1]+trip$change_dweeltime[1]
  trip$arrival_time[nrow(trip)] <- trip$arrival_time[nrow(trip)]-trip$change_dweeltime[nrow(trip)]
  
  trip %>% 
    select(-max_dweeltime,-actual_dweeltime,-change_dweeltime) %>% 
    mutate(arrival_time = as.character.Period(arrival_time)) %>% 
    mutate(departure_time = as.character.Period(departure_time)) %>% 
    dplyr::select(-direction_id) %>% 
    bind_rows(trip_na_dwell) %>% 
    arrange(stop_sequence) %>%
    return()
}

edit_trips_dwelltime.trips_to <- function(trip,value,stops){
  
  trip_na_dwell <- trip %>% 
    filter(arrival_time==""|departure_time=="")
  trip <- trip %>% 
    filter(arrival_time!=""&departure_time!="")
  
  trip <- trip %>% 
    mutate(arrival_time = lubridate::hms(arrival_time) %>%
             suppressWarnings()) %>% 
    mutate(departure_time = lubridate::hms(departure_time) %>%
             suppressWarnings()) %>% 
    mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>% 
    mutate(change_dweeltime = value-actual_dweeltime) %>% 
    mutate(index = 1:n()) %>% 
    mutate(arrival_time = (index-1)*change_dweeltime+arrival_time) %>% 
    mutate(departure_time = index*change_dweeltime+departure_time)
  
  trip %>% 
    select(-actual_dweeltime,-change_dweeltime,-index) %>% 
    mutate(arrival_time = as.character.Period(arrival_time)) %>% 
    mutate(departure_time = as.character.Period(departure_time)) %>% 
    dplyr::select(-direction_id) %>% 
    bind_rows(trip_na_dwell) %>% 
    arrange(stop_sequence) %>% 
    return()
  
}

edit_trips_dwelltime.trips_add <- function(trip,value,stops){
  trip_na_dwell <- trip %>% 
    filter(arrival_time==""|departure_time=="")
  trip <- trip %>% 
    filter(arrival_time!=""&departure_time!="")
  
  if(value<0){
    trip <- trip %>% 
      mutate(arrival_time = lubridate::hms(arrival_time) %>%
               suppressWarnings()) %>% 
      mutate(departure_time = lubridate::hms(departure_time) %>%
               suppressWarnings()) %>% 
      mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>% 
      mutate(non_negative = actual_dweeltime+value >= 0) %>% 
      mutate(index = 1:n()) %>% 
      mutate(change_value = ifelse(non_negative, value, actual_dweeltime )) %>% 
      mutate(arrival_time = (index-1)*change_value + arrival_time) %>% 
      mutate(departure_time = index*change_value + departure_time)
  }else{
    trip <- trip %>% 
      mutate(arrival_time = lubridate::hms(arrival_time) %>%
               suppressWarnings()) %>% 
      mutate(departure_time = lubridate::hms(departure_time) %>%
               suppressWarnings()) %>% 
      mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>% 
      mutate(index = 1:n()) %>%
      mutate(arrival_time = (index-1)*value + arrival_time) %>% 
      mutate(departure_time = index*value + departure_time)
  }
  
  
  
  trip %>% 
    select(-actual_dweeltime,-change_dweeltime,-index) %>% 
    mutate(arrival_time = as.character.Period(arrival_time)) %>% 
    mutate(departure_time = as.character.Period(departure_time)) %>% 
    dplyr::select(-direction_id) %>% 
    bind_rows(trip_na_dwell) %>% 
    arrange(stop_sequence) %>% 
    return()
}

edit_trips_dwelltime.trips_mult <- function(trip,value,stops){
  trip_na_dwell <- trip %>% 
    filter(arrival_time==""|departure_time=="")
  trip <- trip %>% 
    filter(arrival_time!=""&departure_time!="")
  
  trip <- trip %>% 
    mutate(arrival_time = lubridate::hms(arrival_time) %>%
             suppressWarnings()) %>% 
    mutate(departure_time = lubridate::hms(departure_time) %>%
             suppressWarnings()) %>% 
    mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>% 
    mutate(change_dweeltime = (value*actual_dweeltime)-actual_dweeltime) %>% 
    mutate(arrival_time = (1:n()-1)*change_dweeltime+arrival_time) %>% 
    mutate(departure_time = (1:n())*change_dweeltime+departure_time)
  
  trip %>% 
    select(-actual_dweeltime,-change_dweeltime) %>% 
    mutate(arrival_time = as.character.Period(arrival_time)) %>% 
    mutate(departure_time = as.character.Period(departure_time)) %>% 
    dplyr::select(-direction_id) %>% 
    bind_rows(trip_na_dwell) %>% 
    arrange(stop_sequence) %>% 
    return()
}





