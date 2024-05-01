


print.wizardgtfs <- function(){
  
}


gtfs_to_wizard <- function(gtfs_list){
  duplicate_ids <- has_duplicate_primary(gtfs_list)
  if(any(unlist(duplicate_ids))){
    warning("Duplicated ids found in: ", paste0(names(duplicate_ids[duplicate_ids]), 
                                                collapse = ", "), "\n", "The returned object is not a wizardgtfs object.")
    return(gtfs_list)
  }else{
    gtfs_obj <- convert_to_tibble(gtfs_list)
    gtfs_obj <- convert_times_and_dates(gtfs_obj)
    gtfs_obj <- create_dates_services_table(gtfs_obj)
    gtfs_obj <- gtfsio::new_gtfs(gtfs_obj)
    class(gtfs_obj) <- c('wizardgtfs','gtfs','list')
    return(gtfs_obj)
  }
}

has_duplicate_primary <- function(gtfs_list){
  duplicated_ids <- as.list(rep(FALSE,length(gtfs_list)))
  names(duplicated_ids) <- names(gtfs_list)
  for (table_name in names(primary_ids())) {
    primary_vec <- gtfs_list[[table_name]] %>% select(primary_ids()[table_name])
    if(anyDuplicated(primary_vec)>0){
      duplicated_ids[[table_name]] <- TRUE
    }
  }
  return(unlist(duplicated_ids))
}

primary_ids <- function(){
  c(agency='agency_id',trips='trip_id',calendar='service_id',routes='route_id',stops='stop_id')
}

convert_to_tibble <- function(x){
  lapply(x, as_tibble)
}

convert_times_and_dates <- function(gtfs_list){
  
  if('calendar'%in%names(gtfs_list)){
    gtfs_list$calendar$start_date <- 
      date_to_posixct(gtfs_list$calendar$start_date)
    gtfs_list$calendar$end_date <- 
      date_to_posixct(gtfs_list$calendar$end_date)
  }
  if('calendar_dates'%in%names(gtfs_list)){
    gtfs_list$calendar_dates$date <- 
      date_to_posixct(gtfs_list$calendar_dates$date)
  }
  if('stop_times'%in%names(gtfs_list)){
    gtfs_list$stop_times$arrival_time <- 
      chr_to_hms(gtfs_list$stop_times$arrival_time)
    gtfs_list$stop_times$departure_time <- 
      chr_to_hms(gtfs_list$stop_times$departure_time)
  }
  return(gtfs_list)
}

date_to_posixct <- function(x) {
  as.character(x) %>% 
    as.POSIXct(tryFormats = c(
      "%Y-%m-%d",
      "%Y/%m/%d",
      "%Y%m%d"
    ))
}
chr_to_hms <- function(x){
  x[nchar(x)==0] <- NA
  hms::as_hms(x)
}

create_dates_services_table <- function(gtfs_list){
  
  gtfs_list[['dates_services']] <- gtfs_list$calendar %>% 
    mutate(period = interval(start_date,end_date)) %>% 
    mutate(date = lapply(period,function(x) seq(int_start(x),int_end(x),'1 day'))) %>% 
    select(-period) %>% 
    unnest('date') %>% 
    mutate(wday = lubridate::wday(date,week_start = 1)) %>% 
    filter((wday==1&monday==1)|
             (wday==2&tuesday==1)|
             (wday==3&wednesday==1)|
             (wday==4&thursday==1)|
             (wday==5&friday==1)|
             (wday==6&saturday==1)|
             (wday==7&sunday==1)) %>% 
    group_by(date) %>% 
    reframe(service_id=list(service_id)) %>% 
    as_tibble()
  return(gtfs_list)
}

label_wday <- function(x){
  c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday')[xççççççç]
} 


