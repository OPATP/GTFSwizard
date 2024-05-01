


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
      "%Y%m%d",
      "%Y-%m-%d",
      "%Y/%m/%d"
    ))
}
chr_to_hms <- function(x){
  x[nchar(x)==0] <- NA
  hms::as_hms(x)
}

seqs_table <- function(intervals){
  tibble(
    period=unique(intervals)
  ) %>% 
    mutate(date = lapply(period,function(x) seq(int_start(x),int_end(x),'1 day')))
}


label_wday <- function(x=1:7){
  c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday')[x]
}

get_wday_services <- function(x){
  resp <- tibble(
    wday = label_wday()
  )
  resp$service_id <- lapply(label_wday(), function(y){
    list(x$service_id[x[,y]==1])
  })
  resp
}

create_dates_services_table <- function(gtfs_list){
  calendar_intervals <- gtfs_list$calendar %>% 
    mutate(period = interval(start_date,end_date))
  
  week_days_services <- calendar_intervals %>% 
    group_by(period) %>% 
    reframe(get_wday_services(.))
  
  gtfs_list[['dates_services']] <- seqs_table(calendar_intervals$period) %>% 
    unnest('date') %>% 
    mutate(wday = label_wday(lubridate::wday(date,week_start = 1))) %>% 
    left_join(
      week_days_services,
      by = c('period','wday')
    ) %>% 
    select(date,service_id)
  return(gtfs_list)
}


