


print.wizardgtfs <- function(gtfs){
  
  
  
  lapply(gtfs,tibble:::print.tbl_df, n=5)
}

print.summary.wizardgtfs <- function(){
  
}

summary.wizardgtfs <- function(){
  
}

plot.wizardgtfs <- function(){
  
  
}


gtfs_to_wizard <- function(gtfs_list){
  UseMethod('gtfs_to_wizard')
}

gtfs_to_wizard.tidygtfs <- function(gtfs_list){
  gtfs_list[['dates_services']] <- gtfs_list$.$dates_services %>% 
    dplyr::group_by(date) %>% 
    reframe(service_id = list(service_id))
  gtfs_list<-gtfs_list[names(gtfs_list)!='.']
  class(gtfs_list) <- c('wizardgtfs','gtfs','list')
  return(gtfs_list)
}

gtfs_to_wizard.default <- function(gtfs_list){
  duplicate_ids <- has_duplicate_primary(gtfs_list)
  
  if('calendar'%in%names(gtfs_list)|'calendar_dates'%in%names(gtfs_list)==FALSE){
    ## Tentar formatar com cor e itálico
    
    
    warning(paste0("Can't find calendar nor calendar_dates tables in GTFS.\nReturning a gtfs object."))
    return(gtfs_list)
    
    
  }
  if(any(unlist(duplicate_ids))){
    warning("Duplicated ids found in: ", paste0(names(duplicate_ids[duplicate_ids]), 
                                                collapse = ", "), "\n", "The returned object is not a wizardgtfs object.")
    return(gtfs_list)
  }else{
    gtfs_obj <- convert_to_tibble(gtfs_list) %>% 
      convert_times_and_dates() %>% 
      create_dates_services_table()
    #gtfs_obj <- gtfsio::new_gtfs(gtfs_obj)
    class(gtfs_obj) <- c('wizardgtfs','gtfs','list')
    return(gtfs_obj)
  }
}

has_duplicate_primary <- function(gtfs_list){
  duplicated_ids <- as.list(rep(FALSE,length(gtfs_list)))
  names(duplicated_ids) <- names(gtfs_list)
  for (table_name in names(primary_ids())) {
    if(table_name %in% names(duplicated_ids)){
      primary_vec <- gtfs_list[[table_name]] %>% dplyr::select(primary_ids()[table_name])
      if(anyDuplicated(primary_vec)>0){
        duplicated_ids[[table_name]] <- TRUE
      }
    }
    
  }
  return(unlist(duplicated_ids))
}

primary_ids <- function(){
  c(agency='agency_id',trips='trip_id',calendar='service_id',routes='route_id',stops='stop_id')
}

convert_to_tibble <- function(x){
  purrr::map(x, as_tibble)
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
    # gtfs_list$stop_times$arrival_time <- 
    #   chr_to_hms(gtfs_list$stop_times$arrival_time)
    # gtfs_list$stop_times$departure_time <- 
    #   chr_to_hms(gtfs_list$stop_times$departure_time)
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

chr_to_segs <- function(x){
  x <- as.numeric(x)
  return(x[1]*60*60+x[2]*60+x[3])
}

chr_to_hms <- function(x){
  x[nchar(x)==0] <- NA
  x <- purrr::map_vec(str_split(x,':'),function(y){
    ifelse(
        sum(is.na(y))==0,
        NA,
        chr_to_segs(y) 
      )
    }) 
  hms::hms(x)
}

seqs_table <- function(intervals){
  tibble(
    period=unique(intervals)
  ) %>% 
    dplyr::mutate(date = purrr::map(period,function(x) seq(int_start(x),int_end(x),'1 day')))
}


label_wday <- function(x=1:7){
  c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday')[x]
}

without <- function(x,y){
  x[! x %in% y]
}

get_wday_services <- function(x){
  resp <- tibble(
    wday = label_wday()
  )
  resp$service_id <- purrr::map(label_wday(), function(y){
    list(x$service_id[x[,y]==1])
  })
  resp
}

create_dates_services_table <- function(gtfs_list){
  
  if('calendar'%in%names(gtfs_list)&'calendar_dates'%in%names(gtfs_list)){
    
    calendar_intervals <- gtfs_list$calendar %>% 
      dplyr::mutate(period = lubridate::interval(start_date,end_date))
    
    week_days_services <- calendar_intervals %>% 
      dplyr::group_by(period) %>% 
      reframe(get_wday_services(.))
    
    dates_services_regular <- seqs_table(calendar_intervals$period) %>% 
      unnest('date') %>% 
      dplyr::mutate(wday = label_wday(lubridate::wday(date,week_start = 1))) %>% 
      left_join(
        week_days_services,
        by = c('period','wday')
      ) %>% 
      dplyr::select(date,service_id)
    
    aditional_services <- gtfs_list$calendar_dates %>% 
      filter(exception_type==1) %>% 
      dplyr::group_by(date) %>% 
      reframe(service_id = list(service_id))
    
    removed_services<- gtfs_list$calendar_dates %>% 
      filter(exception_type==2) %>% 
      dplyr::group_by(date) %>% 
      reframe(service_id = list(service_id))
    
    if(nrow(aditional_services)>0){
      full_services <- bind_rows(
        dates_services_regular,
        aditional_services
      ) %>% 
        dplyr::group_by(date) %>% 
        reframe(
          service_id = list(unique(unlist(service_id)))
        )
    }else{
      full_services <- dates_services_regular
    }
    
    if(nrow(removed_services)>0){
      full_services <- bind_rows(
        full_services %>% dplyr::mutate(type = 1),
        removed_services %>% dplyr::mutate(type = 2)
      ) %>% 
        dplyr::group_by(date) %>% 
        reframe(service_id = list(without(unlist(service_id[type==1]),unlist(service_id[type==2]))))
      
    }
    
    gtfs_list[['dates_services']] <- full_services
    
    return(gtfs_list)
    
  }else{
    
    if('calendar'%in%names(gtfs_list)){
      
      calendar_intervals <- gtfs_list$calendar %>% 
        dplyr::mutate(period = interval(start_date,end_date))
      
      week_days_services <- calendar_intervals %>% 
        dplyr::group_by(period) %>% 
        reframe(get_wday_services(.))
      
      gtfs_list[['dates_services']] <- seqs_table(calendar_intervals$period) %>% 
        unnest('date') %>% 
        dplyr::mutate(wday = label_wday(lubridate::wday(date,week_start = 1))) %>% 
        left_join(
          week_days_services,
          by = c('period','wday')
        ) %>% 
        dplyr::select(date,service_id)
      return(gtfs_list)
      
    }else{
      
      aditional_services <- gtfs_list$calendar_dates %>% 
        filter(exception_type==1) %>% 
        dplyr::group_by(date) %>% 
        reframe(service_id = list(service_id))
      
      removed_services<- gtfs_list$calendar_dates %>% 
        filter(exception_type==2) %>% 
        dplyr::group_by(date) %>% 
        reframe(service_id = list(service_id))
      
      gtfs_list[['dates_services']] <- bind_rows(
        aditional_services %>% dplyr::mutate(type = 1),
        removed_services %>% dplyr::mutate(type = 2)
      ) %>% 
        dplyr::group_by(date) %>% 
        reframe(service_id = list(without(unlist(service_id[type==1]),unlist(service_id[type==2]))))
      
      return(gtfs_list)
      
    }
    
  }
  
  
}

verify_tables <- function(x,tables){
  ls <- rep(FALSE,length(tables))
  ls <- tables%in%names(x)==FALSE
  names(ls) <- tables
  if(any(ls)){
    stop(paste0("The following tables are missing in gtfs: ", paste0(names(ls[ls]), 
                                                collapse = ", "), "\n", "Fix this problem before proceeding."))
  }
}



geom_shapes <- function(obj){
  if('sf'%in%class(obj)){
    st_crs(obj) <- 4326
    return(obj)
  }else{
    obj %>% 
      group_by(shape_id) %>% 
      reframe(wkt = paste0(shape_pt_lon,' ',shape_pt_lat)) %>% 
      group_by(shape_id) %>% 
      reframe(wkt = paste0('LINESTRING(',paste0(wkt,collapse = ', '), ')')) %>% 
      st_as_sf(wkt = 'wkt',crs=4326)
  }
}


