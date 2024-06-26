

write_gtfs <- function(gtfs, zipfile, ...){
  UseMethod('write_gtfs')
}

write_gtfs.wizardgtfs <- function(gtfs, zipfile, ...){
  
  gtfs <- sf_to_df(gtfs)
  gtfs <- lapply(gtfs, as_df_ordinary)
  class(gtfs) <- c('gtfs','list')
  gtfs <- rm_servicepattern(gtfs)
  gtfsio::export_gtfs(gtfs = gtfs, path = zipfile, ...)
  
}

write_gtfs.list <- function(gtfs, zipfile, ...){
  gtfsio::export_gtfs(gtfs = gtfs, path = zipfile, ...)
}

write_gtfs.tidygtfs <- function(gtfs, zipfile, ...){
  tidytransit::write_gtfs(gtfs, zipfile, ...)
}

write_gtfs.dt_gtfs <- function(gtfs, zipfile, ...){
  gtfstools::write_gtfs(gtfs, path = zipfile, ...)
}


sf_to_df <- function(gtfs){
  if('shapes' %in% names(gtfs)){
    try({gtfs$shapes <- st_as_sf(gtfs$shapes)},silent = T)
    if('sf'%in%class(gtfs$shapes)){
      new_shapes <- gtfs$shapes %>% 
        group_by(shape_id) %>% 
        mutate(coords = coords_shapes(geometry)) %>% 
        st_drop_geometry() %>% 
        unnest('coords') %>% 
        ungroup()
      
      if('shape_dist_traveled' %in% names(new_shapes)){
        new_shapes <- new_shapes %>%  
          group_by('shape_id') %>% 
          mutate(shape_dist_traveled = shape_dist_traveled/n()) %>% 
          ungroup()
      }
      
      gtfs$shapes <- new_shapes
      
    }
  }
  
  if('stops' %in% names(gtfs)){
    try({gtfs$stops <- st_as_sf(gtfs$stops)},silent = T)
    if('sf' %in% class('stops')){
      gtfs$stops <- gtfs$stops %>% 
        mutate(coords_stops(geometry)) %>% 
        st_drop_geometry() %>% 
        as.data.frame()
    }
  }
  
  return(gtfs)
}


coords_shapes <- function(geom){
  st_coordinates(geom)[,1:2] %>% 
    as.data.frame() %>% 
    setnames(new  = c('shape_pt_lon','shape_pt_lat')) %>% 
    mutate(shape_pt_sequence = 1:n()) %>% 
    list()
}
coords_stops <- function(geom){
  st_coordinates(geom)[,1:2] %>% 
    as.data.frame() %>% 
    setnames(new  = c('stop_lon','stop_lat')) 
}




as_df_ordinary <- function(df){
  
  df <- as.data.frame(df)
  
  df2 <- lapply(df, function(col){
    if(hms::is_hms(col)){
      col = as.character(col)
    }
    if(is.POSIXct(col)|is.POSIXlt(col)|is.Date(col)){
      col = as.character(col)
    }
    if(is.character(.col)){
      col[is.na(col)] <- ""
    }
    return(col)
  })
 
  attributes(df2) <- attributes(df)
  
  return(df)
  
}

rm_servicepattern <- function(gtfs){
  gtfs[names(gtfs) != "dates_services"]
}

