

plot.wzd_transfercluster <- function(x,hour){

  if(missing('hour')){
    message("Filtering the time with the most trips")
    hour <- x$hour[which.max(x$n_trips)] %>% as.numeric()
  }
  checkmate::assert_numeric(hour)

  h <- hour
  x %>%
    dplyr::filter(hour == h) %>%
    dplyr::left_join(attr(x,'stop_position'),by = 'stop_id') %>%
    dplyr::mutate(cluster = as.character(cluster)) %>%
    sf::st_as_sf(coords = c('stop_lon','stop_lat'),crs = 4326) %>%
    ggplot2::ggplot(ggplot2::aes(color = cluster,alpha = cluster))+
    ggplot2::geom_sf()+
    ggplot2::theme_void()+
    ggplot2::facet_wrap(~service_pattern)+
    ggplot2::scale_alpha_manual(
      breaks = as.character(1:4),
      values = c(0,0.1,.8,1)
    ) +
    ggplot2::guides(alpha = 'none')+
    ggplot2::labs(title = 'Clusters oh high transfers',subtitle = paste0('Hour: ',hour))
}

if(interactive()){
  plot(transfer_clusters)


  transfer_clusters %>%
    dplyr::filter(hour == 6) %>%
    dplyr::filter(service_pattern == 'servicepattern-1') %>%
    dplyr::filter(cluster == 4|stop_id=='5819') %>%
    dplyr::left_join(attr(transfer_clusters,'stop_position'),by = 'stop_id') %>%
    sf::st_as_sf(coords = c('stop_lon','stop_lat'),crs = 4326) %>%
    leaflet::leaflet() %>%
    leaflet::addMarkers(label =  ~n_routes) %>%
    leaflet::addTiles()


  gtfs$stops %>%
    dplyr::filter(stringr::str_detect(stop_name,'TERMINAL')) %>%
    sf::st_as_sf(coords = c('stop_lon','stop_lat'),crs = 4326) %>%
    leaflet::leaflet() %>%
    leaflet::addMarkers() %>%
    leaflet::addTiles()
}



