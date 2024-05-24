shapes_from_stops <- function(gtfs){
  
  warning('This algorithm reconstructs the shape files using an Euclidean approximation, based on the coordinates and sequence of stops for each trip.')
  
  if(!"wizardgtfs" %in% class(gtfs))(gtfs <- GTFSwizard::gtfs_to_wizard(gtfs))
  
  shapes.dic <- 
    gtfs$stop_times %>%
    dplyr::select(trip_id, stop_id, stop_sequence) %>% 
    dplyr::arrange(trip_id, stop_sequence) %>% 
    dplyr::left_join(gtfs$stops %>%
                       tidytransit::stops_as_sf() %>% 
                       dplyr::select(stop_id),
                     by = join_by(stop_id)
                     ) %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::reframe(geometry = st_union(geometry)) %>% 
    sf::st_as_sf(crs = 4326) %>% 
    sf::st_cast('LINESTRING') %>% 
    dplyr::left_join(gtfs$trips %>%
                       dplyr::select(trip_id, route_id)) %>% 
    dplyr::group_by(geometry) %>% 
    dplyr::reframe(trip_id = list(trip_id)) %>%
    dplyr::mutate(shape_id = paste0('shape-', 1:n()))
  
  gtfs$shapes <-
    shapes.dic[, c(3, 1)] %>% 
    sf::st_as_sf()
  
  gtfs$trips <- 
    gtfs$trips %>% 
    dplyr::select(-shape_id) %>% 
    dplyr::left_join(shapes.dic %>%
                       dplyr::select(-geometry) %>% 
                       tidyr::unnest(cols = 'trip_id'))
  
  gtfs <-
    tidytransit::sf_as_tbl(gtfs)
  
  return(gtfs)
  
}