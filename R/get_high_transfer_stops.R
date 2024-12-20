library(GTFSwizard)


gtfs <- read_gtfs(rstudioapi::selectFile())


`%>%` <- dplyr::`%>%`

gtfs <- for_bus_gtfs

service_pattern <- get_servicepattern(gtfs)

transfers <- gtfs$stop_times %>%
  dplyr::select(trip_id,stop_id,stop_sequence,departure_time) %>%
  dplyr::filter(!is.na(departure_time)&departure_time!="") %>%
  dplyr::left_join(gtfs$trips %>%
              dplyr::select(trip_id,route_id,service_id),by = 'trip_id') %>%
  dplyr::mutate(hour = as.numeric(stringr::str_extract(departure_time,'^\\d{2}'))) %>%
  dplyr::mutate(hour = ifelse(hour>=24,hour-24,hour)) %>%
  dplyr::left_join(service_pattern,by = 'service_id',
                   relationship = 'many-to-many') %>%
  dplyr::group_by(hour,stop_id,service_pattern,pattern_frequency) %>%
  dplyr::reframe(
    n_trips = length(unique(trip_id)),
    n_routes = length(unique(route_id))
  )


attr(transfers,'stop_position') <- gtfs$stops %>% dplyr::select(stop_id,stop_lon,stop_lat) %>% unique()
