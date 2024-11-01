rm(list = ls())


gtfs <- GTFSwizard::for_gtfs

freq <- GTFSwizard::get_frequency(gtfs) %>% 
  filter(pattern_frequency == max(pattern_frequency))

class(freq) <- c('wizard_gtfsfrequency',class(freq))

attr(freq, 'baseshps') <- left_join(
  GTFSwizard::get_shapes_sf(gtfs$shapes),
  gtfs$trips %>% select(shape_id,route_id) %>% unique(),
  by = 'shape_id'
)


plot.wizard_gtfsfrequency <- function(freqs){
  
  shps <- attr(freq, 'baseshps')
  
  freqs <- freqs %>% 
    filter(pattern_frequency == max(pattern_frequency))
  

  shps <- shps %>% 
    left_join(
      freqs,
      by = 'route_id'
    )
  
  ggplot()+
    geom_sf(data = shps, aes(color = daily.frequency))+
    viridis::scale_color_viridis(option = 'H')
  
  # pal <- colorNumeric(
  #   palette = viridis::viridis_pal(option = 'H')(30),
  #   domain = shps$daily.frequency
  # )
  # 
  # leaflet(shps) %>% 
  #   addPolylines(color = ~pal(daily.frequency), weight = 2) %>% 
  #   addTiles()
  
}

plot(freq)

