plot_calendar <- function(gtfs, ncol = 6, facet_by_year = FALSE){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as_gtfswizard() is advised.')
  }
  
  services <- 
    gtfs$trips$service_id %>% 
    table %>% 
    data.frame %>% 
    tibble %>% 
    stats::setNames(c('service_id', 'trips'))
  
  while(rlang::is_list(gtfs$dates_services$service_id)) {
    gtfs$dates_services <- gtfs$dates_services %>% unnest(., cols = c(service_id))
  }
  
  trip_dates_count <- 
    gtfs$dates_services %>% 
    tidyr::unnest(cols = service_id) %>% 
    dplyr::left_join(services,
                     by = 'service_id') %>% 
    dplyr::group_by(date) %>% 
    dplyr::reframe(count = sum(trips, na.rm = T)) %>% 
    dplyr::right_join(
      tibble(date = seq(min(gtfs$dates_services$date), max(gtfs$dates_services$date), 86400)),
      by = 'date'
    ) %>% 
    dplyr::mutate(
      #count = if_else(is.na(count), 0, count), 
      date = lubridate::ymd(date),
      day_of_month = lubridate::day(date),
      month = lubridate::month(date, label = T, abbr = F),
      year = lubridate::year(date),
      weekday = lubridate::wday(date, label = T, abbr = T, week_start = 7),
      first_day_of_month = lubridate::wday(date - day_of_month,  week_start = 7),
      week_of_month = ceiling((day_of_month - as.numeric(weekday) - first_day_of_month) / 7)
    )
  
  
  plot <- 
    ggplot2::ggplot(trip_dates_count, aes(x = weekday, y = -week_of_month)) +
    ggplot2::theme_bw() +
    ggplot2::geom_tile(aes(fill = count), color = 'gray50') +
    ggplot2::geom_text(aes(label = day_of_month), size = 3, colour = "grey20") +
    ggplot2::scale_fill_gradient(low = "pink", high = "red", na.value = "black")+
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   panel.grid = element_blank(),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
    ggplot2::labs(x = NULL, y = NULL, fill = "# trips") +
    ggplot2::coord_fixed()
  
  if(facet_by_year == FALSE){
    plot <-
      plot +
      ggplot2::facet_wrap(year ~ month, ncol = ncol)
  }
  
  if(facet_by_year == TRUE){
    message("face_by_year = TRUE forces ncol = 0")
    plot <-
      plot +
      ggplot2::facet_grid(year ~ month)
  } 
  
  return(plot)
  
}
