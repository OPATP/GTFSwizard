# plot_frequency 
plot_frequency <- function(gtfs){
  
  data <-
    GTFSwizard::get_frequency(gtfs, method = 'detailed') %>% 
    dplyr::mutate(hour = as.numeric(hour))
  
  overal.average <- 
    weighted.mean(data$frequency, data$pattern_frequency, na.rm = T)
  
  plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = data, ggplot2::aes(x = hour, y = frequency, color = 'Hourly\nDistribution\n', group = hour, weight = pattern_frequency), fill = 'gray', alpha = .65) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = overal.average, color = paste0('Overall\nAverage\nFrequency\n', round(overal.average, 1), ' trips')), linetype = 'dashed', linewidth = .75) +
    ggplot2::geom_line(data = dplyr::group_by(data, hour) %>% dplyr::reframe(frequency = round(weighted.mean(frequency, pattern_frequency, na.rm = T), 1)), ggplot2::aes(hour, frequency, color = 'Hourly\nAverage\nFrequency\n', group = NA), linewidth = 1) +
    ggplot2::labs(x = 'Hour of the Day', y = 'Hourly Frequency', colour = '', title = 'System Frequency') +
    hrbrthemes::theme_ipsum() +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24), limits = c(0, 24)) +
    ggplot2::scale_y_continuous(limits = c(0, max(data$frequency))) +
    ggplot2::scale_color_manual(values = c('#00BFC4', 'black', '#F8766D'))
  
  plotly <-
    suppressWarnings(
      plotly::ggplotly(plot,
                       tooltip = c('x', 'y')
      )
    )
  
  return(plotly)
}

plot_routefrequency <- function(gtfs, route = NULL, servicepattern = NULL){
  
  if(length(servicepattern) > 1){
    message('plot_routefrequency must have a single servicepattern argument.')
    stop()
  }
  
  data <-
    tibble(route_id = rep(route, times = rep(24, times = length(route))),
           hour = rep(1:24, times = length(route))) %>% 
    dplyr::left_join(
      filter_route(gtfs, route) %>% 
        filter_servicepattern(., servicepattern) %>% 
        GTFSwizard::get_frequency(method = 'detailed') %>% 
        dplyr::mutate(hour = as.numeric(hour)) 
    ) %>% 
      dplyr::mutate(frequency = dplyr::if_else(is.na(frequency), 0, frequency))
  
  plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_line(data = data, ggplot2::aes(x = hour, y = frequency, color = route_id), alpha = .5, linewidth = 1.25) +
    ggplot2::geom_point(data = data, ggplot2::aes(x = hour, y = frequency, color = route_id), alpha = .5) +
    ggplot2::labs(x = 'Hour of the day', y = 'Hourly Frequency', colour = 'Route(s)', linewidth = "", title = 'Route(s) Frequency') +
    hrbrthemes::theme_ipsum() +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24), limits = c(0, 24)) +
    ggplot2::scale_y_continuous(limits = c(0, max(data$frequency)))
  
  plotly <-
    suppressWarnings(
      plotly::ggplotly(plot,
                       tooltip = c('x', 'y', 'colour')
      )
    )
  
  return(plotly)
}