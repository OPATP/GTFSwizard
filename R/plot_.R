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

#route = c('1', '10')
plot_routefrequency <- function(gtfs, route = NULL, servicepattern = NULL){
  
  if(length(servicepattern) > 1){
    message('plot_routefrequency must have a single servicepattern argument.')
    stop()
  }
  
  data <-
    tibble(route_id = rep(route, times = rep(24, times = length(route))),
           hour = rep(1:24, times = length(route))) %>% 
    dplyr::left_join(
      GTFSwizard::filter_route(gtfs, route) %>% 
        #GTFSwizard::filter_servicepattern(., servicepattern) %>% 
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
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24), limits = c(0, 24))
  
  plotly <-
    suppressWarnings(
      plotly::ggplotly(plot,
                       tooltip = c('x', 'y', 'colour')
      )
    )
  
  return(plotly)
}

# plot_headways 
plot_headways <- function(gtfs){
  
  #data <-
  GTFSwizard::get_headways(gtfs, method = 'by.hour') %>% 
    dplyr::mutate(average.headway = round(average.headway / 60, 0),
                  weight = pattern_frequency * trips,
                  hour = as.numeric(hour)) 
  
  overal.average <- 
    weighted.mean(data$average.headway, data$weight, na.rm = T) %>% 
    round(., 1)
  
  
  plot <- 
    ggplot(data) +
    geom_line(aes(x = hour, y = average.headway, color = service_pattern, group = service_pattern, alpha = service_pattern), size = 1.25) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = overal.average, linetype = paste0('Overall\nAverage\nHeadway of\n', round(overal.average, 1), ' minutes')), linewidth = 1, color = '#113322') +
    ggplot2::labs(x = 'Hour of the Day ', title = 'System Average Headway', linetype = '', y = 'Average Headway (min)') +
    ggplot2::scale_linetype_manual(values = 'dashed') +
    ggplot2::scale_alpha_manual(values = c(.9, rep(.15, length(unique(data$service_pattern)) - 1))) +
    hrbrthemes::theme_ipsum() +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24), limits = c(0, 24)) +
    ggplot2::theme(legend.position = 'none')
  
  plotly <-
    suppressWarnings(
      plotly::ggplotly(plot,
                       tooltip = c('x', 'y', 'yintercept')
      )
    )
  
  return(plotly)
}

