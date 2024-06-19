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
    GTFSwizard::filter_route(gtfs, route) %>% 
    filter_servicepattern(., servicepattern) %>% 
    GTFSwizard::get_frequency(method = 'detailed') %>% 
    dplyr::mutate(hour = as.numeric(hour))
  
  plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_line(data = data, ggplot2::aes(x = hour, y = frequency, color = route_id), alpha = .5, linewidth = 1.25) +
    ggplot2::geom_point(data = data, ggplot2::aes(x = hour, y = frequency, color = route_id), alpha = .5, linewidth = 1.25) +
    ggplot2::labs(x = 'Hour of the day', y = 'Hourly Frequency', colour = 'Route(s)', linewidth = "", title = 'Route(s) Frequency') +
    hrbrthemes::theme_ipsum() +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24), limits = c(0, 24)) +
    ggplot2::scale_y_continuous(limits = c(0, max(data$frequency)))
  
  plotly <-
    suppressWarnings(
      plotly::ggplotly(plot,
                       tooltip = c('x', 'y', 'linewidth', 'colour')
      )
    )
  
  return(plotly)
}

# plot_headways 
plot_headways <- function(gtfs){
  
  data <-
    GTFSwizard::get_headways(gtfs, method = 'by.route') %>% 
    dplyr::mutate(average.headway = average.headway / 60,
                  weight = pattern_frequency * trips)
  
  overal.average <- 
    weighted.mean(data$average.headway, data$weight, na.rm = T)
  
  plot <- 
  ggplot2::ggplot() +
    ggplot2::geom_histogram(data = data, ggplot2::aes(x = average.headway, weight = weight, fill = service_pattern), color = 'black', linewidth = .5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = overal.average, linetype = paste0('Overall\nAverage\nHeadway of\n', round(overal.average, 1), ' minutes')), linewidth = 1, color = '#113322') +
    ggplot2::labs(x = 'Average Headway (min)', fill = '', title = 'System Average Headway', linetype = '', y = 'Frequency') +
    ggplot2::scale_linetype_manual(values = 'dashed') +
    hrbrthemes::theme_ipsum() +
    ggplot2::theme(axis.text.y = element_blank())
  
  plotly <-
    suppressWarnings(
      plotly::ggplotly(plot,
                       tooltip = c('x', 'y')
      )
    )
  
  return(plotly)
}

# get_headways(gtfs, method = 'detailed')
# 
# 
# # plot_dwelltimes ----
# 
# GTFSwizard::get_dwelltimes(gtfs, max.dwelltime = 60, method = 'by.hour')
# 
# 
# 
# GTFSwizard::get_dwelltimes(gtfs, max.dwelltime = 60, method = 'by.route')
# 
# 
# 
# get_dwelltimes(gtfs, max.dwelltime = 60, method = 'by.trip')
# 
# 
# 
# get_dwelltimes(gtfs, max.dwelltime = 60, method = 'detailed')
# 
# 
# # plot_durations ----
# 
# get_durations(gtfs, method = 'by.route')
# 
# 
# 
# get_durations(gtfs, method = 'by.trip')
# 
# 
# 
# get_durations(gtfs, method = 'detailed')
# 
# 
# # plot_distances ----
# 
# get_distances(gtfs, method = 'by.route')
# 
# 
# 
# GTFSwizard::get_distances(gtfs, method = 'detailed')
# 
# 
# # plot_speed ----
# temp <- Sys.time(); gc()
# get_speeds(gtfs, method = 'by.route')
# 
# 
# temp <- Sys.time(); gc()
# get_speeds(gtfs, method = 'by.trip')
# 
# 
# temp <- Sys.time(); gc()
# get_speeds(gtfs, method = 'detailed')
# 
# 
# # plot_fleet ----
# temp <- Sys.time(); gc()
# GTFSwizard::get_fleet(gtfs, method = 'by.hour')
# 
# 
# temp <- Sys.time(); gc()
# GTFSwizard::get_fleet(gtfs, method = 'by.route')
# 
# 
# temp <- Sys.time(); gc()
# GTFSwizard::get_fleet(gtfs, method = 'peak')
# 
# 
# temp <- Sys.time(); gc()
# GTFSwizard::get_fleet(gtfs, method = 'detailed')
# 
# 
# GTFSwizard::get_fleet(gtfs, method = 'by.hour') %>%
#   #filter(service_pattern == 'servicepattern-1') %>% 
#   ggplot() +
#   geom_line(aes(x = hour, y = fleet, color = service_pattern, group = service_pattern)) #+
# #facet_grid(service_pattern~.) +
# #scale_x_time() +
# scale_y_binned()
# 
# # filter_ x7 ----
# GTFSwizard::filter_servicepattern(gtfs, servicepattern = 'wrong-data') %>% summary
# GTFSwizard::filter_servicepattern(gtfs) #%>% summary
# GTFSwizard::filter_servicepattern(gtfs, servicepattern = 'servicepattern-1') %>% summary
# GTFSwizard::filter_servicepattern(gtfs, servicepattern = c('servicepattern-1', 'servicepattern-2')) %>% summary
# 
# GTFSwizard::filter_date(gtfs, date = 'wrong-data') %>% summary
# GTFSwizard::filter_date(gtfs) %>% summary
# GTFSwizard::filter_date(gtfs, date = '2021-01-01') %>% summary
# GTFSwizard::filter_date(gtfs, date = c('2021-01-01', '2021-01-02')) %>% summary
# 
# GTFSwizard::filter_service(gtfs, service = 'wrong_data') %>% summary
# GTFSwizard::filter_service(gtfs) %>% summary
# GTFSwizard::filter_service(gtfs, service = 'U') %>% summary
# GTFSwizard::filter_service(gtfs, service = c('S', 'D')) %>% summary
# 
# GTFSwizard::filter_route(gtfs, route = 'wrong-data') %>% summary
# GTFSwizard::filter_route(gtfs) %>% summary
# GTFSwizard::filter_route(gtfs, route = '011') %>% summary
# GTFSwizard::filter_route(gtfs, route = c('011','004', '070')) %>% summary
# 
# GTFSwizard::filter_trip(gtfs)
# GTFSwizard::filter_trip(gtfs, trip = 'wrong-data') %>% summary
# GTFSwizard::filter_trip(gtfs, trip = 'D030-T01V06B01-I') %>% summary
# GTFSwizard::filter_trip(gtfs, trip = c('D030-T01V06B01-I', 'D030-T01V06B01-V', 'D011-T01V06B01-I')) %>% summary
# 
# GTFSwizard::filter_stop(gtfs, stop = 'wrong-data') %>% summary
# GTFSwizard::filter_stop(gtfs) %>% summary
# GTFSwizard::filter_stop(gtfs, stop = 10) %>% summary
# GTFSwizard::filter_stop(gtfs, stop = c(10, 1000)) %>% summary
# 
# GTFSwizard::filter_time(gtfs) %>% summary
# GTFSwizard::filter_time(gtfs, to = 'wrong-data') %>% summary
# GTFSwizard::filter_time(gtfs, from = '13:12:11', to = '14:15:16') %>% summary
# 
# gtfs %>% summary
