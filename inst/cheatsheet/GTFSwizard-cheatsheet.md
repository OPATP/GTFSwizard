# GTFSwizard Cheat Sheet

Compact workflow reference for GTFSwizard 1.2.1.

## 1. Load Or Create A Feed

```r
library(GTFSwizard)

gtfs <- read_gtfs("path/to/feed.zip")
gtfs <- as_wizardgtfs(gtfs_list)

gtfs <- create_gtfs(
  agency = agency_df,
  routes = routes_df,
  trips = trips_df,
  stop_times = stop_times_df,
  stops = stops_df,
  calendar = calendar_df
)
```

All three functions return a `wizardgtfs` object.

## 2. Inspect The Feed

```r
summary(gtfs)
plot(gtfs)

get_servicepattern(gtfs)
get_shapes_sf(gtfs)
get_stops_sf(gtfs)
```

Use `summary()` for table counts, service range, and spacing diagnostics. Use
`plot()` for a quick network map.

## 3. Select Parts Of The Feed

```r
gtfs |> selection(route_id = c("004", "011"))
gtfs |> selection(service_pattern = "servicepattern-1")
gtfs |> selection(stop_id = c("1000", "1001"))

filter_route(gtfs, "004")
filter_service(gtfs, "U")
filter_servicepattern(gtfs, "servicepattern-1")
filter_stop(gtfs, "1000")
filter_date(gtfs, as.Date("2020-01-15"))
filter_time(gtfs, from = "06:00:00", to = "09:00:00")
```

`selection()` is useful when combining filters. The `filter_*()` functions are
explicit one-step tools.

## 4. Analyze Operations

```r
get_frequency(gtfs)
get_headways(gtfs, method = "by_hour")
get_dwelltimes(gtfs, method = "by_route")
get_speeds(gtfs, method = "by_route")
get_durations(gtfs, method = "by_route")
get_distances(gtfs)
get_fleet(gtfs)
get_1stdeparture(gtfs)
```

Check each help page for its observational unit. Common methods include
`by_trip`, `by_route`, `by_hour`, and `detailed`.

## 5. Plot Planning Indicators

```r
plot_frequency(gtfs)
plot_routefrequency(gtfs)
plot_headways(gtfs)
plot_servicespan(gtfs)
plot_serviceheatmap(gtfs)
plot_servicesupply(gtfs)
plot_routeduration(gtfs)
plot_calendar(gtfs)
```

These functions return `ggplot2` objects and can be customized with regular
`ggplot2` layers.

## 6. Find Corridors And Hubs

```r
get_corridor(gtfs, i = 0.01, min_length = 1500)
plot_corridor(gtfs, i = 0.01, min_length = 1500)

get_hubs(gtfs, i = 0.05)
plot_hubs(gtfs, i = 0.05)
```

Increase `i` to show fewer, stronger corridors or hubs. Decrease it to show
more candidates.

## 7. Edit A Feed

```r
edit_speed(gtfs, route_id = "004", speed = 25)
edit_dwelltime(gtfs, route_id = "004", dwelltime = 30)
delay_trip(gtfs, trip_id = "T1", delay = 300)
split_trip(gtfs, trip_id = "T1", stop_id = "S2")
merge_gtfs(gtfs_a, gtfs_b)
```

Editing functions keep the GTFS structure consistent while preserving partial
trips when they are useful for experimentation.

## 8. Explore Interactively

```r
explore_gtfs(gtfs)
explore_gtfs()
```

Call `explore_gtfs()` without an argument to choose a GTFS `.zip` file from a
browse window.

## 9. Save Results

```r
write_gtfs(gtfs, "edited-feed.zip")
```

Use `write_gtfs()` after validating edits and plots.
