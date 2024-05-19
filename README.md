# GTFSwizard <img align="right" src="GTFSwizard_logo.png?raw=true" alt="logo" width="180">
[![Lifecycle:
experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html)

GTFSwizard is a set of tools for exploring and manipulating [General Transit Feed Specification (GTFS)](https://gtfs.org/) files in R.

Its main purpose is to provide researchers and practitioners with a seamless and easy way to visually explore and simulate changes in  frequency, headway, dwell time, speed, and routes within a GTFS file.

## Installation
``` r
install.packages('remotes') # if not already installed
# wait for the installation to complete

remotes::install_github('OPATP/GTFSwizard')
```
## Cheat Sheet

## Usage
GTFS feeds are read using the `read_gtfs()` function.\
`read_gtfs()` returns a `wizardgtfs` object, which is a slightly improved `gtfs` object.
``` r
library(GTFSwizard)

gtfs <- read_gtfs('path-to-gtfs.zip')

names(gtfs)
# [1] "agency"          "calendar"
# [3] "calendar_dates"  "fare_attributes"
# [5] "fare_rules"      "routes"
# [7] "shapes"          "stop_times"
# [9] "stops"           "trips"
# [11] "dates_services"

class(gtfs)
# [1] "wizardgtfs" "gtfs" "list"
```

GTFS feeds are explored using the `explore_gtfs()` function:
``` r
explore_gtfs(gtfs)
```

Routes frequency, headway and dell times are calculated using the `get_frequency()`, the `get_headway()`, and the `get_dwelltime()` functions:
``` r
get_frequency(gtfs)
## A tibble: 15,522 × 5
#   route_id  hour service_id frequency service_frequency
#   <chr>    <dbl> <chr>          <int>             <int>
# 1 004          7 U                  2               586
# 2 004          8 U                  4               586
# 3 004          9 U                  4               586
# 4 004         10 U                  4               586
# 5 004         11 U                  4               586
# 6 004         12 U                  4               586
# 7 004         13 U                  4               586
# 8 004         14 U                  5               586
# 9 004         15 U                  4               586
#10 004         16 U                  4               586
## ℹ 15,512 more rows
## ℹ Use `print(n = ...)` to see more rows

get_headway(gtfs)
## A tibble: 664,746 × 6
#   route_id stop_id service_id  hour average.headway.minutes service_frequency
#   <chr>    <fct>   <chr>      <dbl>                   <dbl>             <int>
# 1 004      3330    U              8                    149                586
# 2 004      3330    U              9                    148                586
# 3 004      3330    U             10                    143                586
# 4 004      3330    U             11                    179                586
# 5 004      3330    U             12                    112.               586
# 6 004      3330    U             13                    116.               586
# 7 004      3330    U             14                    146                586
# 8 004      3330    U             15                    144                586
# 9 004      3330    U             16                    141                586
#10 004      3303    U              8                    149                586
## ℹ 664,736 more rows
## ℹ Use `print(n = ...)` to see more rows

get_dwelltime(gtfs, max.dwelltime = 60)
# A tibble: 2,198,547 × 6
#   route_id stop_id  hour dwell_time service_id service_frequency
#   <chr>    <fct>   <dbl>      <dbl> <chr>                  <int>
# 1 011      3500        5         10 D                        121
# 2 011      1013        5         20 D                        121
# 3 011      1015        5         20 D                        121
# 4 011      4251        5         20 D                        121
# 5 011      990         5         10 D                        121
# 6 011      991         5         10 D                        121
# 7 011      989         5         20 D                        121
# 8 011      1600        5         20 D                        121
# 9 011      1608        5         30 D                        121
#10 011      4767        5         20 D                        121
## ℹ 2,198,537 more rows
## ℹ Use `print(n = ...)` to see more rows
```

## Related Packages
GTFSwizard mainly rellies on [dplyr](https://dplyr.tidyverse.org/), [tidytransit](https://cran.r-project.org/web/packages/tidytransit/vignettes/introduction.html) and [gtfsio](https://r-transit.github.io/gtfsio/articles/gtfsio.html) for data wrangling, [leaflet](https://leafletjs.com/) for map rendering, [ggplot2](https://ggplot2.tidyverse.org/) and [plotly](https://plotly.com/r/) for data visualization, and [shiny](https://shiny.posit.co/) for the `explore_gtfs()` application assembling.

## Acknowledgement <a href="https://www.ipea.gov.br"><img align="right" src="opatp.png" alt="OPA-TP" width="150" /></a>
**GTFSwizard** is developed by Nelson Quesado and Caio Guimarães at OPA-TP research group, Universidade Federal do Ceará.
