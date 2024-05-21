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

summary(gtfs)
#A wizardgtfs object with:  
#
#10  GTFS tables 
#With the following names and respective numbers of entries in each: 
#         agency        calendar  calendar_dates fare_attributes 
#              1               3               6               2 
#     fare_rules          routes          shapes      stop_times 
#            345             345          125776         2659737 
#          stops           trips 
#           4676           85410 
#345  routes 
#4676  stops 
#85410  trips 
#823  valid days of service 
#271.4  meters is the average distance between sequencial stops in a given route 
```

GTFS feeds are explored using the `explore_gtfs()` function:
``` r
explore_gtfs(gtfs)
```

Routes frequency, headways, dell times, and speeds are calculated using the `get_frequency()`, the `get_headway()`, the `get_dwelltime()`, and the `get_speed()` functions:
``` r
get_frequency(gtfs)
## A tibble: 122 × 5
#   route_id  hour frequency service_id service_frequency
#   <chr>    <dbl>     <int> <chr>                  <int>
# 1 164          6         1 28046026               20429
# 2 164          6         1 28046044                4085
# 3 164          6         1 28046048                4085
# 4 164          7         1 28046028               20429
# 5 164          9         1 28046030               20429
# 6 164         10         1 28046032               20429
# 7 164         16         1 28046034               20429
# 8 164         19         1 28046036               20429
# 9 164         20         1 28046038               20429
#10 164         20         1 28046040               20429
## ℹ 112 more rows
## ℹ Use `print(n = ...)` to see more rows

get_headway(gtfs)
## A tibble: 5,040 × 6
#   route_id stop_id       hour average.headway service_id service_frequency
#   <chr>    <fct>        <dbl>           <dbl> <chr>                  <int>
# 1 1920_700 100000110503     5            40.5 1                        139
# 2 1920_700 100000110503     6           257   1                        139
# 3 1920_700 100000110503    10           224.  1                        139
# 4 1920_700 100000110503    14            91   1                        139
# 5 1920_700 100000110503    16            85   1                        139
# 6 1920_700 100000110602     5            40.5 1                        139
# 7 1920_700 100000110602     6           257   1                        139
# 8 1920_700 100000110602    10           224.  1                        139
# 9 1920_700 100000110602    14            91   1                        139
#10 1920_700 100000110602    16            85   1                        139
## ℹ 5,030 more rows
## ℹ Use `print(n = ...)` to see more rows

get_dwelltime(gtfs, max.dwelltime = 60)
## A tibble: 1,736 × 6
#   route_id stop_id  hour dwell_time service_id service_frequency
#   <chr>    <fct>   <dbl>      <dbl> <chr>                  <int>
# 1 164      M19         6          0 28046026               20429
# 2 164      M10         6         28 28046026               20429
# 3 164      M18         6         22 28046026               20429
# 4 164      M17         6         28 28046026               20429
# 5 164      M16         6         26 28046026               20429
# 6 164      M15         6         27 28046026               20429
# 7 164      M14         6         25 28046026               20429
# 8 164      M13         6         41 28046026               20429
# 9 164      M38         6         23 28046026               20429
#10 164      M39         6         22 28046026               20429
## ℹ 1,726 more rows
## ℹ Use `print(n = ...)` to see more rows

get_speed(gtfs)
## A tibble: 1,614 × 9
#   route_id from_stop_id to_stop_id  hour duration distance speed service_id service_frequency
#   <fct>    <chr>        <chr>      <dbl>    <dbl>    <dbl> <dbl> <chr>                  <int>
# 1 164      M19          M10            6       68     789.  41.8 28046026               20429
# 2 164      M10          M18            6       66     530.  28.9 28046026               20429
# 3 164      M18          M17            6       54     532.  35.5 28046026               20429
# 4 164      M17          M16            6       66     674.  36.8 28046026               20429
# 5 164      M16          M15            6       69     700.  36.5 28046026               20429
# 6 164      M15          M14            6      117    1068.  32.9 28046026               20429
# 7 164      M14          M13            6      120     975.  29.3 28046026               20429
# 8 164      M13          M38            6       87     836.  34.6 28046026               20429
# 9 164      M38          M39            6       68     729.  38.6 28046026               20429
#10 164      M39          M40            6       71     694.  35.2 28046026               20429
## ℹ 1,604 more rows
## ℹ Use `print(n = ...)` to see more rows
```

## Related Packages
GTFSwizard mainly rellies on [dplyr](https://dplyr.tidyverse.org/), [tidytransit](https://cran.r-project.org/web/packages/tidytransit/vignettes/introduction.html) and [gtfsio](https://r-transit.github.io/gtfsio/articles/gtfsio.html) for data wrangling, [leaflet](https://leafletjs.com/) for map rendering, [ggplot2](https://ggplot2.tidyverse.org/) and [plotly](https://plotly.com/r/) for data visualization, and [shiny](https://shiny.posit.co/) for the `explore_gtfs()` application assembling.

## Acknowledgement <a href="https://www.ipea.gov.br"><img align="right" src="opatp.png" alt="OPA-TP" width="150" /></a>
**GTFSwizard** is developed by Nelson Quesado and Caio Guimarães at OPA-TP research group, Universidade Federal do Ceará.
