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
GTFS feeds are read using the `read_gtfs()` function:
``` r
library(GTFSwizard)

gtfs <- read_gtfs('path-to-gtfs.zip')

names(gtfs)
#> [1] "agency"          "calendar"
#> [3] "calendar_dates"  "fare_attributes"
#> [5] "fare_rules"      "routes"
#> [7] "shapes"          "stop_times"
#> [9] "stops"           "trips"
#> [11] "dates_services"
```

`read_gtfs()` returns a `wizardgtfs` object, which is a slightly improved `gtfs` object.

```r
class(gtfs)
#> [1] "wizardgtfs" "gtfs" "list"
```

GTFS feeds are explored using the `explore_gtfs()` function:
``` r
explore_gtfs(gtfs)
```

Routes frequency are calculated using the `get_frequency()` function:
``` r
get_frquency(gtfs)
# A tibble: 6,097 × 3
#   route_id  hour frequency
#   <chr>    <dbl>     <int>
# 1 004          7         2
# 2 004          8         4
# 3 004          9         4
# 4 004         10         4
# 5 004         11         4
# 6 004         12         4
# 7 004         13         4
# 8 004         14         5
# 9 004         15         4
#10 004         16         4
# ℹ 6,087 more rows
# ℹ Use `print(n = ...)` to see more rows
```

## Related Packages
GTFSwizard mainly rellies on [dplyr](https://dplyr.tidyverse.org/), [tidytransit](https://cran.r-project.org/web/packages/tidytransit/vignettes/introduction.html) and [gtfsio](https://r-transit.github.io/gtfsio/articles/gtfsio.html) for data wrangling, [leaflet](https://leafletjs.com/) for map rendering, [ggplot2](https://ggplot2.tidyverse.org/) and [plotly](https://plotly.com/r/) for data visualization, and [shiny](https://shiny.posit.co/) for the `explore_gtfs()` application assembling.

## Acknowledgement <a href="https://www.ipea.gov.br"><img align="right" src="opatp.png" alt="OPA-TP" width="150" /></a>
**GTFSwizard** is developed by Nelson Quesado and Caio Guimarães at OPA-TP research group, Universidade Federal do Ceará.
