# GTFSwizard 1.1.1

* `create_gtfs()` introduced.

* CRAN resubmission release following archival caused by the archived
  `hrbrthemes` dependency. The dependency has been removed, and the package
  dependency set has been substantially reduced.

* GTFS editing and selection
  - `filter_stop()` and `filter_time()` now preserve partial trips for
    experimentation instead of restoring complete stop sequences.
  - `selection()` now supports grouping columns and computed groups in a style
    similar to `dplyr::group_by()`, while continuing to support logical and
    spatial selections without altering GTFS tables.

* Plotting and dashboard
  - Fixed repeated warnings and `-Inf` values when
    `shape_dist_traveled` contains only missing values.
  - `explore_gtfs()` removes duplicated system information, assigns distinct
    route colors, and filters non-finite plotting values.
  - All static plots use a shared visual theme and palette.

* Package quality
  - Added Francisco Moraes de Oliveira Neto as an author.
  - Expanded integrity, round-trip, selection, spatial, plotting, and
    regression tests.
  - Reviewed exported function documentation and GTFS logic against the
    current GTFS Schedule reference.

# GTFSwizard 1.1.0

* Now featuring travel time matrices, trip speed and dweel time edition, corridors, hubs, and more!

* New functions
  - tidy_raptor() estimates travel time matrices from gtfs feeds;
  - edit_speeds() Adjust Travel Speed in a GTFS Dataset
  - edit_dwelltime() modifies the dwell time of trips;
  - set_dwelltime() overwrites the dwell time of trips;
  - get_hubs() estimates high integration potential stops;
  - get_corridor() suggests "right-of-way" transit corridors;
  - plot_corridor() plots the suggested corridors from get_corridor() function;
  - get_1stdeparture() retrieves the start timepoint of each trip;
  - latlon2epsg() determines the appropriate EPSG code for a given sf object.
 
* New methods
  - get_headways() now accepts 'method = "by.stop"' and 'method = "by.shape"'.
  - get_frequency() now accepts 'method = "by.shape"' and 'method = "by.stop"'.
  
* Bug fix & improvements
  - Better column naming;
  - Minor bug fixed on filter_date() and get_shapes_sf();
  - get_headways() output fixed;
  - split_trip() can now be used after filter_time();
  - get_speed(), get_durations() and get_distances() now accepts 'trips = ' argument.

# GTFSwizard 1.0.0

* Initial CRAN submission.
