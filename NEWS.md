# GTFSwizard 1.0.1 (dev)

* Now featuring travel time matrices, corridors and hubs!

* New functions
  - tidy_raptor() estimates travel time matrices from gtfs feeds;
  - get_1stdeparture() retrieves the start timepoint of each trip;
  - get_corridor() suggests "right-of-way" transit corridors;
  - plot_corridor() plots the suggested corridors from get_corridor() function.
 
* New methods
  - get_headways() now accepts 'method = "by.stop"' and 'method = "by.shape"';
  - get_frequency() now accepts 'method = "by.shape"' and 'method = "by.stop"'.
  
* Bug fix & improvements
  - Better column naming;
  - Minor bug fixed on filter_date();
  - get_headways() output fixed;
  - split_trip() can now be used after filter_time().
  

# GTFSwizard 1.0.0

* Initial CRAN submission.
