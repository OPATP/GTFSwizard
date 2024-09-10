empty_gtfs <- function(full = F) {
  
  checkmate::assert_logical(full)
  
  if(!full) {
    message("Loading only required fields. To get required and optional fields please use 'full = TRUE'.")
    return(GTFSwizard::empty_gtfs_minimum)
  } else {
    message("Loading required and optional fields. To get only required fields please use 'full = FALSE'.")
    return(GTFSwizard::empty_gtfs_full)
  }
  
}
