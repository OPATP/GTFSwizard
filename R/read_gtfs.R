

read_gtfs <- function(file.path, files = NULL, quiet = TRUE, ...){
  
  obj <- gtfsio::import_gtfs(path = file.path,files = files, quiet = quiet, ...)
  obj <- gtfs_to_wizard(obj)
  return(obj)
  
}
