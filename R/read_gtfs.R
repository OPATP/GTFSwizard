file.path = 'ber_gtfs.zip'

read_gtfs <- function(file.path, files = NULL, fields = NULL, skip = NULL, quiet = TRUE, 
                      encoding = "unknown"){
  
  obj <- gtfstools::read_gtfs(path = file.path, files = files, fields = fields, skip= skip, quiet = quiet,
                       encoding = encoding)
  obj <- structure(obj,class = 'gtfs_obj')
  return(obj)
  
}

