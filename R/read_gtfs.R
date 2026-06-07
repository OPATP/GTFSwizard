#' Read a GTFS Feed
#'
#' Imports a GTFS zip archive and converts it to `wizardgtfs`.
#'
#' @param file.path Path to a GTFS `.zip` archive.
#' @param files Optional character vector of table names without `.txt`.
#' @param quiet Logical. Suppress importer messages.
#' @param ... Additional arguments passed to [gtfsio::import_gtfs()].
#'
#' @return A validated `wizardgtfs` object.
#'
#' @examples
#' \dontrun{
#' gtfs <- read_gtfs("gtfs.zip")
#' }
#'
#' @seealso [GTFSwizard::write_gtfs()], [GTFSwizard::as_wizardgtfs()]
#' @export
read_gtfs <- function(file.path, files = NULL, quiet = TRUE, ...){
  if(!is.character(file.path) || length(file.path) != 1L ||
     !file.exists(file.path)){
    gw_stop("`file.path` must point to an existing GTFS zip archive.")
  }
  checkmate::assert_flag(quiet)
  object <- gtfsio::import_gtfs(
    path = file.path, files = files, quiet = quiet, ...
  )
  as_wizardgtfs(object)
}
