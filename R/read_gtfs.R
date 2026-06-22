#' Read a GTFS Feed
#'
#' Imports a GTFS zip archive and converts it to `wizardgtfs`.
#'
#' @param file_path Path to a GTFS `.zip` archive.
#' @param files Optional character vector of table names without `.txt`.
#' @param quiet Logical. Suppress importer messages.
#' @param ... Additional arguments passed to [gtfsio::import_gtfs()], including
#'   the legacy argument `file.path`.
#'
#' @return A validated `wizardgtfs` object.
#'
#' @examples
#' path <- tempfile(fileext = ".zip")
#' write_gtfs(for_rail_gtfs, path)
#' gtfs <- read_gtfs(path)
#'
#' @seealso [GTFSwizard::write_gtfs()], [GTFSwizard::as_wizardgtfs()]
#' @export
read_gtfs <- function(file_path = NULL, files = NULL, quiet = TRUE, ...){
  resolved <- resolve_legacy_argument(
    file_path, missing(file_path), list(...), "file.path", "file_path"
  )
  file_path <- resolved$value
  dots <- resolved$dots
  if(!is.character(file_path) || length(file_path) != 1L ||
     !file.exists(file_path)){
    gw_stop("`file_path` must point to an existing GTFS zip archive.")
  }
  gw_assert_flag(quiet, "quiet")
  object <- do.call(
    gtfsio::import_gtfs,
    c(list(path = file_path, files = files, quiet = quiet), dots)
  )
  as_wizardgtfs(object)
}
