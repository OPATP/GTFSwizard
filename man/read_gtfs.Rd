\name{read_gtfs}

\title{
 Read GTFS file
}
\description{
  Reads GTFS files from a .zip file.
}
\usage{
read_gtfs(
  file.path,
  files = NULL,
  quiet = TRUE,
  ...
)
}

\arguments{
  \item{file.path}{
 A path to a .zip GTFS file.
}

  \item{files}{
 A character. A vector containing the text files to be read from the GTFS zip (without the .txt extension). The default is NULL and all files are read.
}
  \item{...}{
 Can be used to pass on arguments to gtfsio::import_gtfs(). The parameters files and quiet are passed on by default
}
}

\value{
A wizardgtfs object: a list of tibbles in which each entry represents a txt file read from the .zip archive and additionally a tibble containing the services for each date (dates_services).
}
\references{
gtfsio::read_gtfs
tidytransit::read_gtfs
}
\author{
"Guimarães, Gustavo <caiogcg.mobilidade@gmail.com>"
}




\seealso{
gtfs_to_wizard
}
\examples{

gtfs_path <- system.file("extdata", "for_gtfs.zip", package = "GTFSwizard")

gtfs <- read_gtfs(gtfs_path)

summary(gtfs)
}

