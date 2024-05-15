\name{gtfs_to_wizard}
\alias{gtfs_to_wizard}
\title{
Converts to wizardgtfs
}
\description{
Converts an object read by another package such as gtfsio, tidytransit and others. Into a wizardgtfs object.
}
\usage{
gtfs_to_wizard(gtfs_list)
}

\arguments{
  \item{gtfs_list}{
 A list or other gtfs format read by any package to be converted in wizardgtfs.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A wizardgtfs object: a list of tibbles in which each entry represents a txt file read from the .zip archive and additionally a tibble containing the services for each date (dates_services).
}
\references{

}
\author{
"Guimarães, Gustavo <caiogcg.mobilidade@gmail.com>"
}
\note{
%%  ~~further notes~~
}


\examples{
gtfs_path <- "path_to_my_gtfs.zip"

gtfs_list <- gtfsio::import_gtfs(gtfs_path)

summary(gtfs_list)

gtfs_wizard <- gtfs_to_wizard(gtfs_list)

summary(gtfs_wizard)
}
