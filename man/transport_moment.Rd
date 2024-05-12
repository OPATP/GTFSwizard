\name{transport_moment}

\title{
Compute the transport moment
}
\description{
Receives a wizardgtfs object and computes the transportation time (Nº. of trips * Route length) for a given date or a list. 
}
\usage{
transport_moment(gtfs, dates = Sys.Date(), routes = NULL, by_route = FALSE, simplify = FALSE)
}

\arguments{
  \item{gtfs}{
  A wizardgtfs object or another list of GTFS files that can be converted using gtfs_to_wizard.
}
  \item{dates}{
 A vector of dates to be used to compute transport moments. Can be POSIXct, Date or character. The default is Sys.Date(). Note that the date must be between the valid days in "calendar.txt" or "calendar_dates.txt". If the object is a wizardgtfs see obj$dates_services.
}
  \item{routes}{
 A character. A list of route_ids to be considered in computation. The default is NULL and all GTFS routes are considered
}
  \item{by_route}{
  A logical. Whether to return a result by route_id.
}
  \item{simplify}{
  A logical. Whether to return a simplified result.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A tibble with the moment of transport for each date in dates. And if by_route is TRUE for each route in routes.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
"Guimarães, Gustavo <caiogcg.mobilidade@gmail.com>"
}
\note{
%%  ~~further notes~~
}



\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


gtfs_path <- system.file("extdata", "for_gtfs.zip", package = "GTFSwizard")

gtfs <- read_gtfs(gtfs_path)

moment <- transport_moment(gtfs)

moment <- transport_moment(gtfs,
          dates = seq(as.POSIXct("2020-11-19"),
          as.POSIXct("2020-11-30"),"1 day"),
          by_routes = TRUE)

}



