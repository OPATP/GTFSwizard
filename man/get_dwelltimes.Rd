\name{get_dwelltimes}
\alias{get_dwelltimes}
\title{
Computes feed dwell times
}
\description{
Computes each (\code{simplify = FALSE}) or average (\code{simplify = TRUE}) dwell time for every hour and service from a wizardgtfs or gtfs object.
}
\usage{
get_dwelltime(
  gtfs,
  max.dwelltime = 90, 
  simplify = TRUE
  )
}

\arguments{
  \item{gtfs}{
  A wizardgtfs object or another list of GTFS files that can be automatically converted using \code{as.gtfs()}.
}
  \item{max.dwelltime}{
  The maximum duration (in seconds) of dwell times considered.   
  }
\item{simplify}{
  A logical. Whether to return a simplified result (defaults to \code{TRUE}).
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A \code{tibble} with every dwell time (in seconds) for each route and service. If \code{simplify} is \code{TRUE},
  hourly average dwell time is computed.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
"Quesado Filho, Nelson O. <nquesado@gmail.com>"
}
\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
Related functions: \code{\link{read_gtfs}}, \code{\link{get_servicepattern}}
}
\examples{
dwell_time <- get_dwelltime(gtfs, max.dwelltime = 30, simplify = F)

print(dwell_time)

## A tibble: 1,596 × 6
#   route_id stop_id  hour dwell_time service_pattern  pattern_frequency
#   <chr>    <fct>   <dbl>      <dbl> <chr>                        <int>
# 1 164      M19         6          0 servicepattern-1           1430030
# 2 164      M10         6         28 servicepattern-1           1430030
# 3 164      M18         6         22 servicepattern-1           1430030
# 4 164      M17         6         28 servicepattern-1           1430030
# 5 164      M16         6         26 servicepattern-1           1430030
# 6 164      M15         6         27 servicepattern-1           1430030
# 7 164      M14         6         25 servicepattern-1           1430030
# 8 164      M38         6         23 servicepattern-1           1430030
# 9 164      M39         6         22 servicepattern-1           1430030
#10 164      M40         6         22 servicepattern-1           1430030
## ℹ 1,586 more rows
## ℹ Use `print(n = ...)` to see more rows
}

\keyword{gtfs}