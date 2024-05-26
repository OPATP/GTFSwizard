\name{get_headways}
\alias{get_headways}
\title{
Computes feed headways
}
\description{
Computes the headway for every stop (\code{simplify = FALSE}) or an hourly average (\code{simplify = TRUE}) for each route and service from a wizardgtfs or gtfs object.
}
\usage{
get_headway(
  gtfs,
  simplify = TRUE
  )
}

\arguments{
  \item{gtfs}{
  A wizardgtfs object or another list of GTFS files that can be automatically converted using \code{as.gtfs()}.
}
  \item{simplify}{
  A logical. Whether to return a simplified result (defaults to \code{TRUE}).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A \code{tibble} with the hourly average headway (in minutes) for each route, stop and service. If \code{simplify} is \code{TRUE}, results are averaged across stops.
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
headways <- get_headways(for_gtfs, simplify = FALSE)

print(headways)

## A tibble: 697 × 6
#   route_id stop_id  hour average.headway service_pattern  pattern_frequency
#   <chr>    <fct>   <dbl>           <dbl> <chr>                        <int>
# 1 164      M19         6            45   servicepattern-1           1430030
# 2 164      M19         7           135   servicepattern-1           1430030
# 3 164      M19         9            35.8 servicepattern-1           1430030
# 4 164      M19        10           399.  servicepattern-1           1430030
# 5 164      M19        16           135   servicepattern-1           1430030
# 6 164      M19        19            60   servicepattern-1           1430030
# 7 164      M19        20            60   servicepattern-1           1430030
# 8 164      M19         6           840   servicepattern-2            106210
# 9 164      M19         6           840   servicepattern-3            106210
#10 164      M10         6            45   servicepattern-1           1430030
## ℹ 687 more rows
## ℹ Use `print(n = ...)` to see more rows
}

\keyword{gtfs}