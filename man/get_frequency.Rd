\name{get_frequency}
\alias{get_frequency}
\title{
Computes route frequency
}
\description{
Computes the hourly (\code{simplify = FALSE}) or dayly (\code{simplify = TRUE}) frequency for each route and service from a wizardgtfs or gtfs object.
}
\usage{
get_frequency(
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
  A \code{tibble} with the hourly frequency (# trips) for each route and service. If \code{simplify} is \code{TRUE}, results are shown for the whole day.
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
frequency <- get_frequency(for_gtfs, simplify = TRUE)

print(frequency)

## A tibble: 667 × 4
#   route_id frequency service_pattern  pattern_frequency
#   <chr>        <int> <chr>                        <int>
# 1 004             66 servicepattern-1                65
# 2 011             53 servicepattern-1                65
# 3 011             30 servicepattern-2                13
# 4 011             17 servicepattern-3                13
# 5 012             53 servicepattern-1                65
# 6 012             28 servicepattern-2                13
# 7 012             20 servicepattern-3                13
# 8 014             30 servicepattern-1                65
# 9 014             19 servicepattern-2                13
#10 014             19 servicepattern-3                13
## ℹ 657 more rows
## ℹ Use `print(n = ...)` to see more rows
}

\keyword{gtfs}