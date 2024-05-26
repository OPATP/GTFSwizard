\name{get_servicepattern}
\alias{get_servicepattern}
\title{
Identifies the service pattern
}
\description{
Creates a service pattern id that identifies all dates each trip runs on.
}
\usage{
get_servicepattern(gtfs)
}

\arguments{
  \item{gtfs}{
  A wizardgtfs object or another list of GTFS files that can be automatically converted using \code{as.gtfs()}.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A \code{tibble} that assigns a service_pattern to each service_id.
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
Related functions: \code{\link{read_gtfs}}
}
\examples{
servicepattern <- get_servicepattern(for_gtfs)

print(servicepattern)

## A tibble: 3 × 3
#  service_id     n service_pattern 
#  <chr>      <int> <chr>           
#1 U             65 servicepattern-1
#2 S             13 servicepattern-2
#3 D             13 servicepattern-3
}

\keyword{gtfs}