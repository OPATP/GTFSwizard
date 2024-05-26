\name{get_shapes}
\alias{get_shapes}
\title{
Reconstructs GTFS shapes table
}
\description{
Reconstructs the shapes table using Euclidean approximation, based on the coordinates and sequence of stops for each trip.
}
\usage{
get_shapes(gtfs)
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
  A new gtfs object that includes the \code{gtfs$shapes} table.
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
# DO NOT RUN

gtfs$shapes
#NULL

gtfs <- get_shapes(gtfs)

gtfs$shapes

## A tibble: 6,830 × 5
#   shape_id shape_pt_lon shape_pt_lat shape_pt_sequence shape_dist_traveled
#   <chr>           <dbl>        <dbl>             <int>               <dbl>
# 1 shape-1         -38.7        -3.96                 1                  0 
# 2 shape-1         -38.7        -3.95                 2                499.
# 3 shape-1         -38.7        -3.96                 3               1428 
# 4 shape-1         -38.7        -3.98                 4               3404.
# 5 shape-1         -38.7        -3.97                 5               5490.
# 6 shape-1         -38.7        -3.97                 6               6248.
# 7 shape-1         -38.7        -3.98                 7               7871.
# 8 shape-1         -38.8        -3.97                 8              11398.
# 9 shape-1         -38.7        -3.97                 9              13093.
#10 shape-1         -38.8        -3.97                10              13395 
## ℹ 6,820 more rows
## ℹ Use `print(n = ...)` to see more rows
}

\keyword{gtfs}