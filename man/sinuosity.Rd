% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sinuosity.R
\name{sinuosity}
\alias{sinuosity}
\title{Compute corrected rediscretized sinuosity index for a given tracklet.}
\usage{
sinuosity(df, scale = 1, segL = NULL, timeCol = "frame", compass = NULL)
}
\arguments{
\item{df}{A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given tracklet, as well as
a column containing time information, whatever the unit, over the tracklet.}

\item{scale}{A ratio corresponding to the scaling factor which should be applied to the trajectory coordinates.
(e.g., size in cm / size in pixels; see \code{\link[trajr]{TrajScale}, default = 1}.}

\item{segL}{A numeric value expressed in the unit specified by user and corresponding
to the length of the resampling step needed to discretize the input trajectory (optional)
see \code{\link[trajr]{TrajRediscretize}}.}

\item{timeCol}{A character string corresponding to the name of the column containing Time information (default = 'frame').}

\item{compass}{A value used to specify the compass direction (in radians). If not NULL, turning angles are calculated for a directed walk, otherwise, a random walk is assumed (default = NULL).}
}
\value{
This function returns a value of sinuosity for a given tracklet according to \code{\link[trajr]{TrajSinuosity2}} from the \code{\link[trajr]{trajr}} package.
}
\description{
Given a data frames containing tracking information for a given tracklet,
this function rediscretize the tracklet and returns a vector containing the value of
sinuosity along the trajectory.
}
\examples{

set.seed(2023)
# generate a dummy tracklet
## start to specify some parameters to generate the tracklet
TrackL <-
  100 # the length of the tracklet or a sequence to randomly sample tracklet's length

TrackDatTemp <-
  trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)
TrackDat <-
  data.frame(
    x.pos = TrackDatTemp[["x"]] - min(TrackDatTemp[["x"]]),
    y.pos = TrackDatTemp[["y"]] - min(TrackDatTemp[["y"]]),
    frame = TrackDatTemp[["time"]]
  )

# compute the sinuosity of the particle' trajectory
MoveR::sinuosity(TrackDat, scale = 1, timeCol = "frame")

}
\references{
Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal's path. Journal of Theoretical Biology, 229(2), 209-220. doi:10.1016/j.jtbi.2004.03.016.
}
\seealso{
\code{\link[trajr]{TrajSinuosity2}}
}
\author{
Quentin PETITJEAN
}
