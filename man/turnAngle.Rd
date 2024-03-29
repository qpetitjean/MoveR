% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/turnAngle.R
\name{turnAngle}
\alias{turnAngle}
\title{Compute particle's turning angle over a trajectory.}
\usage{
turnAngle(
  df,
  timeCol = "frame",
  unit = c("radians", "degrees"),
  compass = NULL,
  scale = 1
)
}
\arguments{
\item{df}{A data frame containing x, y coordinates columns named "x.pos", "y.pos" for a given tracklet.}

\item{timeCol}{A character string corresponding to the name of the column containing Time information (default = 'frame').}

\item{unit}{A character string indicating whether the function should returns turning angle in radians or degrees (default = "radians").}

\item{compass}{A value used to specify the compass direction (in radians). If not NULL, turning angles are calculated for a directed walk, otherwise, a random walk is assumed (default = NULL).}

\item{scale}{A ratio corresponding to the scaling factor to be applied to the trajectory coordinates
(e.g., size in cm / size in pixels; see \code{\link[trajr]{TrajScale}, default = 1}.}
}
\value{
This function returns a vector containing the values of turning angle over a trajectory.
}
\description{
Given a data frames containing tracking information for a particle,
this function returns a vector containing the value of turning angle along the considered tracklet.
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

# compute the turning angle over the particle's trajectory
MoveR::turnAngle(TrackDat,
                 timeCol = "frame",
                 unit = "radians",
                 scale = 1)

}
\seealso{
\code{\link[trajr]{TrajAngles}}
}
\author{
Quentin PETITJEAN
}
