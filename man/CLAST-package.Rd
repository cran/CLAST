\name{CLAST-package}
\alias{CLAST}
\docType{package}
\title{Confidence Limits After Sequential Trial
\packageTitle{CLAST}
}
\description{
\packageDescription{CLAST}
}
\details{This package allows the user to compare different methods of calculating exact upper and lower limits for a probability after a group sequential trial.
}
\author{
\packageAuthor{CLAST}
Maintainer: \packageMaintainer{CLAST}
}
\references{
Lloyd, C.J. (2020) Exact confidence limits after a group sequential single arm binary trial. Statistics in Medicine, Volume 38, 2389-2399. \doi{10.1002/sim.8909}
}
\keyword{package}
\examples{
# Example 1 in table 1 of Lloyd (2020)
  n=c(5,6,5,9)
  a=c(2,4,5,12)
  b=c(5,9,11,13)
  plt.sample.space.SM(n,a,b) # Produces figure 1 in reference
#
  p0=.4
  p1=.75
  errors.SM(n,a,b,p0,p1) # Gives type 1 and type 2 errors
  plt.sample.space.SM(n,a,b,p0,p1) # Add error rates into plot title
#
# Selection of type of limits is based on the diagnostic plots
# from the next command, which produces three panel graphic
# identical to Figure 2 in the main reference. The results here
# support method LR
  mv.plots.SM(n,a,b,p0=p0,p1=p1)
# Once we have an actual outcome we can calculate the limits.
  y=c(4,2,5) # Trial terminates on trial 2 since total successes is 11.
  inference(n,a,b,y,type="LR")
# Produces exact limits for specific outcome y.
}
