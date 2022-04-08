\name{mv.SM}
\alias{mv.SM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean value of upper limits.
}
\description{
Calculates mean value of a provided vector of upper limits as a function of p
}
\usage{
mv.SM(obj, lims = NULL, p = NULL, B = 99, offset = TRUE, wgt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
list with components $S, $N, $count and optionally $lims,
typically the output of sample.space.SM
}
  \item{lims}{
if not a component of obj
}
  \item{p}{
vector of values of p at which to calculate mean value
}
  \item{B}{
number of evenly spaced values of p if not provided
}
  \item{offset}{
if TRUE then ML mean value is subtracted
}
  \item{wgt}{
if TRUE than assign zero probability weight to extreme limits of 1 or 0.
}
}

\value{
list with elements $x (containing grid of B values of probability) and $y (containing corresponding mean values)
}

\author{Chris J. Lloyd}

\examples{
n=c(5,6,5,9)
a=c(2,4,5,12)
b=c(5,9,11,13)
# Enumerate all possible elements of the sufficiency reduced samples
# space i.e. all values of S and M. Also listed are the counts and
# subcounts of these outcomes, the test decision and the vector
# n, a, and b in $design

data.SM=sample.space.SM(n,a,b) # There are 26 elements.

# Calculate all approximate LR upper limits for these 26 outcomes.
all.LR.high=LR.stats.SM(data.SM,type="upper")$lims
# Calculate all approximate LR upper limits for these 26 outcomes.
all.LR.low=LR.stats.SM(data.SM,type="lower")$lims

# Calculate the mean values of these lims as a function of p
mv.high=mv.SM(data.SM,all.LR.high,p=(1:99)/100,offset=FALSE)
mv.low=mv.SM(data.SM,all.LR.low,p=(1:99)/100,offset=FALSE)
plot(c(0,1),c(0,1),ylab="mean value",xlab="p",type="n")
lines(mv.high)
lines(mv.low)
abline(0,1,lty=3)
title(main="Mean value of upper and lower limits by p")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
