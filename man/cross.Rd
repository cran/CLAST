\name{cross}
\alias{cross}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Special combination of Matrix and Vector
}
\description{
This operation is useful in recursively generating all the possible path histories of a sequential design.
}
\usage{
cross(A, v)
}

\arguments{
  \item{A}{
An arbitrary matrix
}
  \item{v}{
An arbitrary vector
}
}
\details{
For each component of v, the matrix A is duplicated with an extra right column equal to v[i]. This is added to the current output until the components of v are exhausted.
}
\value{
A matrix with dim(A)[2]+1 columns and dim(A)[1]*length(v) rows.

}
\author{
Chris J. Lloyd
}

\examples{
A=cbind(c(1,2),c(3,4))
v=c(5,6)
cross(A,v)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
