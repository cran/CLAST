\name{RANK}
\alias{RANK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Produces ranks of entries of vector
}
\description{
An alternative to the function rank (which replaces ties by average rank).
This function instead gives each tied value the same rank. The output will have the same length as x but contain integer values from 1 to length(unique(x)).
}
\usage{
RANK(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric vector
}
}

\value{Vector of integer ranks of the same length as x.
}
\author{
Chris J. Lloyd
}


\examples{
x=c(5,4,4,7,6)
rank(x)
# 3.0 1.5 1.5 5.0 4.0
RANK(x)
# 2 1 1 4 3
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
