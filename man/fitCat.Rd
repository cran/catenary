% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitCat.R
\name{fitCat}
\alias{fitCat}
\title{Get catenary parameters for given endpoints and length}
\usage{
fitCat(endpoints, L)
}
\arguments{
\item{endpoints}{2x2 data frame with first column x 
second column is y}

\item{L}{length}
}
\value{
vector of parameters
}
\description{
Takes endpoints in data frame and length and returns parameters of 
catenary
}
\note{
February 11 2013
}
\examples{
x <- c(-1,1)
y <- c(2,2)
endpoints <- data.frame(x=x,y=y)
L <- 3
fitCat(endpoints,L)
}
\author{
Jono Tuke, Matthew Roughan
}
\keyword{internal}
