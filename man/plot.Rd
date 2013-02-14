\name{plot}
\alias{plot}
\alias{plot,catenary-method}
\alias{plot,fittedCatenary-method}
\title{Plot method for catenary}
\arguments{
  \item{fit}{type of fit to show at present two choices
  "cat" and "para"}

  \item{envelope}{type of envelope to show at present two
  choices "cat" and "para"}
}
\description{
  basic plot with endpoints

  Method that can plot fits and function envelopes
}
\examples{
tmp <- catenary(c1=1,c2=3,lambda=1,x0=0,x1=4)
plot(tmp)
}
\author{
  Jono Tuke, Matthew Roughan

  Jono Tuke, Matthew Roughan
}

