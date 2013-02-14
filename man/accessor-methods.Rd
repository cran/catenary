\docType{methods}
\name{accessors}
\alias{accessors}
\alias{L}
\alias{L,catenary-method}
\title{Get length for catenary}
\arguments{
  \item{x}{A \code{catenary} object}
}
\value{
  length
}
\description{
  Returns the length of catenary
}
\examples{
tmp <- catenary(c1=1,c2=2,lambda=3,x0=0,x1=3)
L(tmp)
}
\author{
  Jono Tuke, Matthew Roughan
}

