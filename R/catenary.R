#' Create a class for catenary
#' 
#' Creates a basic catenary with endpoints
#'
#' @exportClass catenary
#' @name catenary-class
#' @rdname catenary-class
#' @section Slots: \itemize{
#'    \item c1: shape parameter
#'    \item c2: x-location parameter
#'    \item lambda: y-location parameter 
#'    \item endpoints: left and right endpoint in data
#'    frame
#'    \item L: length of caternary
#' }
#' @return an object of class \code{catenary}
#' @examples
#' getSlots("catenary")
setClass(
  Class="catenary",
  representation(
    c1 = "numeric",
    c2 = "numeric",
    lambda = "numeric",
    endpoints = "data.frame",
    L="numeric"
  ),
  validity = function(object){
    if(length(object@c1) >1 | length(object@c2) > 1 | length(object@lambda) > 1){
      stop ("c1, c2, and lambda must all be scalars")
    }
    if(length(object@c1) == 0 | length(object@c2) == 0 | 
         length(object@lambda) == 0){
      stop ("c1, c2, and lambda must be given")
    }
    return(TRUE)
  }
)
#' Creates a catenary object
#' 
#' First constructor that takes c1, c2, lambda, x0 and x1
#' Second constructor takes endpoints and either length or gives 
#' natural or maximum catenary (one that just touches the ground)
#' 
#' @param c1 shape parameter
#' @param c2 x-location parameter
#' @param lambda y-location parameter 
#' @param x0 left point
#' @param x1 right point
#' @param endpoints 2 x 2 matrix or data frame with column x and y
#' and rows left and right
#' @param L length of catenary
#' @param type max or natural if length not given
#' @return an instance of \code{catenary} class
#' @export
#' @examples
#' cat1 <- catenary()
#' plot(cat1)
#' cat2 <- catenary(c1=1,c2=2,lambda=3,x0=0,x1=4)
#' plot(cat2)
#' x <- c(-1,1)
#' y <- c(2,2)
#' endpoints <- data.frame(x=x,y=y)
#' cat3 <- catenary(endpoints=endpoints,L=5)
#' plot(cat3)
#' cat4 <- catenary(endpoints=endpoints,type='natural')
#' plot(cat4)
#' cat5 <- catenary(endpoints=endpoints,type='max')
#' plot(cat5)
catenary <- function(c1=1,c2=0,lambda=0,x0=-1,x1=1,endpoints=NULL,L=NULL,type="natural"){
  if(x1 <= x0){
    stop("x0 must be to the left of x1")
  }
  if(!is.null(endpoints)){
    # Use end points to calcalate para
    if(is.null(L)){
      # in this case no length so either natural or max
      if(type=="natural"){
        par <- fitNaturalCat(endpoints)
        c1 <- par[1]
        c2 <- par[2]
        lambda <- par[3]
      } else {
        # Assume that wanted max
        par <- fitMaxCat(endpoints)
        c1 <- par[1]
        c2 <- par[2]
        lambda <- par[3]
      }
    } else {
      # Given length
      if(nrow(endpoints) != 2 | ncol(endpoints) != 2){
        stop("Given endpoints must be a 2x2 data frame")
      }
      # Old method no longer used
      # par <- fitCatEndPts(endpoints,L)
      par <- fitCat(endpoints,L)
      c1 <- par[1]
      c2 <- par[2]
      lambda <- par[3]
    }
    x0 <- endpoints[1,1]
    x1 <- endpoints[2,1]
  }
  y0 <- f(x0,c1,c2,lambda)
  y1 <- f(x1,c1,c2,lambda)
  endpoints <- data.frame(x=c(x0,x1),y=c(y0,y1))
  row.names(endpoints) <- c('left','right')
  L <- getCatLength(x0=x0,x1=x1,c1=c1,c2=c2)
  new('catenary',c1=c1,c2=c2,lambda=lambda,endpoints=endpoints,L=L)
}
#' Plot method for catenary
#' 
#' basic plot with endpoints
#' 
#' @export
#' @author Jono Tuke, Matthew Roughan
#' @aliases plot,catenary-method
#' @rdname plot
#' @examples
#' tmp <- catenary(c1=1,c2=3,lambda=1,x0=0,x1=4)
#' plot(tmp)
setMethod(f='plot',
          signature='catenary',
          definition = function(x,y,...){
            xpts <- ypts <- NULL
            tmp <- getPoints(x)
            p <- qplot(x=xpts,y=ypts,data=tmp,geom='line')
            p <- p + geom_point(aes(x=x,y=y),data=x@endpoints)
            p <- p + labs(x="x",y="y")
            return(p)
          }
)
#' Method to get points for catenary
#' 
#' Gives points for ploting
#' 
#' @return data frame of points
#' @export
#' @keywords internal
#' @docType methods
setGeneric('getPoints',
           function(object){
             standardGeneric('getPoints')  
           }
)
#' @aliases getPoints,catenary-method
#' @rdname getPoints
setMethod('getPoints',
          signature = 'catenary',
          definition = function(object){
            xpts <- seq(object@endpoints[1,1],object@endpoints[2,1],l=100)
            ypts <- f(xpts,object@c1,object@c2,object@lambda)
            tmp <- data.frame(xpts,ypts)
            return(tmp)
          }
)
#' Method to get vertex for catenary
#' 
#' Gives vertex point
#' 
#' @return coordinates of vertex
#' @export
#' @docType methods
setGeneric('vertex',
           function(object){
             standardGeneric('vertex')  
           }
)
#' @aliases vertex,catenary-method
#' @rdname vertex
#' @examples
#' cat <- catenary(c1=1,c2=1,lambda=1)
#' vertex(cat)
setMethod('vertex',
          signature = 'catenary',
          definition = function(object){
            x <- object@c2
            y <- object@c1 + object@lambda
            return(c(x=x,y=y))
          }
)
#' Method to get min and max of catenary
#' 
#' Gives min or max
#' 
#' @return gives min or max values of catenary
#' @export
#' @docType methods
setGeneric('minmax',
           function(object){
             standardGeneric('minmax')  
           }
)
#' @aliases minmax,catenary-method
#' @rdname minmax
#' @examples
#' cat <- catenary(c1=1,c2=1,lambda=1)
#' minmax(cat)
setMethod('minmax',
          signature = 'catenary',
          definition = function(object){
            if(object@c1 > 0){
              miny <- c(x=object@c2,y=object@c1 + object@lambda)
              maxy <- object@endpoints[which.max(object@endpoints$y),]
            }            
            if(object@c1 < 0){
              maxy <- c(x=object@c2,y=object@c1 + object@lambda)
              miny <- object@endpoints[which.min(object@endpoints$y),]
            } 
            tab <- rbind(miny,maxy)
            row.names(tab) <- c("min","max")
            return(tab)
          }
)
#' Get length for catenary
#'
#' Returns the length of catenary
#'
#' @param x A \code{catenary} object
#' @return length
#' @name accessors
#' @export
#' @docType methods
#' @rdname accessor-methods
#' @author Jono Tuke, Matthew Roughan
#' @examples
#' tmp <- catenary(c1=1,c2=2,lambda=3,x0=0,x1=3)
#' L(tmp)
setGeneric("L",function(x) {
  standardGeneric("L")
})
#' @rdname accessor-methods
#' @aliases L,catenary-method
setMethod("L", "catenary", function(x){
  slot(x,"L")
})

#' Summary method for catenary
#' 
#' Nicely formatted output
#' 
#' @export
#' @author Jono Tuke, Matthew Roughan
#' @aliases Summary,catenary-method
#' @rdname Summary
#' @examples
#' tmp <- catenary(c1=1,c2=3,lambda=1,x0=0,x1=4)
#' Summary(tmp)
setMethod(f='Summary',
          signature='catenary',
          definition = function(x,..., na.rm=FALSE){
            output <- list()
            output$parameters <- data.frame(value = c(x@c1,x@c2,x@lambda))
            rownames(output$parameters) <- c("c1",'c2','lambda')
            output$endpoints <- x@endpoints
            output$length <- L(x)
            tmp <- data.frame(value=c(L(x),vertex(x)))
            output$vertex <- vertex(x)
            return(output)
          }
)

