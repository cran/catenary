#' Create a class for fittedCatenary
#' 
#' Creates a catenary object from observed data
#'
#' @exportClass fittedCatenary
#' @name fittedCatenary-class
#' @rdname fittedCatenary-class
#' @author Jono Tuke, Matthew Roughan
#' @section Slots: \itemize{
#'    \item c1: shape parameter
#'    \item c2: x-location parameter
#'    \item lambda: y-location parameter 
#'    \item endpoints: left and right endpoint in data
#'    frame
#'    \item L: length of catenary
#'    \item obs: data frame of observed data
#'    \item fitted: fitted points for plots and prediction
#'    \item ss: sum of squares for fitted parabola and catenary
#' }
#' @return an object of class \code{fittedCatenary}
#' @examples
#' getSlots("fittedCatenary")
methods::setClass(
  Class="fittedCatenary",
  representation(
    obs = "data.frame",
    fitted = "data.frame",
    gof = "data.frame",
    ss = "numeric"),
  contains = 'catenary'
)
#' Creates a fittedCatenary object
#' 
#' Takes observed points and fits catenary and parabola
#' 
#' @param x values of x coordinates of observed values
#' @param y values of y coordinates of observed values
#' @param R number of iterations in bootstrap for function envelopes
#' @return an instance of \code{fittedCatenary} class
#' @author Jono Tuke, Matthew Roughan
#' @export
#' @examples
#' x <- runif(100,0,4)
#' y <- f(x,c1=1,c2=2,lambda=3) + rnorm(100,sd=0.1)
#' tmp <- fittedCatenary(x,y) 
fittedCatenary <- function(x,y,R=1000){
  if(length(x) != length(y)){
    stop("x and y must be of same length")
  }
  obs.lm <- lm(y~x + I(x^2))
  fitted <- data.frame(x=seq(min(x),max(x),l=100))
  fitted$para <- predict(obs.lm,newdata=fitted)
  bounds <- getFunctionEnvelopePara(data=data.frame(x=x,y=y),x=fitted$x,R=R)
  fitted$para_lwr <- bounds$lwr
  fitted$para_upr <- bounds$upr
  tmp <- coef(obs.lm)
  a <- tmp[3]; b <- tmp[2]; c <- tmp[1]
  c1 <- sign(a) / sqrt(abs(a))
  c2 <- -b / (2*a)
  lambda <- c - b^2 / (4 * a) - c1
  tryCatch(
    obs.cat <- nls(y ~ c1 * cosh( (x-c2)/c1) + lambda,
                   start=list(c1=c1,c2=c2,lambda=lambda)),
           error = function(e){stop("cannot fit catenary")},
           finally=print("Fitted catenary"))
  ss <- c(para=sum((obs.lm$residuals)^2),catenary=sum((resid(obs.cat))^2))
  gof <- as.data.frame(dplyr::bind_rows(cat=broom::glance(obs.cat), 
                          para=broom::glance(obs.lm), 
                          .id="model"))
  coef <- coef(obs.cat)
  fitted$cat <- predict(obs.cat,newdata=fitted)
  bounds <- getFunctionEnvelopeCat(data=data.frame(x=x,y=y),
                                   initial=coef,x=fitted$x,R=R)
  fitted$cat_lwr <- bounds$lwr
  fitted$cat_upr <- bounds$upr
  cat <- catenary(c1=coef[1],c2=coef[2],x0=min(x),x1=max(x),lambda=coef[3])
  obs <- data.frame(x,y)
  methods::new('fittedCatenary',obs=obs,fitted=fitted,cat,ss=ss,gof=gof)
}
#' Generic plot
#' 
#' @param x x-coordinate
#' @param y y-coordinate
#' @export
#' @name plot
methods::setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
#' Plot method for fitted Catenary
#' 
#' Method that can plot fits and function envelopes 
#' 
#' @export
#' @author Jono Tuke, Matthew Roughan
#' @param fit type of fit to show at present two choices "cat" and "para"
#' @param envelope type of envelope to show at present two choices "cat" and "para"
#' @aliases plot,fittedCatenary-method
#' @rdname plot
#' @name plot
methods::setMethod(f='plot',
          signature='fittedCatenary',
          definition = function(x,y,fit="none",envelope="none",...){
            # Hack to get rid of warnings about global variables. 
            para <- NULL; para_lwr <- NULL; para_upr <- NULL; cat_lwr <- NULL; cat_upr <- NULL
            p <- ggplot2::ggplot(data = x@obs, ggplot2::aes(x=x, y=y)) +
              ggplot2::geom_point()
            if("para" %in% fit){
              p <- p + ggplot2::geom_line(ggplot2::aes(x=x,y=para,col='parabola'),data=x@fitted)
            }
            if("para" %in% envelope){
              p <- p + ggplot2::geom_ribbon(ggplot2::aes(x=x,y=para,ymin=para_lwr,
                                       ymax=para_upr,fill='parabola'),
                                   alpha=0.2,data=x@fitted)
            }
            if("cat" %in% fit){
              p <- p + ggplot2::geom_line(ggplot2::aes(x=x,y=cat,col='catenary'),data=x@fitted)
            }
            if("cat" %in% envelope){
              p <- p + ggplot2::geom_ribbon(ggplot2::aes(x=x,y=cat,ymin=cat_lwr,
                                       ymax=cat_upr,fill='catenary'),
                                   alpha=0.2,data=x@fitted)
            }
            p <- p + ggplot2::labs(col='fit',fill='envelope')
            p <- p + ggplot2::scale_colour_manual(
              values = c("catenary" = "red","parabola" = "blue")) +
              ggplot2::scale_fill_manual(values=c('catenary'='red','parabola'='blue'))
            return(p)
          }
)
#' show method for fitted Catenary
#' 
#' @export
#' @author Jono Tuke, Matthew Roughan
#' @param object fittedCatenary object
#' @aliases show,fittedCatenary-method
#' @rdname show
#' @name show
methods::setMethod(f = "show", signature = "fittedCatenary", 
                   definition = function(object){
                     output <- methods::callNextMethod()
                     output$ss <- object@ss
                     return(output)
                   })
#' Get goodness of fit statistics for fitted catenary
#'
#' Returns data frame of summary stats for models
#'
#' @param x A fittedCatenary object
#' @return data frame of summary statistics
#' @export
#' @docType methods
#' @name gof
methods::setGeneric("gof",function(x) {
  standardGeneric("gof")
})
#' @aliases gof,fittedCatenary-method
#' @rdname gof
#' @name gof
methods::setMethod(f = "gof", 
                   signature = "fittedCatenary", 
                   definition = function(x){
                     slot(x,"gof")
                   }
)