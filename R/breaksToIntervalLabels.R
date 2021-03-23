# breaksToIntervalLabels -------------------------------------------------------

#' Create Interval Labels from Breaks Vector
#' 
#' Create labels for intervals defined by breaks in different possible styles
#' 
#' @param breaks numeric vector of breaks
#' @param style passed to \code{\link{intervalLabel}}
#' @param \dots further arguments passed to \code{\link{intervalLabel}}
#' 
#' @export
#' 
breaksToIntervalLabels <- function(breaks, style = 5, ...)
{
  intervalLabel(a = c(-Inf, breaks), b = c(breaks, Inf), style = style, ...)
}

# intervalLabel ----------------------------------------------------------------

#' Create Interval Label from Upper and Lower Boundary
#' 
#' Create a label for the interval defined by the upper boundary \code{a} and
#' the lower boundary \code{b}
#' 
#' @param a upper boundary
#' @param b lower boundary
#' @param right if TRUE (default) the interval is closed at the upper boundary
#' @param style integer number between 1 and 5 indicating one of five possible
#'   styles to name the interval between \code{a} and \code{b}. See examples
#'   below.
#' @param sep separator to be used between lower and upper boundary
#' @param space space between comparison operators and boundary values.
#' 
#' @export
#' 
#' @examples 
#' # Labels of different styles for right closed intervals (right = TRUE is the 
#' # default)
#' intervalLabel(1, 10, style = 1) # "(1,10]"
#' intervalLabel(1, 10, style = 2) # "<= 10"
#' intervalLabel(1, 10, style = 3) # "> 1"
#' intervalLabel(1, 10, style = 4) # "<= " "> 1" (vector of two elements!)
#' intervalLabel(1, 10, style = 5) # "<= 10" "> " (vector of two elements!)
#' 
#' # The same with left closed intervals:
#' right <- FALSE
#' intervalLabel(1, 10, right, style = 1) # "[1,10)"
#' intervalLabel(1, 10, right, style = 2) # "< 10"
#' intervalLabel(1, 10, right, style = 3) # ">= 1"
#' intervalLabel(1, 10, right, style = 4) # "< " ">= 1" (vector of two elements!)
#' intervalLabel(1, 10, right, style = 5) # "< 10" ">= " (vector of two elements!)
#'
intervalLabel <- function(a, b, right = TRUE, style = 1, sep = ",", space = " ")
{
  stopifnot(length(a) == length(b))
  
  stopifnot(length(style) == 1 && style %in% 1:5)
  
  below <- function(x) {
    
    paste0(ifelse(right, "<=", "<"), space, x)
  }
  
  above <- function(x) {
    
    paste0(ifelse(right, ">", ">="), space, x)
  }
  
  aboveBelow <- function(x, y) {
    
    limits <- c(ifelse(right, "(", "["), ifelse(right, "]", ")"))
    
    paste0(limits[1], x, sep, y, limits[2])
  }
  
  if (style == 1) {
    
    aboveBelow(a, b)
    
  } else if (style == 2) {
    
    below(b)
    
  } else if (style == 3) {
    
    above(a)
    
  } else if (style == 4) {
    
    N <- length(a)
    
    c(below(b[-N]), above(a[N]))
    
  } else if (style == 5) {
    
    c(below(b[1]), above(a[-1]))
  }
}
