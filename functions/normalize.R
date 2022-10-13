## exported from spieceasi 

#' @method clr default
#' @param base base for log transformation
#' @param tol tolerance for a numerical zero
#' @rdname clr
#' @export
clr.default <- function(x.f, base=exp(1), tol=.Machine$double.eps, ...) {
  nzero <- (x.f >= tol)
  LOG <- log(ifelse(nzero, x.f, 1), base)
  ifelse(nzero, LOG - mean(LOG)/mean(nzero), 0.0)
}

#' @method clr matrix
#' @param mar margin to apply the transformation (rows: 1 or cols: 2)
#' @rdname clr
#' @export
clr.matrix <- function(x.f, mar=2, ...) {
  apply(x.f, mar, clr, ...)
}

#' Centered log-ratio functions
#' @param x.f input data
#' @param ... pass through arguments
#' @export
clr <- function(x.f, ...) {
  UseMethod('clr')
}


#' @param dt in row obs in col variables
#' @method used for the transformation
rau_transforms <- function(x, method, trt.val = 0){
  ## transform in compositional data
  x/rowSums(x)
  ## treat NA values if the sum is 0
  is.na(x) <- trt_val
  ## 
  if (method == "arcsin"){
    res <- asin(sqrt(x))
  } else if (method == "logit") {
    res <- log2(x/(1-x))
  }
  
  return(res)
}


box_cox <- function(x, lambda){
  if (lambda != 0){
    res <- (x^lambda - 1) / lambda
  } else {
    res <- log(x)
  }
  
  return(res)
}

non_paranormal <- function(){
  huge.npn()
}