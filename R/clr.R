#' Centered Log-Ratio Transformation
#'
#'
#' @param X (Required) numeric matrix or vector with all elements greater than or equal to 0.
#' @param mar (Optional) Integer giving the dimension where the function will be applied;
#' 1 for rows and 2 for columns (default 1).

#' @export
clr <- function(X, mar=1){
  
  if( !(is.matrix(X) | is.vector(X)) ) stop("X must be a matrix or a vector")
  if(!is.numeric(X) | any(X<=0)) stop("X must be numeric with all elements greater than 0")
  if(!(mar%in%c(1,2))) stop("mar has as possible values only 1 and 2.")

  if(is.null(dim(X))){
    return(log(X) - mean(log(X)))
  } else {
    ref <- apply(X, mar, function(x) mean(log(x)) )
    return(as.matrix(log(X) - ref))
  }
}