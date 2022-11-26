#------------------------------------------------------------------------------#
#' Upper triangular values into square matrix
#' 
#' @description reorganize a vector containing the values of an upper triangular 
#' matrix into a symmetric square matrix.
#' 
#' @param v (Required) upper triangular elements.
#' @param diag (Optional) diagonal elements of resulting matrix (default 0)
#' 
#' @export
triu2mat <- function(v,diag=0){
  
  if(!is.vector(v) & !is.numeric(v)) stop("v must be a numeric vector")
  
  N <- .5*(1 + sqrt(1+8*length(v)))
  if(round(N)!=N) stop("the length of the vector of the upper triangular elements does not match the dimensions of a square matrix")
  if(length(diag)!=1 & length(diag)!=N) stop("diag must a number or a vector with length equal to the resulting square matrix")
  
  M <- matrix(0,nrow=N,ncol=N)
  M[upper.tri(M)] <- v
  M <- M+t(M)
  diag(M) <- diag
  return(M)    
}