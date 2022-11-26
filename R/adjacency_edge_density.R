#' Edge Density Threshold
#'
#'@description
#'Retrieves the adjacency matrix from the correlation matrix by keeping only 
#'the higher absolute correlations up to the desired graph link density.
#'
#'@param corr correlation matrix
#'@param edge.density threshold value
#'
#'@export
adjacency_edge_density <- function(corr, edge.density=.05){
  
  #checks
  if(nrow(corr)!=ncol(corr)) stop("matrix must be square")
  if(any(rownames(corr)!=colnames(corr))) stop("Weights rownames and colnames must be the same")
  if(edge.density<0 || edge.density>1) stop("edge.density must be in range [0,1]")
  if(any(abs(corr)>1)) stop("all correlation elements in corr must be in range [0,1]")
  if(!isSymmetric(corr)) stop("correlation matrix corr must be symmetric")
  
  diag(corr) <- 0
  
  # Take correlation upper triangular values as vector
  xv <- corr[upper.tri(corr)]
  
  # Sort Aboslute values
  xv.sort <- sort(abs(xv),decreasing=TRUE)
  
  # Take the Absolute Min Values
  th.value <- xv.sort[round(length(xv.sort)*edge.density)]
  
  adj <- X * (abs(X)>=th.value)
  return(adj)
}  