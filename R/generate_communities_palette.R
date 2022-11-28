#' Generate communities color palette 
#' 
#' @description Create a named character vector with the color palette 
#' associated with the communities. The function is based on the
#' function \code{\link{distinctColorPalette}}.
#' 
#' @param N (Required) integer with the wanted number if distinct color.
#' @param seed random seed for reproducibility.
#' 
#' @examples
#' generate_communities_palette(8)
#' 
#' @importFrom randomcoloR distinctColorPalette
#' 
#' @export
generate_communities_palette <- function(N, seed=123){
  
  if(!is.numeric(N) | round(N)!=(N) | N<=0) stop("N must be integer positive number")
  if(!is.numeric(seed))  stop("seed must be numeric")
  
  palette <- distinctColorPalette(k=N)
  names(palette) <- 1:N
  palette <- c("0"="#FFFFFF",palette)
  
  return(palette)
}