#' Generate communities palette color
#' 
#' @description Create a named character vector with the color palette 
#' associated with the communities. The function is based on the
#' function \code{\link{distinctColorPalette}}.
#' 
#' @param N (Required) integer with the wanted number if distinct color.
#' 
#' @examples
#' palette_communities(8)
#' 
#' @importFrom randomcoloR distinctColorPalette
#' 
#' @export
generate_palette_communities <- function(N){
  
  if(!is.numeric(N) | round(N)!=(N) | N<=0) stop("N must be integer positive number")
  
  palette <- distinctColorPalette(k=N)
  names(palette) <- 1:N
  palette <- c("0"="#FFFFFF",palette)
  
  return(palette)
}