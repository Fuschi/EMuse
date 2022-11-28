#' Generate rank palette color
#' 
#' @description Create a named character vector with the color palette 
#' associated with the taxonomy classification. The function is based on the
#' function \code{\link{distinctColorPalette}}.
#' 
#' @param taxaID (Required) character vector with the taxonomic IDs. 
#' For a number of unique elements greater than 40, the colors may no longer be
#' clearly distinguishable.
#' 
#' @examples
#' palette_rank(c("Proteobacteria","Firmicutes","Proteobacteria","Bacteroidetes","Firmicutes","Proteobacteria"))
#' 
#' @importFrom randomcoloR distinctColorPalette
#' 
#' @export
generate_palette_rank <- function(taxaID){
  uniqueID <- unique(taxaID)
  palette <- distinctColorPalette(k=length(uniqueID))
  names(palette) <- uniqueID
  return(palette)
}