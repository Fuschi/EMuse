#' Generate taxonomy color palette
#' 
#' @description Create a named character vector with the color palette 
#' associated with the taxonomy classification. The function is based on the
#' \code{\link{distinctColorPalette}}.
#' 
#' @param taxaID (Required) character vector with the taxonomic IDs. 
#' For a number of unique elements greater than 40, the colors may no longer be
#' clearly distinguishable.
#' @param seed random seed for reproducibility.
#' 
#' @examples
#' generate_taxonomy_palette(c("Proteobacteria","Firmicutes","Proteobacteria","Bacteroidetes","Firmicutes","Proteobacteria"))
#' 
#' @importFrom randomcoloR distinctColorPalette
#' 
#' @export
generate_taxonomy_palette <- function(taxaID,seed=123){
  
  if(!is.character(taxaID) | !is.vector(taxaID)) stop("taxaID must be a character vector")
  if(!is.numeric(seed))  stop("seed must be numeric")
  
  uniqueID <- unique(taxaID)
  set.seed(seed)
  palette <- distinctColorPalette(k=length(uniqueID))
  names(palette) <- uniqueID
  return(palette)
}