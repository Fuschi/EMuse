#' Collection of metagenomics soil sample sequenced with shoutgun tecnique
#' (only bacteria).
#'
#'
#' @format ## `bacteria`
#' A matrix with 125 rows/sample and 1280 columns/species:
"bacteria"

#' Samples metadata
#'
#'
#' @format ## `metadata`
#' A data.frame with the experimental variables of the samples. 
#' The columns contain the geographical sites and the date.
#' 
"metadata"

#' Taxonomic Classification
#'
#'
#' @format ## `taxonomy`
#' A character matrix with 1280 rows/taxa and 7 columns/taxonomic rank.
#' It contains the taxonomy classification of the bacteria.
#' 
"taxonomy"