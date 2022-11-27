#' Signed-Weighted Graph Layout
#'
#' @description Signed version of fruchterman reingold layout. The function take
#' into account only the positive links.
#' 
#' @param graph weighted undirected network belong to \code{\link{igraph}} class.
#' @param seed random seed for reproducibility.
#'
#'@importFrom igraph layout.fruchterman.reingold subgraph.edges E is.weighted is.directed
#'@export
layout_signed <- function(graph, seed=123){
  
  #Check Graph
  if(!is.weighted(graph)) stop("graph must be weighted graph")
  if(is.directed(graph))  stop("graph must be undirected")
  
  graph.sub <- subgraph.edges(graph=graph,
                              eids=which(E(graph)$weight>0),
                              delete.vertices=FALSE)
  
  set.seed(seed)
  layout <- layout.fruchterman.reingold(graph.sub)
  return(layout)
}