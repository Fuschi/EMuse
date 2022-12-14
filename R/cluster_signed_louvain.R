#------------------------------------------------------------------------------#
#' Get Communities of a Signed Weighted Graph
#'
#'@description The algorithm tries to find dense subgraph, also called 
#' communities via optimization of a signed definition of modularity
#' score.
#' 
#'@param graph weighted undirected network belong to \code{\link{igraph}} class.
#'@param OS string with the operating system running. Possible choices are 
#'"Linux","Windows","Mac" (default "Linux").
#'
#'@return \code{\link{communities}} igraph object able to manage to communities
#'graph info. The unique difference from igraph routine is the treatment with 
#'the isolated nodes. In this case all isolated nodes are classified in the
#'community \code{'0'} and not as different communities of size one
#'
#'@importFrom igraph graph_from_adjacency_matrix write_graph make_clusters is.weighted is.directed as_adjacency_matrix make_clusters sizes
#'@importFrom stringr str_split
#'
#'@export
cluster_signed_louvain <- function(graph, OS="Linux"){
  
  #Check Graph
  OS <- match.arg(OS,c("Linux","Windows","Mac"))
  if(!is.weighted(graph)) stop("graph must be weighted graph")
  if(is.directed(graph))  stop("graph must be undirected")
  if(file.exists("graph.net")) file.remove("graph.net")
  
  adj <- as_adjacency_matrix(graph, attr="weight", sparse=FALSE)
  #End Checks
  
  #get path of executable Communities_Detection.exe
  path <- system.file("exec", package="EMuse", mustWork=TRUE)
  path <- paste(path,"/",sep="")
  
  #write graph in pajek format as request from executable
  write_graph(graph  = graph_from_adjacency_matrix(adjmatrix=adj, mode="undirected", weighted=TRUE),
              file   = paste(path,"graph.net",sep=""),
              format ="pajek")
  
  #path for graph and results files
  path.graph <- paste(path,"graph.net",sep="")
  path.result <- paste(path, "res.txt", sep="")
  
  #save command for the execution
  if(OS=="Linux"){
    cmd <- paste(path,"Communities_Detection_Linux.exe none WS l 1",sep="")
  } else if(OS=="Windows"){
    cmd <- paste(path,"Communities_Detection_Windows.exe none WS l 1",sep="")
  } else if(OS=="Mac"){
    cmd <- paste(path,"Communities_Detection_Mac.exe none WS l 1",sep="")
  }
  cmd <- paste(cmd, path.graph, path.result)
  
  #make communities detection
  if(OS=="Windows"){
    system(cmd,ignore.stdout=TRUE)
  } else {
    system(cmd,ignore.stdout=TRUE,ignore.stderr=TRUE)
  }
  #open file of results
  res.file = file(path.result, "r")
  
  #read and store results
  file.lines <- readLines(res.file)
  info <- file.lines[2]
  modularity <- as.numeric(str_split(file.lines[3]," ")[[1]][3])
  
  vertex.num <- as.numeric(str_split(file.lines[5]," ")[[1]][4])
  comm.num <- as.numeric(str_split(file.lines[6]," ")[[1]][4])
  comm.vert <- vector(mode="list",length=comm.num)
  
  for(line in 8:(7+comm.num)){
    
    i <- line-7
    #comm[[i]]
    tmp <- unlist(str_split(file.lines[line]," "))
    tmp <- as.numeric(tmp[2:length(tmp)])
    
    comm.vert[[i]] <- tmp
  }
  
  comm <- vector(mode="integer", length=vertex.num)
  for(c in 1:comm.num){comm[comm.vert[[c]]] <- c}
  
  #close connection
  close(res.file)
  
  #remove files
  file.remove(path.graph)
  file.remove(path.result)
  
  res <- make_clusters(graph=graph,
                       membership=comm,
                       algorithm="signed weights louvain",
                       modularity=modularity)
  
  isolated.membership <- which(sizes(res)==1)
  res$membership[res$membership %in% isolated.membership] <- 0
  
  
  #return the results as communities structure of igraph package
  return(res)
}
