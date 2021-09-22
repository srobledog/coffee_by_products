add_clusters_to_nodes <- function(graph) {
  
  clusters <- 
    graph |> 
    igraph::as.undirected(mode = "each") |> 
    igraph::cluster_louvain()
  
  graph_clusters <- 
    graph |> 
    igraph::set_vertex_attr(name = "cluster",
                            value = membership(clusters))
  
  return(graph_clusters)
  
}
