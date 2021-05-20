library(igraph)
library(RColorBrewer)

as.igraph <- function(edges) {
  type <- c("N","P","U","Z")
  arrow <- c(1,1,3,0)
  pal <- brewer.pal(6,"Set1")[c(1,2,4,6)]

  weight <- c(-1,1,NA,0)

  g <- graph.edgelist(cbind(edges$To,edges$From)-1)
  V(g)$label <- levels(edges$To)
  V(g)$color <- brewer.pal(6,"Set1")[3]
  V(g)$label.color <-  "black"
  E(g)$type <- edges$Type
  E(g)$curved <- T
  E(g)$arrow.mode <- arrow[match(edges$Type,type)]
  E(g)$color <- pal[match(edges$Type,type)]
  E(g)$weight <- weight[match(edges$Type,type)]
  E(g)$lty <- edges$Group
  g
}

