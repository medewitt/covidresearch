library(igraph)
library(lsa)
# http://rstudio-pubs-static.s3.amazonaws.com/5014_4e3001382f7442629c0760f373cdadd4.html
# https://users.dimi.uniud.it/~massimo.franceschet/R/communities.html

g = make_graph("Zachary")
coords = layout_with_fr(g)
# plot the graph
plot(g, layout=coords, vertex.label=NA, vertex.size=10, edge.size = 2)

laplacian_matrix(g)
length(V(g))
V(g)$status <-sample(c("infected", "negative"), length(V(g)),  replace = T)
V(g)[V(g)$status == "infected"]$shape <- "square"
V(g)[V(g)$status == "negative"]$shape <- "circle"
gg.com <- fastgreedy.community(g)
V(g)$color <- gg.com$membership + 1
plot(g)

plot(gg.com, g, layout=coords)