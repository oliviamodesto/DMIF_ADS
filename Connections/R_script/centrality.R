# PARTE 2 : analisi g_stesso_gruppo

# trova le componenti del grafo

components_word_groups <- components(g_stesso_gruppo)
# esiste una component con 4656 nodi visualizzabile con max(components_word_groups$csize)
# Questa è la componente principale, su cui ci si concentra l'analisi.

not_main_component <- which(components_word_groups$membership != which.max(components_word_groups$csize))
# nodi non appartenenti alla componente principale
# questi sono da rimuovere dal main component
# not_main_component si può eliminare dall'environment


g_stesso_gruppo_main_component <- delete_vertices(g_stesso_gruppo, not_main_component)

rm(not_main_component)

#
#
#
#

degree_main_component <- degree(g_stesso_gruppo_main_component)

levels_main_component <- E(g_stesso_gruppo_main_component)$color

num_yellow <- sum(levels_main_component == "yellow")
num_green <- sum(levels_main_component == "green")
num_blue <- sum(levels_main_component == "blue")
num_purple <- sum(levels_main_component == "purple")

levels_g_stesso_gruppo <- E(g_stesso_gruppo)$color


total_yellow <- sum(levels_g_stesso_gruppo  == "yellow")
total_green <- sum(levels_g_stesso_gruppo == "green")
total_blue <- sum(levels_g_stesso_gruppo == "blue")
total_purple <- sum(levels_g_stesso_gruppo == "purple")

frac_yellow <- num_yellow/total_yellow
frac_green <- num_green/total_green
frac_blue <- num_blue/total_blue
frac_purple <- num_purple/total_purple

#
#
#
#

degree_main_component <- sort(degree_main_component, index.return=TRUE, decreasing=TRUE)


#view degree distribution 
df_degree_main_component <- data.frame(degree_main_component)

rm(degree_main_component)

colnames(df_degree_main_component) <- c("degree", "index")

# crea colonna con le etichette dei nodi
degree_labels <- c()

for(i in df_degree_main_component$index){
  degree_labels <- append(degree_labels, V(g_stesso_gruppo_main_component)[i]$label)
}

df_degree_main_component$label <- degree_labels

#
#
#
#

edgelist <- as_edgelist(g_stesso_gruppo_main_component, name = TRUE)

df_edgelist <- data.frame(edgelist)

colnames(df_edgelist) <- c("to", "from")



# 
#
#
#
# betweenness
# i pesi degli archi in g_stesso_gruppo_main_component dipendono dalla difficoltà
# per questa parte di analisi, la difficoltà non ci interessa.
# si crea una copia del grafo. Tutti i nodi hanno lo stesso peso.

g_corrected_weights <- g_stesso_gruppo_main_component

for (i in 1:ecount(g_corrected_weights)){
  E(g_corrected_weights)[i]$weight <- 1
}

corrected_weights_betweenness <- betweenness(g_corrected_weights, normalized = TRUE)
corrected_weights_betweenness <- data.frame(corrected_weights_betweenness)
corrected_weights_betweenness$word <- V(g_corrected_weights)$label
colnames(corrected_weights_betweenness) <- c("betweenness", "word")
corrected_weights_betweenness <- corrected_weights_betweenness[order(corrected_weights_betweenness$betweenness, decreasing = TRUE), ] 

#
#
#
#
# diameter of graph

diameter_main_component <- get_diameter(g_stesso_gruppo_main_component)
word_diameter <- c()

for (i in diameter_main_component){
  word_diameter <- append(word_diameter, V(g_stesso_gruppo_main_component)[i]$label)
}

g_main_component_diameter <- g_stesso_gruppo_main_component


V(g_main_component_diameter)$color <- "white"
E(g_main_component_diameter)$color <- "grey"
E(g_main_component_diameter)$width <- 0.5
V(g_main_component_diameter)[diameter_main_component]$color <- "red"
V(g_main_component_diameter)[diameter_main_component]$width <- 3
E(g_main_component_diameter, path=diameter_main_component)$color <- "red"
E(g_main_component_diameter, path=diameter_main_component)$width <- 3


plot(g_main_component_diameter, vertex.label = NA, vertex.size=0.5, layout=layout_as_tree)




#
#
#
# closeness centrality
# g_corrected_weights

main_component_closeness_centrality <- closeness(g_corrected_weights, normalized = TRUE)
main_component_closeness_centrality <- data.frame(main_component_closeness_centrality)

main_component_closeness_centrality$word <- V(g_corrected_weights)$label


colnames(main_component_closeness_centrality) <- c("closeness", "word")

main_component_closeness_centrality <- main_component_closeness_centrality[order(main_component_closeness_centrality$closeness, decreasing = TRUE), ] 



#
#
#
# page rank
g_stesso_gruppo_page_rank <- page_rank(g_stesso_gruppo_main_component)

g_stesso_gruppo_page_rank <- data.frame(g_stesso_gruppo_page_rank$vector)

g_stesso_gruppo_page_rank$word <- V(g_stesso_gruppo_main_component)$label

colnames(g_stesso_gruppo_page_rank) <- c("rank", "word")

g_stesso_gruppo_page_rank <- g_stesso_gruppo_page_rank[order(g_stesso_gruppo_page_rank$rank, decreasing = TRUE), ] 




#
#
#
#
# eigenvector centrality
main_component_eigenvector_centrality <- eigen_centrality(g_corrected_weights)

main_component_eigenvector_centrality <- data.frame(main_component_eigenvector_centrality$vector)

main_component_eigenvector_centrality$word <- V(g_corrected_weights)$label

colnames(main_component_eigenvector_centrality) <- c("eigen", "word")

main_component_eigenvector_centrality <- main_component_eigenvector_centrality[order(main_component_eigenvector_centrality$eigen, decreasing = TRUE), ] 
