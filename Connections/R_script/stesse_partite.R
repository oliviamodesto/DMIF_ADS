
# 
# 
#
# clustering

clusters_g_stessa_partita <- cluster_leading_eigen(g_stessa_partita)

community_g_stessa_partita_sizes <- sizes(clusters_g_stessa_partita)

#
#
#
# betweenness

betweenness_g_stessa_partita <- betweenness(g_stessa_partita, normalized = TRUE)
betweenness_g_stessa_partita <- data.frame(betweenness_g_stessa_partita)
betweenness_g_stessa_partita$word <- V(g_stessa_partita)$label
colnames(betweenness_g_stessa_partita) <- c("betweenness", "word")
betweenness_g_stessa_partita <- betweenness_g_stessa_partita[order(betweenness_g_stessa_partita$betweenness, decreasing = TRUE), ] 

#
#
#
# diameter
diameter_stessa_partita <- get_diameter(g_stessa_partita)
word_diameter_stessa_partita <- c()

for (i in diameter_stessa_partita){
  word_diameter_stessa_partita <- append(word_diameter_stessa_partita, V(g_stessa_partita)[i]$label)
}

g_stessa_partita_diameter <- g_stessa_partita


V(g_stessa_partita_diameter)$color <- "white"
E(g_stessa_partita_diameter)$color <- "grey"
E(g_stessa_partita_diameter)$width <- 0.5
V(g_stessa_partita_diameter)[diameter_stessa_partita]$color <- "red"
V(g_stessa_partita_diameter)[diameter_stessa_partita]$width <- 3
E(g_stessa_partita_diameter, path=diameter_stessa_partita)$color <- "red"
E(g_stessa_partita_diameter, path=diameter_stessa_partita)$width <- 3


plot(g_stessa_partita_diameter, vertex.label = NA, vertex.size=0.5, layout=layout_as_tree)

#
#
#
# closeness centrality

stessa_partita_closeness_centrality <- closeness(g_stessa_partita, normalized = TRUE)
stessa_partita_closeness_centrality <- data.frame(stessa_partita_closeness_centrality)

stessa_partita_closeness_centrality$word <- V(g_stessa_partita)$label


colnames(stessa_partita_closeness_centrality) <- c("closeness", "word")

stessa_partita_closeness_centrality <- stessa_partita_closeness_centrality[order(stessa_partita_closeness_centrality$closeness, decreasing = TRUE), ] 

#
#
#
# page rank

page_rank_g_stessa_partita <- page_rank(g_stessa_partita)
page_rank_g_stessa_partita <- data.frame(page_rank_g_stessa_partita$vector)

page_rank_g_stessa_partita$word <- V(g_stessa_partita)$label

colnames(page_rank_g_stessa_partita) <- c("rank", "word")

page_rank_g_stessa_partita <- page_rank_g_stessa_partita[order(page_rank_g_stessa_partita$rank, decreasing = TRUE), ] 


#
#
#
# eigenvector centrality

eigen_centrality_g_stessa_partita <- eigen_centrality(g_stessa_partita)

eigen_centrality_g_stessa_partita <- data.frame(eigen_centrality_g_stessa_partita$vector)

eigen_centrality_g_stessa_partita$word <- V(g_stessa_partita)$label

colnames(eigen_centrality_g_stessa_partita) <- c("eigen", "word")

eigen_centrality_g_stessa_partita <- eigen_centrality_g_stessa_partita[order(eigen_centrality_g_stessa_partita$eigen, decreasing = TRUE), ] 





#
#
#
# se un nodo è in una sola partita avrà 15 archi
# quindi, cerca nodi con grado > 15
# chi sono i nodi più popolari

degree_g_stessa_partita <- degree(g_stessa_partita)

degree_g_stessa_partita <- data.frame(degree_g_stessa_partita)

colnames(degree_g_stessa_partita) <- c("degree")

degree_g_stessa_partita$word <- df_dictionary$dictionary

degree_g_stessa_partita <- degree_g_stessa_partita[order(degree_g_stessa_partita$degree, decreasing=TRUE),]

#
#
#
#
# analisi simile a quella vista con la prima parte
# trova altre parole con cui ha più di un arco

# considera creare un grafo con i nomi dei gruppi e le partite di appartenenza
# se un gruppo è comparso in più di una partita allora avrà più di 4 occorrenze

game_group <- raw[, c("Game.ID", "Group.Name")]
group_num_occurances <- table(game_group$Group.Name)
group_num_occurances <- data.frame(group_num_occurances)
colnames(group_num_occurances) <- c("name", "freq")

group_num_occurances <- group_num_occurances[order(group_num_occurances$freq, decreasing=TRUE),]

#
#
#
#
#
#

# ci sono stessi gruppi con nomi diversi?
# utilizza Game.ID, Word, Group.Name
game_group_name <- raw[, c("Game.ID", "Word", "Group.Name")]

# crea nuovo dataframe con struttura
# Game.ID, Game.Name, Word1, Word2, Word3, Word4


grouping_words <- c()
index <- 1

for (k in 1:619){
  
  temp <- subset(game_group_name, Game.ID == k)
  temp <- temp[order(temp$Group.Name),]
  
  if(nrow(temp) > 1){
    
    for (i in seq(1,nrow(temp),4)){
      gr <- temp$Group.Name[i]
      
      words <- c()
      for(j in i:(i+3)){
        words <- append(words, temp$Word[j])  
      }
      
      words <- sort(words)
      
      new_entry <-c()
      new_entry <- append(new_entry, k)
      new_entry <- append(new_entry, gr)
      new_entry <- append(new_entry, words)
      
      grouping_words[[index]] <- new_entry
      index <- index+1
    }
  }
  
}

rm(temp)
rm(gr)
rm(words)
rm(new_entry)

df_grouping_words <- do.call(rbind, grouping_words)

colnames(df_grouping_words) <- c("Game.ID", "Game.Name", "Word.1", "Word.2", "Word.3", "Word.4")
df_grouping_words <- data.frame(df_grouping_words)
df_grouping_words <- df_grouping_words[order(df_grouping_words$Game.Name, decreasing=FALSE),]

check_group_members <- group_num_occurances[c(group_num_occurances$freq > 4), ]

rownames(check_group_members) <- c(1:nrow(check_group_members))
##################################  

# per ogni gruppo in check_group_members troviamo le words corrispondenti

check_members = function(group_name){
  
  index <- 1
  df <- c()
  set <- which(df_grouping_words$Game.Name == group_name)
    
  for(j in set){
    new_item <- c(df_grouping_words$Word.1[j], df_grouping_words$Word.2[j], df_grouping_words$Word.3[j], df_grouping_words$Word.4[j])
    df[[index]] <- new_item
    index <- index+1
  }
 
  return(df)
   
}

HOMOPHONES_members <- check_members("HOMOPHONES")
HOMOPHONES_members <- do.call(rbind, HOMOPHONES_members)

FISH_members <- check_members("FISH")
FISH_members <- do.call(rbind, FISH_members)

ANAGRAMS_members <- check_members("ANAGRAMS")
ANAGRAMS_members <- do.call(rbind, ANAGRAMS_members)

MAGAZINES_members <- check_members("MAGAZINES")
MAGAZINES_members <- do.call(rbind, MAGAZINES_members)