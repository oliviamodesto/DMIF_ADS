library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(dplyr)
library(ggplot2)
library(lpSolve)
library(lpSolveAPI)
library(RColorBrewer)


# PARTE 1 : creazione grafi di partenza

raw <- read.csv("./connections_data/Connections_Data.csv", header=TRUE)

dictionary <- raw[, "Word"]
df_dictionary <- data.frame(dictionary)
df_dictionary <- unique(df_dictionary)
comment(df_dictionary) <- c("tutte le WORD")
rm(dictionary)


# Creazione grafo g_stesso_gruppo
# parole appartenenti allo stesso gruppo tenendo conto della difficolta' dei gruppi
# 0 -> "yellow"
# 1 -> "green"
# 2 -> "blue"
# 3 -> "purple"

word_group_level <- raw[, c("Word", "Group.Name", "Group.Level")]
nrows <- nrow(data["Word"])

difficulty_pair <- c()
index <- 1

for (i in 1:nrows){
  
  curr_word <- word_group_level$Word[i]
  curr_group <- word_group_level$Group.Name[i]
  
  print(paste0("Trova vicini del vertice ", i, "-esimo: ", curr_word, " nel gruppo ", curr_group))
  
  for(j in i:nrows){
    match_group <- word_group_level$Group.Name[j]
    
    print(paste0("Gruppo trovato : ", match_group))
    
    if(curr_group == match_group){
      match_word <- word_group_level$Word[j]
      difficulty_degree <- word_group_level$Group.Level[j]
      new_set <- c(curr_word, match_word, difficulty_degree)
    
      print("MATCH ! ")
      
      difficulty_pair[[index]] <- new_set
      index <- index+1
      
    }
  }
}

rm(match_word)
rm(curr_word)
rm(curr_group)
rm(match_group)
rm(difficulty_degree)
rm(new_set)
rm(i)
rm(j)
rm(index)

df_pairing <- do.call(rbind, difficulty_pair)

colnames(df_pairing) <- c("to", "from", "difficulty")
df_pairing <- data.frame(df_pairing)

df_pairing <- subset(df_pairing, (df_pairing$to != df_pairing$from))

g_stesso_gruppo <- tbl_graph(nodes = df_dictionary, edges = df_pairing, directed = FALSE)

for (i in 1:nrow(df_pairing)){
  
  print(paste0("Etichetta arco numero : ", i))
  E(g_stesso_gruppo)[i]$weight <- df_pairing$difficulty[i]
  
  if (df_pairing$difficulty[i] == "0"){
    E(g_stesso_gruppo)[i]$color <- "yellow"
  }else if (df_pairing$difficulty[i] == "1"){
    E(g_stesso_gruppo)[i]$color <- "green"
  }else if (df_pairing$difficulty[i] == "2"){
    E(g_stesso_gruppo)[i]$color <- "blue"
  }else{
    E(g_stesso_gruppo)[i]$color <- "purple"
  }
  
}

# etichettare i grafi con in vocaboli corretti

for (i in 1:vcount(g_stesso_gruppo)){
  
  print(paste0("Etichetta vertice numero : ", i))
  V(g_stesso_gruppo)[i]$label <- df_dictionary$dictionary[i]
}


#
#
#
#


gameId_word <- raw[, c("Game.ID", "Word")]

game_session_pairing <- c()
index <- 1

for (i in 1:nrows){
  
  curr_word <- gameId_word$Word[i]
  curr_game <- gameId_word$Game.ID[i]
  
  print(paste0("Trova vicini del vertice ", i, "-esimo: ", curr_word, " della partita ", curr_game))
  
  for(j in i:nrows){
    match_game <- gameId_word$Game.ID[j]
    
    print(paste0("Partita trovata : ", match_game))
    
    if(curr_game == match_game){
      match_word <- gameId_word$Word[j]
      
      new_pair <- c(curr_word, match_word, match_game)
      print("MATCH! ")
      game_session_pairing[[index]] <- new_pair
      index <- index+1
      
    }
  }
}

rm(curr_word)
rm(curr_game)
rm(match_game)
rm(match_word)
rm(new_pair)
rm(i)
rm(j)
rm(index)


df_pairing_2 <- do.call(rbind, game_session_pairing)

colnames(df_pairing_2) <- c("to", "from", "session")
df_pairing_2 <- data.frame(df_pairing_2)

df_pairing_2 <- subset(df_pairing_2, (df_pairing_2$to != df_pairing_2$from))

g_stessa_partita <- tbl_graph(nodes = df_dictionary, edges = df_pairing_2, directed = FALSE)

n_colors <- 619

library(circlize)

session_palette <- rand_color(n = n_colors, luminosity = "dark")

unloadNamespace(circlize) # circlize interferisce con altre librerie, quindi viene rimossa subito


for (i in 1:nrow(df_pairing_2)){
  print(paste0("Etichetta arco numero : ", i))
  
  E(g_stessa_partita)[i]$weight <- df_pairing_2$session[i]
  
  which_color <- strtoi(df_pairing_2$session[i])
  print(which_color)
  
  E(g_stessa_partita)[i]$color <- session_palette[which_color]

}


for (i in 1:vcount(g_stessa_partita)){
  print(paste0("Etichetta vertice numero : ", i))
  V(g_stessa_partita)[i]$label <- df_dictionary$dictionary[i]
}

rm(i)
rm(n_colors)
rm(which_color)
rm(session_palette)