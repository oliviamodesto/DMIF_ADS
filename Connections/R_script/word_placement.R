library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(RColorBrewer)
library(dplyr)

# analisi griglia
# 
# estrapola da raw le colonne
# Game.ID , Group.Level , Starting.Row , Starting.Column

# isola per game ID
# genera grafo con 16 nodi
# (coord_x, coord_y) color

generic_game_nodes <- c("11", "12", "13", "14", "21", "22", "23", "24", "31", "32", "33", "34", "41", "42", "43", "44")

generic_game_edges <- data.frame(to=numeric(), from=numeric())

generic_game_edges[1, ] <- list("11", "12")
generic_game_edges[2, ] <- list("11", "21")
generic_game_edges[3, ] <- list("12", "13") 
generic_game_edges[4, ] <- list("21", "31")
generic_game_edges[5, ] <- list("13", "14")
generic_game_edges[6, ] <- list("31", "41")
generic_game_edges[7, ] <- list("21", "22")
generic_game_edges[8, ] <- list("12", "22")
generic_game_edges[9, ] <- list("22", "23")
generic_game_edges[10, ] <- list("22", "32")
generic_game_edges[11, ] <- list("23", "24") 
generic_game_edges[12, ] <- list("32", "42")
generic_game_edges[13, ] <- list("31", "32") 
generic_game_edges[14, ] <- list("13", "23")
generic_game_edges[15, ] <- list("32", "33") 
generic_game_edges[16, ] <- list("23", "33")
generic_game_edges[17, ] <- list("33", "34") 
generic_game_edges[18, ] <- list("33", "43")
generic_game_edges[19, ] <- list("41", "42") 
generic_game_edges[20, ] <- list("14", "24")
generic_game_edges[21, ] <- list("42", "43") 
generic_game_edges[22, ] <- list("24", "34")
generic_game_edges[23, ] <- list("43", "44") 
generic_game_edges[24, ] <- list("34", "44")



generic_game_nodes <- data.frame(generic_game_nodes)

## crea grafo

g_game <- tbl_graph(nodes = generic_game_nodes, edges = generic_game_edges, directed = FALSE)

## colora i nodi
color_set <- raw[, "Group.Level"]

g_game_1 <- g_game

for(i in 1:16){
  curr_color <- color_set[i]
  
  if (curr_color == 0){
    V(g_game_1)[i]$color <- "yellow"
  }else if(curr_color == 1){
    V(g_game_1)[i]$color <- "green"
  }else if(curr_color == 2){
    V(g_game_1)[i]$color <- "blue"
  }else{
    V(g_game_1)[i]$color <- "purple"
  }
  
}

g_game_2 <- g_game
index <- 1
for(i in 17:32){
  curr_color <- color_set[i]
  
  if (curr_color == 0){
    V(g_game_2)[index]$color <- "yellow"
  }else if(curr_color == 1){
    V(g_game_2)[index]$color <- "green"
  }else if(curr_color == 2){
    V(g_game_2)[index]$color <- "blue"
  }else{
    V(g_game_2)[index]$color <- "purple"
  }
  
  index <- index+1
}

# hamming distance tra i vettori di colori

colors_g_game_1 <- V(g_game_1)$color
colors_g_game_2 <- V(g_game_2)$color

sum(colors_g_game_1 != colors_g_game_2)
# distanza di edit e matching dei nodi
# assirtativita'

# cammini medi?
# distanze tra i nodi ?
# pattern ripetuti ?