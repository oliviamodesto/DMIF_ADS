# PARTE 4
#
#
# trova le coppie più popolari

A_g_stesso_gruppo_main_component <- as_adjacency_matrix(g_stesso_gruppo_main_component)

highest_paircount <- max(A_g_stesso_gruppo_main_component)

pairs <- which(A_g_stesso_gruppo_main_component == highest_paircount, arr.ind = TRUE)

pairs <- data.frame(pairs)

index <- 1
for(i in pairs$row){
  
  
  word_name <- V(g_stesso_gruppo_main_component)[i]$label 
  print(word_name)
  
  pairs$row[index] <- word_name
  index <- index+1
}


index <- 1
for(i in pairs$col){
  
  word_name <- V(g_stesso_gruppo_main_component)[i]$label 
  print(word_name)
  
  pairs$col[index] <- word_name
  index <- index+1
}

rm(index)
rm(word_name)