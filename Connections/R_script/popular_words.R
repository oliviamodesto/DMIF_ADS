# PARTE 4

## defining function popularity

popularity = function(word) {
  # trova id del vertice con etichetta word

  word_id <- which(df_degree_main_component$label == word)
  word_id <- df_degree_main_component$index[word_id]
  print(word_id)
  
  # costruisci il dataframe con colonne
  info <- c()
  index <- 1
  
  for(i in which(df_edgelist$to == word_id)){
    new_set <- c(i, df_edgelist$to[i], df_edgelist$from[i], E(g_stesso_gruppo_main_component)[i]$color)
    print(new_set)
    info[[index]] <- new_set
    index <- index+1
  }
  
  for(i in which(df_edgelist$from == word_id)){
    new_set <- c(i, df_edgelist$from[i], df_edgelist$to[i], E(g_stesso_gruppo_main_component)[i]$color)
    print(new_set)
    info[[index]] <- new_set
    index <- index+1
  }
  
  df_info <- do.call(rbind, info)
  
  colnames(df_info) <- c("edge_id", word, "node_id",  "difficulty")
  df_info <- data.frame(df_info)
  
  # aggiungi colonna "word"
  
  for (i in 1:nrow(df_info)){
    node_id <- df_info$node_id[i]
    node_id <- strtoi(node_id)
    df_info$word[i] <- V(g_stesso_gruppo_main_component)[node_id]$label #df_dictionary$dictionary[node_id]
  }
  
  # aggiungi colonna ranking
  
  for (i in 1:nrow(df_info)){
    print(df_info$word[i])
    ranking <- which(df_degree_main_component$label == df_info$word[i] )
    
    if(length(ranking) == 0){
      df_info$ranking[i] <- -1
    }else{
      df_info$ranking[i] <- ranking
    }
  }
  
  # leggi i numeri di occerrenze in base alla difficoltà
  
  return(df_info)
}

df_YOU_info <- popularity("YOU")
df_EWE_info <- popularity("EWE")
df_SOLE_info <- popularity("SOLE")
df_U_info <- popularity("U")
df_YEW_info <- popularity("YEW")
df_BALL_info <- popularity("BALL")
df_POP_info <- popularity("POP")
df_LEAD_info <- popularity("LEAD")
df_NEW_info <- popularity("NEW")
df_DOWN_info <- popularity("DOWN")
df_PERCH_info <- popularity("PERCH")
df_FLY_info <- popularity("FLY")
df_BULL_info <- popularity("BULL")
df_BASS_info <- popularity("BASS")
