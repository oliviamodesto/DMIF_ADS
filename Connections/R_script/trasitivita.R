# PARTE 5
#
#
#
# le parole nel g_stesso_gruppo_main_component sono transitive 
transitivity_main_component <- transitivity(g_stesso_gruppo_main_component)

#
#
#
# BARPLOT


barplot_neighbors = function(dataframe_info){
  
  # conta occorrenza di vicini per ogni parola popolare
  neighbors <-table(dataframe_info$word)
  neighbors <- data.frame(neighbors)
  neighbors <- neighbors[order(neighbors$Freq, decreasing = TRUE), ]
  
  unique_neighbors <- dataframe_info$word
  unique_neighbors <- unique(unique_neighbors)
  
  barplot_data <- matrix(rep(0, (length(unique_neighbors)*4)), nrow = length(unique_neighbors), ncol = 4) 
  
  rownames(barplot_data) <- unique_neighbors
  colnames(barplot_data) <- c("yellow", "green", "blue", "purple")
  
  for (i in 1:nrow(dataframe_info)){
    selected_word <- dataframe_info$word[i]
    selected_difficulty <- dataframe_info$difficulty[i]
    
    barplot_data[selected_word, selected_difficulty] <- barplot_data[selected_word, selected_difficulty] + 1
  }
  
  return(barplot_data)
  
}

YOU_barplot_data <- barplot_neighbors(df_YOU_info)
EWE_barplot_data <- barplot_neighbors(df_EWE_info)
SOLE_barplot_data <- barplot_neighbors(df_SOLE_info)
U_barplot_data <- barplot_neighbors(df_U_info)
YEW_barplot_data <- barplot_neighbors(df_YEW_info)
BALL_barplot_data <- barplot_neighbors(df_BALL_info)
POP_barplot_data <- barplot_neighbors(df_POP_info)
LEAD_barplot_data <- barplot_neighbors(df_LEAD_info)
NEW_barplot_data <- barplot_neighbors(df_NEW_info)
DOWN_barplot_data <- barplot_neighbors(df_DOWN_info)
PERCH_barplot_data <- barplot_neighbors(df_PERCH_info)
FLY_barplot_data <- barplot_neighbors(df_FLY_info)
BULL_barplot_data <- barplot_neighbors(df_BULL_info)
BASS_barplot_data <- barplot_neighbors(df_BASS_info)


#
#
# g_stesso_gruppo_main_component
# fai delle heatmap, una per ogni livello

heatmap_data_g_stesso_gruppo_main_component <- df_edgelist
g_stesso_gruppo_main_component_difficulty <- E(g_stesso_gruppo_main_component)$color

heatmap_data_g_stesso_gruppo_main_component$difficulty <- g_stesso_gruppo_main_component_difficulty

heatmap_yellow_data_g_stesso_gruppo_main_component <- heatmap_data_g_stesso_gruppo_main_component[heatmap_data_g_stesso_gruppo_main_component$difficulty == "yellow",]

heatmap_green_data_g_stesso_gruppo_main_component <- heatmap_data_g_stesso_gruppo_main_component[heatmap_data_g_stesso_gruppo_main_component$difficulty == "green",]

heatmap_blue_data_g_stesso_gruppo_main_component <- heatmap_data_g_stesso_gruppo_main_component[heatmap_data_g_stesso_gruppo_main_component$difficulty == "blue",]

heatmap_purple_data_g_stesso_gruppo_main_component <- heatmap_data_g_stesso_gruppo_main_component[heatmap_data_g_stesso_gruppo_main_component$difficulty == "purple",]



# costruzione matrice di adiacenza
label_heatmap <- heatmap_data_g_stesso_gruppo_main_component$to
label_heatmap <- append(label_heatmap, heatmap_data_g_stesso_gruppo_main_component$from)
label_heatmap <- sort(label_heatmap)
label_heatmap <- unique(label_heatmap)

heatmap_size <- length(label_heatmap)

yellow_heatmap_data <- matrix(rep(0, (heatmap_size*heatmap_size)), nrow = heatmap_size, ncol = heatmap_size) 


for(i in 1:nrow(heatmap_yellow_data_g_stesso_gruppo_main_component)){
  print(i)
  to <- heatmap_yellow_data_g_stesso_gruppo_main_component$to[i]
  from <- heatmap_yellow_data_g_stesso_gruppo_main_component$from[i]
  
  yellow_heatmap_data[to, from] <- yellow_heatmap_data[to, from] + 1
  yellow_heatmap_data[from, to] <- yellow_heatmap_data[from, to] + 1
}

isSymmetric(yellow_heatmap_data) # TRUE

#
# green heatmap
#
#

green_heatmap_data <- matrix(rep(0, (heatmap_size*heatmap_size)), nrow = heatmap_size, ncol = heatmap_size) 


for(i in 1:nrow(heatmap_green_data_g_stesso_gruppo_main_component)){
  print(i)
  to <- heatmap_green_data_g_stesso_gruppo_main_component$to[i]
  from <- heatmap_green_data_g_stesso_gruppo_main_component$from[i]
  
  green_heatmap_data[to, from] <- green_heatmap_data[to, from] + 1
  green_heatmap_data[from, to] <- green_heatmap_data[from, to] + 1
}

isSymmetric(green_heatmap_data) # TRUE

#
# blue heatmap
#
#

blue_heatmap_data <- matrix(rep(0, (heatmap_size*heatmap_size)), nrow = heatmap_size, ncol = heatmap_size) 


for(i in 1:nrow(heatmap_blue_data_g_stesso_gruppo_main_component)){
  print(i)
  to <- heatmap_blue_data_g_stesso_gruppo_main_component$to[i]
  from <- heatmap_blue_data_g_stesso_gruppo_main_component$from[i]
  
  blue_heatmap_data[to, from] <- blue_heatmap_data[to, from] + 1
  blue_heatmap_data[from, to] <- blue_heatmap_data[from, to] + 1
}

isSymmetric(blue_heatmap_data) # TRUE

#
# purple heatmap
#
#

purple_heatmap_data <- matrix(rep(0, (heatmap_size*heatmap_size)), nrow = heatmap_size, ncol = heatmap_size) 


for(i in 1:nrow(heatmap_purple_data_g_stesso_gruppo_main_component)){
  print(i)
  to <- heatmap_purple_data_g_stesso_gruppo_main_component$to[i]
  from <- heatmap_purple_data_g_stesso_gruppo_main_component$from[i]
  
  purple_heatmap_data[to, from] <- purple_heatmap_data[to, from] + 1
  purple_heatmap_data[from, to] <- purple_heatmap_data[from, to] + 1
}

isSymmetric(purple_heatmap_data) # TRUE