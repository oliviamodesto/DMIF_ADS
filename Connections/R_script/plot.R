# Rplot
ggraph(g_stesso_gruppo, layout = 'stress') + 
  geom_edge_link(color = "lightblue",  arrow = arrow(type = "closed", length = unit(0.5, "mm")), alpha = 0.3 ) + 
  geom_node_point(colour = "blue", size = 0.5, alpha = 0.03) +
  theme_graph(background = "white")

# Rplot01
par(bg = 'grey')
plot(g_stesso_gruppo_main_component, vertex.label=NA, vertex.size=1, layout = layout_as_tree)


# Rplot02
plot(1:nrow(df_degree_main_component), df_degree_main_component$degree, 
     main= "Degree dei nodi ",
     xlab= "number",
     ylab= "degree",
     col= "lightblue",
     pch = 15,
     cex = 0.4,
     lty = "solid",
     lwd = 2)

# Rplot03 -> sostituisci con un barplot
plot(1:nrow(df_degree_main_component), df_degree_main_component$degree, 
     main= "Degree dei nodi ",
     xlab= "number",
     ylab= "degree",
     col= "lightblue",
     pch = 15,
     cex = 0.4,
     lty = "solid",
     lwd = 2)

text(1:nrow(df_degree_main_component), df_degree_main_component$degree, labels=df_degree_main_component$label, cex= 0.3, pos=4)

# Rplot23

barplot(df_degree_main_component$degree[1:15],
        names.arg = df_degree_main_component$label[1:15],
        las=2,
        cex.name=1,
        ylim=c(0, 80),
        col= "lightblue",
        main= "Parole più popolari",
        ylab= "degree",
        xpd = FALSE)


# Rplot04
library(wordcloud2)
library(webshot)
#webshot::install_phantomjs()

wordcloud <- df_degree_main_component$label
wordcloud <- data.frame(wordcloud)
wordcloud$freq <- df_degree_main_component$degree
colnames(wordcloud) <- c("word", "freq")

stessa_partita_wordcloud <- wordcloud2(data=wordcloud, size=0.5)

# save it in html
library("htmlwidgets")
saveWidget(stessa_partita_wordcloud,"tmp.html",selfcontained = F)

# and in png or pdf
webshot("tmp.html","Rplot04.png", delay =10, vwidth = 1200, vheight=1200)

rm(wordcloud)
rm(stessa_partita_wordcloud)


# RPlot19
heatmap(yellow_heatmap_data)
legend(x = "bottomright", legend = 0:9, cex = 0.8) 

# RPlot20
heatmap(green_heatmap_data)

# RPlot21
heatmap(blue_heatmap_data)

# RPlot22
heatmap(purple_heatmap_data)

#
#
# stessa partita
#
# 'stress'

# Rplot24
ggraph(g_stessa_partita, layout = 'stress') + 
  geom_edge_link(color = "lightblue",  arrow = arrow(type = "closed", length = unit(0.5, "mm")), alpha = 0.3 ) + 
  geom_node_point(colour = "blue", size = 0.5, alpha = 0.03) +
  theme_graph(background = "white")

# Rplot25
par(bg = 'grey')
plot(g_stessa_partita, vertex.label=NA, vertex.size=1, layout = layout_as_tree)


# Rplot27
library(wordcloud2)
library(webshot)
webshot::install_phantomjs()


#degree_g_stessa_partita

wordcloud <- degree_g_stessa_partita$word
wordcloud <- data.frame(wordcloud)
wordcloud$freq <- degree_g_stessa_partita$degree
colnames(wordcloud) <- c("word", "freq")

stessa_partita_wordcloud <- wordcloud2(data=wordcloud, size=0.1)

# save it in html
library("htmlwidgets")
saveWidget(stessa_partita_wordcloud,"tmp.html",selfcontained = F)

# and in png or pdf
webshot("tmp.html","Rplot27.png", delay =20, vwidth = 800, vheight=600)

rm(wordcloud)
rm(stessa_partita_wordcloud)


# Rplot28

relabeled_group_num_occurances <- group_num_occurances

rownames(relabeled_group_num_occurances) <- seq(1,2183)


barplot(relabeled_group_num_occurances$freq[1:25]/4,
        names.arg = relabeled_group_num_occurances$name[1:25],
        las=2,
        cex.name=0.5,
        ylim=c(0, 10),
        col= "lightblue",
        main= "Riutilizzo di gruppi",
        ylab= "numero utilizzi",
        xpd = FALSE)

#, aes(x=group_num_occurances$name, y=group_num_occurances$freq))

# Rplot29
ggplot(HOMOPHONES_tb, aes(x = when, y = event.type, label = what)) +
  geom_line() +
  geom_point() +
  geom_text(hjust = -0.3, angle = 45, cex = 2) + # colour = issues.tb$difficulty, 
  scale_x_date(name = "", date_breaks = "1 years") +
  scale_y_discrete(name = "") +
  theme_minimal()


# Rplot30
ggplot(FISH_tb, aes(x = when, y = event.type, label = what)) +
  geom_line() +
  geom_point() +
  geom_text(hjust = -0.3, angle = 45, cex = 2) + # colour = issues.tb$difficulty, 
  scale_x_date(name = "", date_breaks = "1 years") +
  scale_y_discrete(name = "") +
  theme_minimal()

# Rplot32
barplot(degree_g_stessa_partita$degree[1:25],
        names.arg = degree_g_stessa_partita$word[1:25],
        las=2,
        cex.name=0.5,
        ylim=c(0, 300),
        col= "lightblue",
        main= "Parole più popolari",
        ylab= "degree",
        xpd = FALSE)

# Rplot33

barplot_eigen_main_component <- main_component_eigenvector_centrality
rownames( barplot_eigen_main_component ) <- seq(1:nrow(barplot_eigen_main_component))

barplot(barplot_eigen_main_component$eigen[1:15],
        names.arg = barplot_eigen_main_component$word[1:15],
        las=2,
        cex.name=1,
        ylim=c(0, 1.5),
        col= "lightblue",
        main= "Parole più popolari",
        ylab= "degree",
        xpd = FALSE)

rm(barplot_eigen_main_component)

# RPlot34

barplot_eigen_stesso_gruppo <- eigen_centrality_g_stessa_partita
rownames( barplot_eigen_stesso_gruppo ) <- seq(1:nrow(barplot_eigen_stesso_gruppo))

barplot(barplot_eigen_stesso_gruppo$eigen[1:25],
        names.arg = barplot_eigen_stesso_gruppo$word[1:25],
        las=2,
        cex.name=0.5,
        ylim=c(0, 1.5),
        col= "lightblue",
        main= "Parole più popolari",
        ylab= "degree",
        xpd = FALSE)

rm(barplot_eigen_main_component)