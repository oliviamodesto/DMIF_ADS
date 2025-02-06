# 

date_name_level <- raw[, c("Puzzle.Date", "Group.Name", "Group.Level")]
date_name_level <- data.frame(date_name_level)

#
#
#
#

find_timeline = function(word){
  
  occurrances <- which(date_name_level$Group.Name == word)
  date_occurrances <- c()
  index <- 1
  
  for(i in occurrances){
    date_occurrances[index] <- date_name_level$Puzzle.Date[i]
    index <- index+1
  }
  
  date_occurrances <- unique(date_occurrances)
  
  
  tb <- data.frame(what = date_occurrances,
                   when = ymd(date_occurrances),
                   event.type = word )

  return(tb)
}

HOMOPHONES_tb <- find_timeline("HOMOPHONES")
FISH_tb <- find_timeline("FISH")
ANAGRAMS_tb <- find_timeline("ANAGRAMS")
MUSIC_GENRES_tb <- find_timeline("MUSIC GENRES") 
MUSICAL_INSTRUMENTS_tb <- find_timeline("MUSICAL INSTRUMENTS") 
PALINDROMES_tb <- find_timeline("PALINDROMES")
ANIMAL_GROUP_NAMES_tb <- find_timeline("ANIMAL GROUP NAMES")

ggplot(HOMOPHONES_tb, aes(x = when, y = event.type, label = what)) +
  geom_line() +
  geom_point() +
  geom_text(hjust = -0.3, angle = 45, cex = 2) +  
  scale_x_date(name = "", date_breaks = "1 years") +
  scale_y_discrete(name = "") +
  theme_minimal()


ggplot(FISH_tb, aes(x = when, y = event.type, label = what)) +
  geom_line() +
  geom_point() +
  geom_text(hjust = -0.3, angle = 45, cex = 2) +  
  scale_x_date(name = "", date_breaks = "1 years") +
  scale_y_discrete(name = "") +
  theme_minimal()


ggplot(ANAGRAMS_tb, aes(x = when, y = event.type, label = what)) +
  geom_line() +
  geom_point() +
  geom_text(hjust = -0.3, angle = 45, cex = 2) +  
  scale_x_date(name = "", date_breaks = "1 years") +
  scale_y_discrete(name = "") +
  theme_minimal()


# Rplot38
ggplot(MUSIC_GENRES_tb, aes(x = when, y = event.type, label = what)) +
  geom_line() +
  geom_point() +
  geom_text(hjust = -0.3, angle = 45, cex = 2) +  
  scale_x_date(name = "", date_breaks = "1 years") +
  scale_y_discrete(name = "") +
  theme_minimal()

# Rplot39
ggplot(MUSICAL_INSTRUMENTS_tb, aes(x = when, y = event.type, label = what)) +
  geom_line() +
  geom_point() +
  geom_text(hjust = -0.3, angle = 45, cex = 2) +  
  scale_x_date(name = "", date_breaks = "1 years") +
  scale_y_discrete(name = "") +
  theme_minimal()

# Rplot40
ggplot(PALINDROMES_tb, aes(x = when, y = event.type, label = what)) +
  geom_line() +
  geom_point() +
  geom_text(hjust = -0.3, angle = 45, cex = 2) +  
  scale_x_date(name = "", date_breaks = "1 years") +
  scale_y_discrete(name = "") +
  theme_minimal()

# Rplot41
ggplot(ANIMAL_GROUP_NAMES_tb, aes(x = when, y = event.type, label = what)) +
  geom_line() +
  geom_point() +
  geom_text(hjust = -0.3, angle = 45, cex = 2) +  
  scale_x_date(name = "", date_breaks = "1 years") +
  scale_y_discrete(name = "") +
  theme_minimal()
