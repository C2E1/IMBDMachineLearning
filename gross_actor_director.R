library(readr)
library(plyr)
library(ggthemes)
library(ggplot2)
movie_metadata <- read_csv("~/movie_metadata.csv")
moviesUSUK = subset(movie_metadata, country == "USA" | country == "UK")

ratingdat <- ddply(moviesUSUK, c("actor_1_name"), summarise,
                                        M = mean(gross, na.rm=T),
                                        SE = sd(gross, na.rm=T)/sqrt(length(na.omit(imdb_score))),
                                        N = length(na.omit(imdb_score)))
ratings<-ratingdat[which(ratingdat$N>=15),]
ratings$actor_1_name <- factor(ratings$actor_1_name)
ratings$actor_1_name <- reorder(ratings$actor_1_name, ratings$M)
ggplot(ratings, aes(x = M, xmin = M-SE, xmax = M+SE, y = actor_1_name )) +
       geom_point() + 
       geom_segment( aes(x = M-SE, xend = M+SE,
                                                 y = actor_1_name, yend=actor_1_name)) +
       theme(axis.text=element_text(size=8)) +
       xlab("Gross") + ylab("First Actor")

ratingdat <- ddply(moviesUSUK, c("director_name"), summarise,
                   M = mean(gross, na.rm=T),
                   SE = sd(gross, na.rm=T)/sqrt(length(na.omit(imdb_score))),
                   N = length(na.omit(imdb_score)))
ratings<-ratingdat[which(ratingdat$N>=10 & !(ratingdat$director_name=='')),]
ratings$director_name <- factor(ratings$director_name)
ratings$director_name <- reorder(ratings$director_name, ratings$M)
ggplot(ratings, aes(x = M, xmin = M-SE, xmax = M+SE, y = director_name )) +
  geom_point() + 
  geom_segment( aes(x = M-SE, xend = M+SE,
                                           y = director_name, yend=director_name)) +
  theme(axis.text=element_text(size=8)) +
  xlab("Gross") + ylab("Director")


