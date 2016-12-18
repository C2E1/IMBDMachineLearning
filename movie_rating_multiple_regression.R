movies = read.csv("movie_metadata.csv")
movies = subset(movies, country == "USA")
ms = na.omit(movies)
ms$Colord[ms$color == "Color"] = 1
ms$Colord[ms$color != "Color"] = 0
msp = ms[, c("imdb_score",
                                        "director_facebook_likes",
                                        "duration", 
                                        "actor_1_facebook_likes",
                                        "actor_2_facebook_likes",
                                        "actor_3_facebook_likes",
                                        "title_year", 
                                        "facenumber_in_poster",
                                        "Colord",
                                        "budget")]


model.saturated = lm(imdb_score ~ .-actor_3_facebook_likes, data = msp)
summary(model.saturated)

