view_model = function(model) {
  par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
  plot(model)
  par(mfrow=c(1,1)) # Change back to 1 x 1
}
movies = read.csv("movie_metadata.csv")
ms = subset(movies, country == "USA")
ms = na.omit(movies) #Take out empty data
ms$Colord[ms$color == "Color"] = 1 #Color dummy variable
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


skewness(msp$imdb_score) #skewed, evidence for not normal
hist(msp$imdb_score)
model.empty = lm(imdb_score ~ 1, data = msp) #The model with an intercept ONLY.
model.full = lm(imdb_score ~ ., data = msp) #The model with ALL variables
scope = list(lower = formula(model.empty), upper = formula(model.full))
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
summary(forwardAIC) #step regression take out actor 3
view_model(forwardAIC) # normality bot satisfied
confint(forwardAIC)
bc = boxCox(forwardAIC) #makes model more linear, homosckedastic, normal errors
lambda = bc$x[which(bc$y == max(bc$y))] # Extracting the best lambda value
imdb_score.bc = (ms$imdb_score^lambda - 1)/lambda #transforms output vars
skewness(imdb_score.bc) # alot less skewess
hist(imdb_score.bc)
model.bc = lm(imdb_score.bc ~ director_facebook_likes + facenumber_in_poster + 
                title_year + Colord + actor_1_facebook_likes + actor_2_facebook_likes
                + budget + duration, data=ms)
summary(model.bc)
view_model(model.bc) #normality assumption holds
