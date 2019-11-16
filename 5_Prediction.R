rm(list=ls())

package_list <- c("ggplot2", "dplyr", "leaps", "car", "caTools", "splines", "boot", "lmtest", "plm", "psych")
for (pack in package_list) {
  install.packages(pack)
}

library(ggplot2)
library(dplyr)
library(leaps)
library(car)
library(caTools)
library(splines)
library(boot)
library(lmtest)
library(plm)
library(psych)

df_selec = read.csv('C:/Users/paulc/Documents/MMA/Multivariate/midterm/df_selec.csv')
df_selec$release_month <- factor(df_selec$release_month)

#### Best model ####
best_model = glm(imdbRating ~ bs(total_number_of_actors, knots=c(20), degree=4)+
                   bs(duration_minutes, knots=c(90), degree=4)+
                   bs(budget_genres, knots=c(30000000), degree=3)+
                   bs(duration_genres, knots=c(270), degree=3)+
                   bs(total_number_of_spoken_languages, knots=c(1), degree=3)+
                   genre_drama + genre_action + genre_documentary + genre_horror + genre_comedy + genre_family + genre_animation + genre_filmnoir +
                   budget_duration + short_duration + main_actor1_is_female + genre_musical + genre_war + release_month + genre_biography + action_budget +
                   budget_language + duration_actors + war_budget, data=df_selec)

best_model_lm = lm(imdbRating ~ bs(total_number_of_actors, knots=c(20), degree=4)+
                   bs(duration_minutes, knots=c(90), degree=4)+
                   bs(budget_genres, knots=c(30000000), degree=3)+
                   bs(duration_genres, knots=c(270), degree=3)+
                   bs(total_number_of_spoken_languages, knots=c(1), degree=3)+
                   genre_drama + genre_action + genre_documentary + genre_horror + genre_comedy + genre_family + genre_animation + genre_filmnoir +
                   budget_duration + short_duration + main_actor1_is_female + genre_musical + genre_war + release_month + genre_biography + action_budget +
                   budget_language + duration_actors + war_budget, data=df_selec)

#### Prediction data ####

predict_clean = read.csv('C:/Users/paulc/Documents/MMA/Multivariate/Midterm/movies_predict.csv', sep=";")
predict_clean$log_duration = log(predict_clean$duration_minutes - 49)
predict_clean$release_month <- factor(predict_clean$release_month)

predict_clean$budget <- as.numeric(predict_clean$budget)
#predict_clean$budget <- rep(0, 12)

str(predict_clean)

predict_cross <- predict_clean

#crossing genre with continuous variables
predict_cross$action_budget <- predict_clean$budget*predict_clean$genre_action
predict_cross$war_budget <- predict_clean$budget*predict_clean$genre_war
predict_cross$short_duration <- predict_clean$log_duration*predict_clean$genre_shortfilm

#crossing continuous
#budget, duration, total_actors, total_language, total_genres

predict_cross$budget_duration = predict_clean$budget*predict_clean$duration_minutes
predict_cross$budget_actors = predict_clean$budget*predict_clean$total_number_of_actors
predict_cross$budget_language = predict_clean$budget*predict_clean$total_number_of_spoken_languages
predict_cross$budget_genres = predict_clean$budget*predict_clean$total_number_of_genres
predict_cross$duration_actors = predict_clean$duration_minutes*predict_clean$total_number_of_actors
predict_cross$duration_language = predict_clean$duration_minutes*predict_clean$total_number_of_spoken_languages
predict_cross$duration_genres = predict_clean$duration_minutes*predict_clean$total_number_of_genres
predict_cross$actors_language = predict_clean$total_number_of_spoken_languages*predict_clean$total_number_of_actors
predict_cross$actors_genres = predict_clean$total_number_of_genres*predict_clean$total_number_of_actors

str(predict_cross)

ncvTest(best_model_lm) #There is heteroskedasticity
correction_hetero <- coeftest(best_model_lm,vcov=vcovHC(best_model_lm,type='HC1'))

#Correcting heteroskedasticity before making final prediction
best_model_lm$coefficients <- correction_hetero[,1]
predict_cross$rating_correc = predict(best_model_lm, predict_cross)
predict_cross$rating = predict(best_model, predict_cross)
#Results are the same

final_results = predict_cross %>% select(movietitle, rating)
write.csv(final_results, 'C:/Users/paulc/Documents/MMA/Multivariate/Midterm/final_results.csv')
