rm(list=ls())

#### Installation and loading ####

package_list <- c("ggplot2", "dplyr", "leaps", "olsrr", "car", "caTools", "splines", "boot")
for (pack in package_list) {
  install.packages(pack)
}

library(ggplot2)
library(dplyr)
library(leaps)
library(olsrr)
library(car)
library(caTools)
library(splines)
library(boot)

#### Data ####

df_clean = read.csv('C:/Users/paulc/Documents/MMA/Multivariate/midterm/df_clean.csv', encoding="UTF-8")
str(df_clean)

#### 1. Type processing #### 
df_clean$release_month <- as.factor(df_clean$release_month)

#### 2. Subset selection ####

sel <- regsubsets(imdbRating ~ budget + release_month + duration_minutes + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                    genre_adventure + genre_animation +  genre_biography + genre_comedy + genre_documentary + genre_drama + genre_family + genre_fantasy + genre_filmnoir + genre_history +
                    genre_horror + genre_music + genre_musical + genre_scifi + genre_shortfilm + genre_thriller + genre_war + main_actor1_is_female +  main_actor1_star_meter +
                    main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                    language_used_d, data=df_clean, nvmax = 32)

sum_sel <- summary(sel)
sum_sel$cp #Best Mallow's CP is obtained for 25 variables
#This model is obtained with the following variables:

sum_sel$which[25,]

#It has only 22 features because among the 25 many were the different release_month factors
sel_subset <- c("budget", "release_month", "duration_minutes", "total_number_of_spoken_languages", "total_number_of_genres", "genre_action", 
  "genre_animation", "genre_biography", "genre_comedy", "genre_documentary", "genre_drama", "genre_family", "genre_filmnoir", 
  "genre_horror", "genre_musical", "genre_shortfilm", "genre_war", "main_actor1_is_female", "total_number_of_actors", "log_duration", "country_prod_d",  
  "language_used_d")


#### 3. Stepwise ####

model = lm(imdbRating ~ budget + release_month + duration_minutes + total_number_of_spoken_languages + total_number_of_genres + genre_action +
             genre_adventure + genre_animation +  genre_biography + genre_comedy + genre_documentary + genre_drama + genre_family + genre_fantasy + genre_filmnoir + genre_history +
             genre_horror + genre_music + genre_musical + genre_scifi + genre_shortfilm + genre_thriller + genre_war + main_actor1_is_female +  main_actor1_star_meter +
             main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
             language_used_d, data=df_clean)
  
summary(model)
#mallow <- ols_step_best_subset(model)
stepwise <- ols_step_both_p(model)

# 21 variables
sel_stepwise <- c("duration_minutes", "genre_drama", "total_number_of_actors", "genre_action", "genre_documentary", "log_duration", "genre_horror",
          "genre_comedy", "genre_family", "genre_animation", "genre_shortfilm", "genre_filmnoir", "total_number_of_genres", "budget",
          "language_used_d", "main_actor1_is_female", "genre_musical", "genre_war", "release_month", "genre_biography", 
          "total_number_of_spoken_languages")

#### 4. Analyzing differences ####

setdiff(sel_subset, sel_stepwise)#country_prod
setdiff(sel_stepwise, sel_subset)#0

#There is only one difference variable between the two selection methods. We will keep
model = lm(imdbRating ~ country_prod_d, data=df_clean)
summary(model) #p-value suggests to keep it

#### 5. Adding crossed variables ####

df_cross <- df_clean

#crossing genre with continuous variables
df_cross$drama_spoken <- df_clean$total_number_of_spoken_languages*df_clean$genre_drama
df_cross$action_budget <- df_clean$budget*df_clean$genre_action
df_cross$adventure_budget <- df_clean$budget*df_clean$genre_adventure
df_cross$scifi_budget <- df_clean$budget*df_clean$genre_scifi
df_cross$war_budget <- df_clean$budget*df_clean$genre_war
df_cross$short_duration <- df_clean$log_duration*df_clean$genre_shortfilm
df_cross$history_duration <- df_clean$log_duration*df_clean$genre_history
df_cross$documentary_duration <- df_clean$log_duration*df_clean$genre_documentary
df_cross$documentary_spoken <- df_clean$total_number_of_spoken_languages*df_clean$genre_documentary
df_cross$bio_duration <- df_clean$log_duration*df_clean$genre_biography
df_cross$action_actors <- df_clean$total_number_of_actors*df_clean$genre_action
df_cross$adventure_actors <- df_clean$total_number_of_actors*df_clean$genre_adventure
df_cross$scifi_actors <- df_clean$total_number_of_actors*df_clean$genre_scifi
df_cross$war_actors <- df_clean$total_number_of_actors*df_clean$genre_war

#crossing continuous
#budget, duration, total_actors, total_language, total_genres

df_cross$budget_duration = df_clean$budget*df_clean$duration_minutes
df_cross$budget_actors = df_clean$budget*df_clean$total_number_of_actors
df_cross$budget_language = df_clean$budget*df_clean$total_number_of_spoken_languages
df_cross$budget_genres = df_clean$budget*df_clean$total_number_of_genres
df_cross$duration_actors = df_clean$duration_minutes*df_clean$total_number_of_actors
df_cross$duration_language = df_clean$duration_minutes*df_clean$total_number_of_spoken_languages
df_cross$duration_genres = df_clean$duration_minutes*df_clean$total_number_of_genres
df_cross$actors_language = df_clean$total_number_of_spoken_languages*df_clean$total_number_of_actors
df_cross$actors_genres = df_clean$total_number_of_genres*df_clean$total_number_of_actors
df_cross$genres_language = df_clean$total_number_of_spoken_languages*df_clean$total_number_of_genres

model = lm(imdbRating ~ budget + release_month + duration_minutes + total_number_of_spoken_languages + total_number_of_genres + genre_action +  
             genre_animation + genre_biography + genre_comedy + genre_documentary + genre_drama + genre_family + genre_filmnoir +
             genre_horror + genre_musical + genre_shortfilm + genre_war + main_actor1_is_female + total_number_of_actors + log_duration + country_prod_d +  
             language_used_d + drama_spoken + action_budget + adventure_budget + scifi_budget + war_budget + short_duration + history_duration +
             documentary_duration + documentary_spoken + bio_duration + action_actors + adventure_actors + scifi_actors + war_actors + 
             budget_duration + budget_actors + budget_language + budget_genres + duration_actors + duration_language + duration_genres +
             actors_language + actors_genres + genres_language, data=df_cross)

summary(model)

stepwise <- ols_step_both_p(model)

#### 6. Coming up with the final selection of features ####

kept_cross <- c("duration_minutes", "genre_drama", "total_number_of_actors", "genre_action", "genre_documentary", "genre_horror",
                "genre_comedy", "genre_family", "genre_filmnoir", "genre_animation", "budget", "budget_duration",
                "main_actor1_is_female", "duration_genres", "budget_genres", "short_duration", "release_month", "genre_musical", "genre_war",
                "action_budget", "budget_language", "duration_actors", 
                "genre_biography", "total_number_of_spoken_languages", "war_budget")

setdiff(kept_cross, sel_subset) #only crossed variables
setdiff(sel_subset, kept_cross) 
  #total_number_of_genres: we include crossed variables duration_genres and budget_genres. We will investigate how we select these
  #shortfilm: we include shofilm*duration so it is simply a substitution
  #log_duration: removed. Maybe our hypothesis was not relevant
  #country_prod_d: we already saw it was a variable on the edge
  #language_used_d: not many information contained so not surprised

#Now let's investigate our crossed continuous variables related to genre, if they are powerful or not:
summary(lm(imdbRating ~ total_number_of_genres + budget + duration_minutes + duration_genres, data=df_cross))
summary(lm(imdbRating ~ total_number_of_genres + budget + duration_minutes + budget_genres, data=df_cross))
summary(lm(imdbRating ~ total_number_of_genres + budget + duration_minutes + duration_genres + budget_genres, data=df_cross))

#It seems indeed that the total_number of genres corresponds to very low information according to the p-value
#It seems particularly impacted by the duration*genres variable.

df_selec = df_cross[,kept_cross]
df_selec$imdbRating = df_cross$imdbRating
write.csv(df_selec, 'C:/Users/paulc/Documents/MMA/Multivariate/midterm/df_selec.csv')


