rm(list=ls())

#### Installation and loading ####

package_list <- c("ggplot2", "dplyr", "leaps", "olsrr", "car", "caTools", "splines", "boot", "lmtest", "plm", "psych")
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
library(lmtest)
library(plm)
library(psych)

#### Data ####

df = read.csv('C:/Users/paulc/Documents/MMA/Multivariate/midterm/df_selec.csv')
str(df)

#### 1. Preparing all possible situations ####

attach(df)

var = c("total_number_of_actors", "duration_minutes", "budget_genres", "duration_genres", "total_number_of_spoken_languages")
for (v in var) {
  knot_n = paste("knot_", v, sep="")
  assign(knot_n, quantile(get(v), probs=c(0.33, 0.66)))
}

degrees = c(2, 3, 4)
degrees_low = c(2, 3)

#Cross-validation
K=20

models = expand.grid(a1 = knot_total_number_of_actors,
                     b1 = knot_duration_minutes,
                     c1 = knot_budget_genres,
                     d1 = knot_duration_genres,
                     e1 = knot_total_number_of_spoken_languages,
                     a2 = degrees,
                     b2 = degrees,
                     c2 = degrees_low,
                     d2 = degrees_low,
                     e2 = degrees)

models_u = unique(models)


#total actors

returning_cv <- function(a1, b1, c1, d1, e1, a2, b2, c2, d2, e2) {
  model_test = glm(imdbRating ~ bs(total_number_of_actors, knots=c(a1), degree=a2)+
                     bs(duration_minutes, knots=c(b1), degree=b2)+
                     bs(budget_genres, knots=c(c1), degree=c2)+
                     bs(duration_genres, knots=c(d1), degree=d2)+
                     bs(total_number_of_spoken_languages, knots=c(e1), degree=e2)+
                     genre_drama + genre_action + genre_documentary + genre_horror + genre_comedy + genre_family + genre_animation + genre_filmnoir +
                     budget_duration + short_duration + main_actor1_is_female + genre_musical + genre_war + release_month + genre_biography + action_budget +
                     budget_language + duration_actors + war_budget)
  #summary(model_test)
  
  cv_test = cv.glm(df, model_test, K=K)$delta[1]
  return(cv_test)
}

#returning_cv(13, 90, 30000000, 180, 1, 2, 4, 3, 2, 2)

#### 2. Running the model selection ####

#attach(models_u)
ptm = proc.time()
models_u$test_m = mapply(returning_cv, models_u$a1, models_u$b1, models_u$c1, models_u$d1, models_u$e1, models_u$a2, models_u$b2, models_u$c2, models_u$d2, models_u$e2)
process_time = proc.time() - ptm
process_time #3996

models_u$test_m

write.csv(models_u, 'C:/Users/paulc/Documents/MMA/Multivariate/midterm/models_results.csv')

best_model = glm(imdbRating ~ bs(total_number_of_actors, knots=c(20), degree=4)+
                   bs(duration_minutes, knots=c(90), degree=4)+
                   bs(budget_genres, knots=c(30000000), degree=3)+
                   bs(duration_genres, knots=c(270), degree=3)+
                   bs(total_number_of_spoken_languages, knots=c(1), degree=3)+
                   genre_drama + genre_action + genre_documentary + genre_horror + genre_comedy + genre_family + genre_animation + genre_filmnoir +
                   budget_duration + short_duration + main_actor1_is_female + genre_musical + genre_war + release_month + genre_biography + action_budget +
                   budget_language + duration_actors + war_budget, data=df)

best_model_lm = lm(imdbRating ~ bs(total_number_of_actors, knots=c(20), degree=4)+
                      bs(duration_minutes, knots=c(90), degree=4)+
                      bs(budget_genres, knots=c(30000000), degree=3)+
                      bs(duration_genres, knots=c(270), degree=3)+
                      bs(total_number_of_spoken_languages, knots=c(1), degree=3)+
                      genre_drama + genre_action + genre_documentary + genre_horror + genre_comedy + genre_family + genre_animation + genre_filmnoir +
                      budget_duration + short_duration + main_actor1_is_female + genre_musical + genre_war + release_month + genre_biography + action_budget +
                      budget_language + duration_actors + war_budget, data=df)

#### 3. Assessment of the model ####

# Linearity ####
residualPlots(best_model_lm)

#Outlier test ####
outlierTest(best_model)
#No outlier

#Heteroskedasticity ####
ncvTest(best_model_lm) #There is heteroskedasticity
correction_hetero <- coeftest(best_model_lm,vcov=vcovHC(best_model_lm,type='HC1'))

#Collinearity
quantvars_1=df[,2:13]
quantvars_2=df[,14:26]
colin_1 <- pairs.panels(quantvars_1)
colin_2 <- pairs.panels(quantvars_2)
