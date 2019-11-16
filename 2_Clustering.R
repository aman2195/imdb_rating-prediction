# Clustering try
rm(list=ls())

##### Installation and loading ####

package_list = c("dplyr", "cluster", "rattle.data", "car", "caTools", "NbClust")
for (pack in package_list) {
  install.packages(pack)
}

library(dplyr)
library(cluster)
library(rattle.data)
library(NbClust)
library(car)
library(caTools)

#### Processing ####

df_clean = read.csv('C:/Users/paulc/Documents/MMA/Multivariate/midterm/df_clean.csv', encoding="UTF-8")
df_backup = df_clean[,2:33]

df_clean = df_clean[,3:33]
df_std <- data.frame(scale(df_clean))

# wssplot <- function(df, nc=15, seed=1234){
#   wss <- (nrow(df)-1)*sum(apply(df,2,var))
#   for (i in 2:nc){
#     set.seed(seed)
#     wss[i] <- sum(kmeans(df, centers=i)$withinss)}
#   
#   plot(1:nc, wss, type="b", xlab="Number of Clusters",
#        ylab="Within groups sum of squares")
# }
# 
# wssplot(df_std)

#Using wwsplot we can see that 7 is the best number of clusters. However one of the drawbacks of this number is that it creates 
#Clusters with small number of observations. Therefore we used also a 4 cluster approach which seemed a good compromise.


#### 1. Clustering ####

#Clustering regression - Rank 7
fit.km_7 <- kmeans(df_std,centers = 7, iter.max = 5000)
#table(fit.km$cluster, df_clean$genre_action*df_clean$log_duration)

df_clean$cluster_id_7 = fit.km_7$cluster
df_clean %>% group_by(cluster_id_7) %>% summarize(n=length(cluster_id_7))

#Clustering regression - Rank 3
fit.km_4 <- kmeans(df_std,centers = 4, iter.max = 5000)
#table(fit.km$cluster, df_clean$genre_action*df_clean$log_duration)

df_clean$cluster_id_4 = fit.km_4$cluster
df_clean %>% group_by(cluster_id_4) %>% summarize(n=length(cluster_id_4))

#### 2. Sampling ####

df_clean$release_month <- factor(df_clean$release_month)
df_clean$imdbRating <- df_backup$imdbRating
str(df_clean)

set.seed(123)
df_clean$sample <- sample.split(df_backup$imdbRating, SplitRatio=0.7)
train=subset(df_clean, sample==TRUE)
test=subset(df_clean, sample==FALSE)

#### 3. Naive approach ####

model_naive = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                   genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                   genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                   main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                   language_used_d, data=train)

test$pred_naive = predict(model_naive, test)
test$diff_naive = test$imdbRating - test$pred_naive
result_naive = sum(test$diff_naive^2, na.rm=T)

summary(model_naive)

#### 4. Clustering approach rank 7 ####

df_1 = train %>% filter(cluster_id_7 == 1)
df_2 = train %>% filter(cluster_id_7 == 2)
df_3 = train %>% filter(cluster_id_7 == 3)
df_4 = train %>% filter(cluster_id_7 == 4)
df_5 = train %>% filter(cluster_id_7 == 5)
df_6 = train %>% filter(cluster_id_7 == 6)
df_7 = train %>% filter(cluster_id_7 == 7)

test_1 = test %>% filter(cluster_id_7 == 1)
test_2 = test %>% filter(cluster_id_7 == 2)
test_3 = test %>% filter(cluster_id_7 == 3)
test_4 = test %>% filter(cluster_id_7 == 4)
test_5 = test %>% filter(cluster_id_7 == 5)
test_6 = test %>% filter(cluster_id_7 == 6)
test_7 = test %>% filter(cluster_id_7 == 7)


model_c_1 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_1)

model_c_2 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_2)

model_c_3 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_3)

model_c_4 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_4)

model_c_5 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_5)

model_c_6 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_6)

model_c_7 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_7)


test_1$pred_1 = predict(model_c_1, test_1)
test_1$diff_1 = test_1$imdbRating - test_1$pred_1
result_1 = sum(test_1$diff_1^2, na.rm=T)

test_2$pred_2 = predict(model_c_2, test_2)
test_2$diff_2 = test_2$imdbRating - test_2$pred_2
result_2 = sum(test_2$diff_2^2, na.rm=T)

test_3$pred_3 = predict(model_c_3, test_3)
test_3$diff_3 = test_3$imdbRating - test_3$pred_3
result_3 = sum(test_3$diff_3^2, na.rm=T)

test_4$pred_4 = predict(model_c_4, test_4)
test_4$diff_4 = test_4$imdbRating - test_4$pred_4
result_4 = sum(test_4$diff_4^2, na.rm=T)

test_5$pred_5 = predict(model_c_5, test_5)
test_5$diff_5 = test_5$imdbRating - test_5$pred_5
result_5 = sum(test_5$diff_5^2, na.rm=T)

test_6$pred_6 = predict(model_c_6, test_6)
test_6$diff_6 = test_6$imdbRating - test_6$pred_6
result_6 = sum(test_6$diff_6^2, na.rm=T)

test_7$pred_7 = predict(model_c_7, test_7)
test_7$diff_7 = test_7$imdbRating - test_7$pred_7
result_7 = sum(test_7$diff_7^2, na.rm=T)


#### 5. Clustering approach rank 4 ####

df_1 = train %>% filter(cluster_id_4 == 1)
df_2 = train %>% filter(cluster_id_4 == 2)
df_3 = train %>% filter(cluster_id_4 == 3)
df_4 = train %>% filter(cluster_id_4 == 4)

test_1 = test %>% filter(cluster_id_4 == 1)
test_2 = test %>% filter(cluster_id_4 == 2)
test_3 = test %>% filter(cluster_id_4 == 3)
test_4 = test %>% filter(cluster_id_4 == 4)


model_c_1 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_1)

model_c_2 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_2)

model_c_3 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_3)

model_c_4 = lm(imdbRating ~ release_month + total_number_of_spoken_languages + total_number_of_genres + genre_action +
                 genre_biography + genre_documentary + genre_drama + genre_filmnoir + genre_history +
                 genre_horror + genre_musical + genre_scifi + genre_shortfilm + genre_war + main_actor1_star_meter +
                 main_actor2_star_meter + main_actor3_star_meter + total_number_of_actors + log_duration + country_prod_d +
                 language_used_d, data=df_4)

test_1$pred_1 = predict(model_c_1, test_1)
test_1$diff_1 = test_1$imdbRating - test_1$pred_1
result_1 = sum(test_1$diff_1^2, na.rm=T)

test_2$pred_2 = predict(model_c_2, test_2)
test_2$diff_2 = test_2$imdbRating - test_2$pred_2
result_2 = sum(test_2$diff_2^2, na.rm=T)

test_3$pred_3 = predict(model_c_3, test_3)
test_3$diff_3 = test_3$imdbRating - test_3$pred_3
result_3 = sum(test_3$diff_3^2, na.rm=T)

test_4$pred_4 = predict(model_c_4, test_4)
test_4$diff_4 = test_4$imdbRating - test_4$pred_4
result_4 = sum(test_4$diff_4^2, na.rm=T)


#### 6. Results ####

print('naive')
print(result_naive)/nrow(test)

print('cluster')
# Run either paragraph 4 or 5 to plot the results
#print(result_1 + result_2 + result_3 + result_4 + result_5 + result_6 + result_7)/nrow(test)
print(result_1 + result_2 + result_3 + result_4)/nrow(test)
