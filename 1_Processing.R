rm(list=ls())

#### Installation ####
package_list = c("dplyr", "ggplot2", "car", "stringr", "gridExtra", "wesanderson", "lmtest", "sandwich", "psych")
for (pack in package_list) {
  install.packages(pack)
}

#### Loading ####
library(dplyr)
library(ggplot2)
library(car)
library(stringr)
library(gridExtra)
library(wesanderson)
library(lmtest)
library(sandwich)
library(psych)

df = read.csv('C:/Users/paulc/Documents/MMA/Multivariate/midterm/films_dataset_2019.csv', encoding="UTF-8")
str(df)

id = 'film_title'

# We are going to create two lists, one containing variables we definitely don't want to include in our model
# and the other one containing variables we will consider
# We don't want to keep all the variables for the feature selection because it would take too much time and it
# will also give us a better understanding of the relation between rating and these variables.
to_drop = c('imdb_id', 'url' )
to_drop_maybe = c('release_day, release_year')

#Note that in order to process the data you only need to run steps 0, 8 and 9.
#The other steps are only analysis


#### 0. Preliminaries ####
#Creating a manipulable variable
#df$main_actor_know_for_wDate = str_sub(df$main_actor1_known_for, 1, str_length(df$main_actor1_known_for)-7)

#Creating a factor for rating that we will use for plots
df$imdbRating_factor = factor(trunc(df$imdbRating))

#US country prod
df$country_prod = ifelse(df$main_production_country == "United States of America", "US", "Other")

#English language
df$language_used = ifelse(df$main_spoken_language == "English", "English", "Other")

#log_duration: as we found out during our analysis (it will be developed in paragraph 3.5)
df$log_duration = ifelse(df$duration_minutes < 50, 1, log(df$duration_minutes-49))

#star meter as a numeric variable
df$main_actor1_star_meter = as.numeric(df$main_actor1_star_meter)
df$main_actor2_star_meter = as.numeric(df$main_actor2_star_meter)
df$main_actor3_star_meter = as.numeric(df$main_actor3_star_meter)

#Removing badly rated movies (often non-Hollywood)
# Half of the range of possible values is below 0 and 5, the other half being between 5 and 10.
# However very few movies (303) have a rating below 5. We want to calibrate our regression on
# an interval that is not too large and therefore we will use "good" movies only in the training dataset
df_backup <- df
df = df %>% filter(imdbRating >= 5)

#### 1. Removing unuseful genres ####
list_styles = c("action", "adventure", "animation", "biography", "comedy", "crime", "documentary", "drama", "family", "fantasy", "filmnoir", "history", "horror", "music", "musical", "mystery", "realitytv", "romance", "scifi", "shortfilm", "sport", "thriller", "war", "western")
list_genres = paste("genre_", list_styles, sep="")

for (genre in list_genres) {
  
  df[[genre]] <- factor(df[[genre]])
  
  assign(paste("violin_", genre, sep=""),
         ggplot(df, aes_string(x=genre, y="imdbRating", fill=genre))+
           geom_violin()+
           scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
           theme_classic()
  )
}

violin_genre_action #1 We can see action movies are usually rated lower than non action ones
violin_genre_adventure # 1 idem
violin_genre_animation # 1 The distirbutions are really different
violin_genre_biography # 1 Biographical movies are better
violin_genre_comedy # 1
violin_genre_crime # 0 There does not seem to be any difference between the distributions
violin_genre_documentary # 1
violin_genre_drama # 1
violin_genre_family # 1
violin_genre_fantasy # 1
violin_genre_filmnoir # 1
violin_genre_history # 1
violin_genre_horror # 1
violin_genre_music # 1
violin_genre_musical # 1
violin_genre_mystery # tbd it is not clear if it has an impact
violin_genre_realitytv # 0
violin_genre_romance # 0
violin_genre_scifi # 1
violin_genre_shortfilm #1
violin_genre_sport #0
violin_genre_thriller # 1
violin_genre_war #1
violin_genre_western #0

to_drop = c(to_drop, "genre_crime", "genre_realitytv", "genre_romance", "genre_sport", "genre_western")
to_drop_maybe = c(to_drop_maybe, "genre_mystery")

#### 2. Removing other binary variables ####
other_bin = c("main_actor1_is_female", "main_actor2_is_female", "main_actor3_is_female", "main_director_is_female")

for (bin in other_bin) {
  
  df[[bin]] <- factor(df[[bin]])
  
  assign(paste("violin_", bin, sep=""),
         ggplot(df, aes_string(x=bin, y="imdbRating", fill=bin))+
           geom_violin()+
           scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
           theme_classic()
  )
}

violin_main_actor1_is_female #1 Even if the distributions seem to be the same, we have the intuition that this variable would still make sense and we will keep it 
violin_main_actor2_is_female #0 The distributions appear different however we have the intuition that this variable will not make much sense. A movie can always be summarized with a main actor and other important actors who have less impact.
violin_main_actor3_is_female #0  
violin_main_director_is_female #tbd

to_drop = c(to_drop, "main_actor2_is_female", "main_actor3_is_female")
to_drop_maybe = c(to_drop_maybe, "main_director_is_female")

#### 3. Integer variables ####
# 3.1. Preparation ####
integer_var = df%>%select(imdbRating, budget, duration_minutes, total_number_of_genres, total_number_of_actors, total_number_of_directors, total_number_of_producers, total_number_of_production_companies, total_number_of_production_countries, total_number_of_spoken_languages)

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )

plot_distrib <- function(var) {
  main <- ggplot(df, aes_string(x=var, y="imdbRating", color = "imdbRating_factor"))+
    geom_point()+
    #geom_smooth()+
    scale_color_manual(values = wes_palette("Rushmore", 5))+
    theme_classic()
  
  x_density <- ggplot(df, aes_string(x=var, color="imdbRating_factor"))+
    geom_density(alpha=0.5)+
    scale_color_manual(values = wes_palette("Rushmore", 5))+
    theme_classic()
  
  y_density <- ggplot(df, aes_string(x="imdbRating", color="imdbRating_factor"))+
    geom_density(alpha=0.5)+
    scale_color_manual(values = wes_palette("Rushmore", 5))+
    theme_classic()
  
  grid.arrange(x_density, blankPlot, main, y_density, 
               ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
  
  return("Success")
}


# 3.2. Collinearity ####
pairs.panels(df%>%select(imdbRating, budget, duration_minutes, total_number_of_genres, total_number_of_actors, total_number_of_directors, total_number_of_producers, total_number_of_production_companies, total_number_of_production_countries, total_number_of_spoken_languages))

# 3.3. BUDGET ####

model_budget = lm(df$imdbRating ~ df$budget)
summary(model_budget) #p-value indicates it is significant

residualPlots(model_budget) #Tukey test says not linear
ncvTest(model_budget) #No heteroskedasticity
#coeftest(model_budget, vcov=vcovHC(model_budget, type='HC1'))

plot_distrib("budget")
#doesn't seem to have any influence ... but it makes sense to keep it


# 3.4. DURATION_MINUTES ####

model_duration_minutes = lm(df$imdbRating ~ df$duration_minutes)
summary(model_duration_minutes) #p-value says significant

residualPlots(model_duration_minutes) #Tukey test says not linear
ncvTest(model_duration_minutes) #Heteroskedasticity

#Has an influence, let's look at a logarithm version of it
plot_distrib("duration_minutes")


# 3.5. LOG_DURATION ####
# really seems correlated linearly
df$log_duration = ifelse(df$duration_minutes < 50, 1, log(df$duration_minutes-49))

model_log_duration = lm(df$imdbRating ~ df$log_duration)
summary(model_log_duration) #p-value says significant

residualPlots(model_log_duration) #Tukey test says not linear still ...
ncvTest(model_log_duration) #Heteroskedasticity

plot_distrib("log_duration")

# 3.6. TOTAL_NUMBER_OF_GENRES ####
model_total_number_of_genres = lm(df$imdbRating ~ df$total_number_of_genres)
summary(model_total_number_of_genres)

residualPlots(model_total_number_of_genres) #Tukey test says linear
ncvTest(model_total_number_of_genres) #Heteroskedasticity

# Genre definitively has a role to play
plot_distrib("total_number_of_genres")

# 3.7. TOTAL_NUMBER_OF_ACTORS ####
model_total_number_of_actors = lm(df$imdbRating ~ df$total_number_of_actors)
summary(model_total_number_of_actors)

residualPlots(model_total_number_of_actors) #Tukey test says not linear
ncvTest(model_total_number_of_actors) #Heteroskedasticity

# Number of actors definitively has a role to play
plot_distrib("total_number_of_actors")

# 3.8. TOTAL_NUMBER_OF_DIRECTORS ####
model_total_number_of_directors = lm(df$imdbRating ~ df$total_number_of_directors)
summary(model_total_number_of_directors) #very bad p-value

residualPlots(model_total_number_of_directors) #Tukey test says linear
ncvTest(model_total_number_of_directors) #No Heteroskedasticity

# Number of directors definitively has no role to play
plot_distrib("total_number_of_directors") # to remove

# 3.9. TOTAL_NUMBER_OF_PRODUCERS ####
model_total_number_of_producers = lm(df$imdbRating ~ df$total_number_of_producers)
summary(model_total_number_of_producers) #very bad p-value

residualPlots(model_total_number_of_producers) #Tukey test says not linear
ncvTest(model_total_number_of_producers) #No Heteroskedasticity

# Number of producers definitively has no role to play
plot_distrib("total_number_of_producers") # to remove

# 3.10. TOTAL_NUMBER_OF_PRODUCTION_COMPANIES ####
model_total_number_of_production_companies = lm(df$imdbRating ~ df$total_number_of_production_companies)
summary(model_total_number_of_production_companies) #bad p-value

residualPlots(model_total_number_of_production_companies) #Tukey test says not linear
ncvTest(model_total_number_of_production_companies) #No Heteroskedasticity

# Number of production_companies definitively has no role to play
plot_distrib("total_number_of_production_companies") # to remove

# 3.11. TOTAL_NUMBER_OF_PRODUCTION_COUNTRIES ####
model_total_number_of_production_countries = lm(df$imdbRating ~ df$total_number_of_production_countries)
summary(model_total_number_of_production_countries) # very bad p-value

residualPlots(model_total_number_of_production_countries) #Tukey test says linear
ncvTest(model_total_number_of_production_countries) #Heteroskedasticity

# Number of production_countries definitively has no role to play
plot_distrib("total_number_of_production_countries") # to remove

# 3.12. TOTAL_NUMBER_OF_SPOKEN_LANGUAGES ####
model_total_number_of_spoken_languages = lm(df$imdbRating ~ df$total_number_of_spoken_languages)
summary(model_total_number_of_spoken_languages) #good p-value

residualPlots(model_total_number_of_spoken_languages) #Tukey test says linear
ncvTest(model_total_number_of_spoken_languages) #Heteroskedasticity

# Number of spoken_languages definitively has a role to play
plot_distrib("total_number_of_spoken_languages") # we can keep

# 3.13. Conclusion ####
#We remove variables for which p-values are really not good.
to_drop = c(to_drop, "total_number_of_directors", "total_number_of_producers", "total_number_of_production_companies", "total_number_of_production_countries")
#to_drop_maybe = c(to_drop_maybe) no new variable on the edge


#### 4. Dates ####

#release_day
scatter_release_day <- ggplot(df, aes_string(x="release_day", y="imdbRating", color = "imdbRating_factor"))+
  geom_point()+
  #geom_smooth()+
  scale_color_manual(values = wes_palette("Rushmore", 5))+
  theme_classic()
scatter_release_day

release_day_x_density <- ggplot(df, aes_string(x="release_day", color="imdbRating_factor"))+
  geom_density(alpha=0.5)+
  scale_color_manual(values = wes_palette("Rushmore", 5))+
  theme_classic()


release_day_y_density <- ggplot(df, aes_string(x="imdbRating", color="imdbRating_factor"))+
  geom_density(alpha=0.5)+
  scale_color_manual(values = wes_palette("Rushmore", 5))+
  theme_classic()

grid.arrange(release_day_x_density, blankPlot, scatter_release_day, release_day_y_density, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))


#release_month
#June/July and end of the year have a premium.
#Having quarter is not a bad idea
scatter_release_month <- ggplot(df, aes_string(x="release_month", y="imdbRating", color = "imdbRating_factor"))+
  geom_point()+
  #geom_smooth()+
  scale_color_manual(values = wes_palette("Rushmore", 5))+
  theme_classic()
scatter_release_month

release_month_x_density <- ggplot(df, aes_string(x="release_month", color="imdbRating_factor"))+
  geom_density(alpha=0.5)+
  scale_color_manual(values = wes_palette("Rushmore", 5))+
  theme_classic()


release_month_y_density <- ggplot(df, aes_string(x="imdbRating", color="imdbRating_factor"))+
  geom_density(alpha=0.5)+
  scale_color_manual(values = wes_palette("Rushmore", 5))+
  theme_classic()

grid.arrange(release_month_x_density, blankPlot, scatter_release_month, release_month_y_density, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#release_year
#It basically just says we have more average movies nowadays
#I would maybe remove it just take into account Imdb's new movie premium
scatter_release_year <- ggplot(df, aes_string(x="release_year", y="imdbRating", color = "imdbRating_factor"))+
  geom_point()+
  #geom_smooth()+
  scale_color_manual(values = wes_palette("Rushmore", 5))+
  theme_classic()
scatter_release_year

release_year_x_density <- ggplot(df, aes_string(x="release_year", color="imdbRating_factor"))+
  geom_density(alpha=0.5)+
  scale_color_manual(values = wes_palette("Rushmore", 5))+
  theme_classic()


release_year_y_density <- ggplot(df, aes_string(x="imdbRating", color="imdbRating_factor"))+
  geom_density(alpha=0.5)+
  scale_color_manual(values = wes_palette("Rushmore", 5))+
  theme_classic()

grid.arrange(release_year_x_density, blankPlot, scatter_release_year, release_year_y_density, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#Conclusion

to_drop <- c(to_drop, "release_day", "release_year")

#### 5. Actors tiers based on star_metric ####

# 5.1. Prep ####
df$main_actor1_star_meter = as.numeric(df$main_actor1_star_meter)
df$main_actor2_star_meter = as.numeric(df$main_actor2_star_meter)
df$main_actor3_star_meter = as.numeric(df$main_actor3_star_meter)

# 5.2. Main actor 1 ####
model_main_actor1_star_meter = lm(df$imdbRating ~ df$main_actor1_star_meter)
summary(model_main_actor1_star_meter) # very bad p-value

residualPlots(model_main_actor1_star_meter) #Tukey test says linear
ncvTest(model_main_actor1_star_meter) # No Heteroskedasticity

plot_distrib("main_actor1_star_meter") # no influence

# 5.3. Main actor 2 ####
model_main_actor2_star_meter = lm(df$imdbRating ~ df$main_actor2_star_meter)
summary(model_main_actor2_star_meter) #bad p-value

residualPlots(model_main_actor2_star_meter) #Tukey test says linear
ncvTest(model_main_actor2_star_meter) #Heteroskedasticity

plot_distrib("main_actor2_star_meter") # we should remove
# 5.4. Main actor 3 ####
model_main_actor3_star_meter = lm(df$imdbRating ~ df$main_actor3_star_meter)
summary(model_main_actor3_star_meter) #bad p-value

residualPlots(model_main_actor3_star_meter) #Tukey test says linear
ncvTest(model_main_actor3_star_meter) #Heteroskedasticity

plot_distrib("main_actor3_star_meter") # we can keep



# 5.5. Conclusion ####
#Though we have very bad results we would like to keep these variables as they appear to make sense
#Maybe they will be kept by our feature selection
#to_drop <- c(to_drop, "main_actor1_star_meter", "main_actor2_star_meter", "main_actor3_star_meter")

#### 6. Direction/Production treatment ####

studio = c("main_director_name", "main_producer_name", "editor_name", "main_production_company", "main_production_country")

director_analysis = df %>% group_by(main_director_name) %>% summarize(n = length(main_director_name),
                                                                      rating = mean(imdbRating))# We will tier

producer_analysis = df %>% group_by(main_producer_name) %>% summarize(n = length(main_producer_name),
                                                                      rating = mean(imdbRating))# we will tier

editor_analysis = df %>% group_by(editor_name) %>% summarize(n = length(editor_name),
                                                             rating = mean(imdbRating))# we will tier

production_analysis = df %>% group_by(main_production_company) %>% summarize(n = length(main_production_company),
                                                                             rating = mean(imdbRating))# we will tier

country_analysis = df %>% group_by(main_production_country) %>% summarize(n = length(main_production_country),
                                                                          rating = mean(imdbRating))

to_drop <- c(to_drop, studio)

#### 7. Language and countries ####

language_analysis = df %>% group_by(main_spoken_language) %>% summarize(n = length(main_spoken_language),
                                                                        avg_rating = mean(imdbRating))

to_drop = c(to_drop, "main_spoken_language")

#### 8. Final to_drops ####
#This is just a summary of all the steps. 

#1
to_drop = c(to_drop, "genre_crime", "genre_realitytv", "genre_romance", "genre_sport", "genre_western")
to_drop_maybe = c(to_drop_maybe, "genre_mystery")
#2
to_drop = c(to_drop, "main_actor2_is_female", "main_actor3_is_female")
to_drop_maybe = c(to_drop_maybe, "main_director_is_female")
#3
to_drop = c(to_drop, "total_number_of_directors", "total_number_of_producers", "total_number_of_production_companies", "total_number_of_production_countries")
#4
to_drop <- c(to_drop, "release_day", "release_year")
#6
studio = c("main_director_name", "main_producer_name", "editor_name", "main_production_company", "main_production_country")
to_drop <- c(to_drop, studio)
#7
to_drop = c(to_drop, "main_spoken_language")

#### 9. Clean dataset ####
#backup_to_drop <- to_drop
#to_drop <- backup_to_drop

#Adding additional features to remove that were not in the analysis
to_drop <- c(to_drop, "film_title", "main_actor1_name", "main_actor1_known_for", 
             "main_actor2_name", "main_actor2_known_for", "main_actor2_is_female",  
             "main_actor3_name", "main_actor3_known_for", "main_actor3_is_female",
             "main_producer_name", "main_director_name", "editor_name", "main_production_company", "main_production_country", "main_actor_know_for_wDate",
             "imdbRating_factor", "country_prod", "language_used")

df$duration_minutes = 30*trunc(df$duration_minutes/30) #Getting duration per slice of 30 minutes
df$budget = ifelse(is.na(df$budget), 0, df$budget) 
df$budget = 5000*trunc(df$budget/5000)

#dummies
df$country_prod_d = ifelse(df$country_prod == "US", 1, 0)
df$language_used_d = ifelse(df$language_used == "English", 1, 0)

#cleaning
df_clean <- df[, !colnames(df) %in% to_drop]
df_clean <- df_clean[, !colnames(df_clean) %in% to_drop_maybe]

str(df_clean)

write.csv(df_clean, 'C:/Users/paulc/Documents/MMA/Multivariate/midterm/df_clean.csv')






