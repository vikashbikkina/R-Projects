#EDA on Fifa3 data set 
# Analysis on the Club level information 

library(tidyverse)
library(forcats)
#install.packages("feather")
library(feather)


#import the data set

fifa3 <- read.csv(file = "Fifa3.csv", stringsAsFactors = FALSE)


#Covert to tibble

fifa3 <- as_tibble(fifa3)
# Select Data - Basic information
basic <- fifa3 %>% select( "name","club","age", "league", "height_cm", "weight_kg","eur_value", "eur_wage", "eur_release_clause", "overall", "potential", "international_reputation", "preferred_foot", "skill_moves", "weak_foot", "strength", "stamina")

# Factor the cat variables as factors

basic$league <- as.factor(basic$league)
basic$club <- as.factor(basic$club)

#identify clubs with players of skill above 85 and name it a starclub 

basic %>% filter(overall > 85) %>% group_by(club) %>%summarise(count = n()) %>% filter(count > 1) %>% droplevels() -> star

starclubs <- levels(star$club)

#Prepare data fo Viz on the club level

club_data <- basic %>% filter(club %in% clubs)%>%group_by(league,club) %>% summarise(count = n(), mean_overall = round(mean(overall)), mean_potential = round(mean(potential)), mean_age = round(mean(age)), sum_wage = sum(eur_wage), sum_value = sum(eur_value))

club_data %>% arrange(desc(mean_overall)) %>% head(10)
