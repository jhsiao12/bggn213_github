#Investigating Pertussis Resurgence


#Libraries
library(datapasta)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(dplyr)


#Pertussis by Year - CDC Data
#Importing Data
cdc <- read_excel("CDCData.xlsx")
cdc <- data.frame(cdc)
cdc
#Visualizing
ggplot(cdc, aes(Year, Cases)) + 
  geom_point() + 
  theme_bw() + 
  #Adding vlines for wP and aP vaccination rollouts
  geom_vline(xintercept = c(1946, 1996), color = c("red", "blue"), linetype = "dotted") +
  geom_text(data = data.frame(x = c(1946, 1996), y = 100000, label = c("wP", "aP")), aes(x = x, y = y, label = label))


## Exploring CMI-PB Data
subject <- read_json("https://www.cmi-pb.org/api/subject", simplifyVector = TRUE) 
#Determining Number in Dataset
summary(subject)
#Determining aP vs wP numbers in subject object
table(subject$infancy_vac)
#By sex
table(subject$biological_sex)
#By race and biological sex
table(subject$biological_sex, subject$race)
#Average age of wP and aP individuals
subject$age <- time_length(today() - ymd(subject$year_of_birth), "year")
subject$age #overall ages

ap <- subject %>% filter(infancy_vac == "aP")
round(summary(ap$age)) #ages of aP vaccinated

wp <- subject %>% filter(infancy_vac == "wP")
round(summary(wp$age)) #ages of wP vaccinated

#Age of all individuals at time of boost
subject$age_at_boost <- time_length(ymd(subject$date_of_boost) - ymd(subject$year_of_birth), "year")
subject$age_at_boost
#Plotting aP vs wP ages at time of boost
ggplot(subject) +
  aes(time_length(age),
  fill = as.factor(infancy_vac)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(vars(infancy_vac), nrow = 2) 


#Merging subject and specimen data 
specimen <- read_json("https://www.cmi-pb.org/api/specimen", simplifyVector = TRUE) #importing specimen data
titer <- read_json("https://www.cmi-pb.org/api/plasma_ab_titer", simplifyVector = TRUE) #importing titer data





