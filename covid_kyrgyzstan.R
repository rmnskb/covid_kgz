library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)

getwd()
setwd(choose.dir())
Sys.setlocale(category = "LC_ALL", locale = "english")

#getting the data itself
covid <- read_xlsx("owid-covid-data.xlsx")
#extracting only data about Kyrgyzstan
covid_kgz <- covid[covid$iso_code == "KGZ", ]
#back up of our original data frame
coronavirus <- covid_kgz

#dropping columns that don't contain any info
covid_kgz <- covid_kgz[colSums(!is.na(covid_kgz)) > 0]
#dropping irrelevant columns
columns_to_drop <- c("iso_code", "continent", "location", "extreme_poverty",
                     "female_smokers", "male_smokers")
covid_kgz <- covid_kgz[, !(names(covid_kgz) %in% columns_to_drop)]
covid_kgz <- covid_kgz[, -c(39:42)]

#changing data type of the column from char to date
covid_kgz$date <- as.Date(covid_kgz$date)

#plotting the total cases data
ggplot(data = covid_kgz, mapping = aes(date, total_cases)) + geom_col() +
  labs(x = "Date", y = "Total cases",
               title = "Growth of total cases of COVID-19 in Kyrgyzstan")

#plotting the registered cases data 
ggplot(data = covid_kgz, mapping = aes(date, new_cases_smoothed)) + 
  geom_area(color = "darkred", fill = "firebrick4") +
  labs(x = "Date", y = "Registered cases",
       title = "New registered cases of COVID-19 in Kyrgyzstan") +
  geom_vline(xintercept = as.numeric(covid_kgz$date[396]), linetype = 4)

#plotting the total deaths data
ggplot(covid_kgz, aes(x = date, y = total_deaths)) +
  geom_line(color = "darkred") +
  labs(x = "Date", y = "Total deaths",
       title = "Registered death cases of COVID-19 in Kyrgyzstan")

#plotting the reproduction rate data
ggplot(covid_kgz, aes(date, reproduction_rate)) +
  geom_line(color = "darkblue", linetype = 1) +
  labs(x = "Date", y = "Reproduction rate",
       title = "Reproduction rate of COVID-19 in Kyrgyzstan") +
  geom_hline(yintercept = as.numeric(covid_kgz$reproduction_rate[591]), 
             linetype = 4)

#creating a new data frame for vaccine data and cleaning it from NA values
kgz_vaccine <- covid_kgz[, c(1, 15:25)]
kgz_vaccine <- drop_na(kgz_vaccine, 2)

#plotting the total vaccinations data
ggplot(kgz_vaccine, aes(date, total_vaccinations)) +
  geom_line() + 
  labs(x = "Month", y = "Total number of vaccinated", 
       title = "Total number of people vaccinated from COVID-19 in Kyrgyzstan",
       subtitle = "Both with one and two shots")

#creating new data frame for new vaccinations
#in order to drop NA values from the column
vccn <- kgz_vaccine[-c(1:10), c("date", "new_vaccinations")]
vccn <- drop_na(vccn, 2)

#plotting the new vaccinations data
ggplot(vccn, aes(date, new_vaccinations)) +
  geom_line(color = "darkolivegreen") +
  labs(x = "Month", y = "Number of vaccinations", 
       title = "Tempo of vaccination from COVID-19 in Kyrgyzstan")
