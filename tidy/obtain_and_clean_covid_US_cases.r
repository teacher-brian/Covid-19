library(here)
library(tidyverse)


covid_cases_us <- read.csv(file='./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',stringsAsFactors = F)
recent_date <- colnames(covid_cases_us)[length(covid_cases_us)]
#recent_date1 <- colnames(covid_cases_us)[length(covid_cases_us)]
#if (rescent_date1<=recent_date)
#write.csv(covid_cases_us,file='./data/covid_US_cases_raw.csv')

head(covid_cases_us)

str(covid_cases_us)

UID_dates<- covid_cases_us %>% select(UID,X1.22.20:recent_date) %>% gather(key = Date, value= count,-UID)

UID_info<- covid_cases_us %>% select(-c(X1.22.20:recent_date))

covid_US_cases_tidy<- left_join(UID_info,UID_dates,by="UID")

covid_US_cases_tidy<-covid_US_cases_tidy %>% mutate(Date=gsub("X","",Date))
str(covid_US_cases_tidy)

cols<- names(covid_US_cases_tidy[c(1:8,11)])

#Use lapply() to coerce and replace the chosen columns:
covid_US_cases_tidy[cols] <- lapply(covid_US_cases_tidy[cols], factor)  ## as.factor() could also be used
covid_US_cases_tidy$Date<- mdy(covid_US_cases_tidy$Date)
str(covid_US_cases_tidy)
#write.csv(covid_US_cases_tidy,file='./data/covid_US_cases_tidy.csv')
