library(tidyverse)
library(lubridate)
library(here)
covid_us <- read.csv(file='./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv',stringsAsFactors = F)
#write.csv(covid_us,file='./data/covid_US_deaths_raw.csv')

recent_date <- colnames(covid_us)[length(covid_us)]
head(covid_us)

str(covid_us)

unique(covid_us$Province_State)

UID_dates<- covid_us %>% select(UID,X1.22.20:recent_date) %>% gather(key = Date, value= count,-UID)

UID_info<- covid_us %>% select(-c(X1.22.20:recent_date))

 covid_tidy<- left_join(UID_info,UID_dates,by="UID")

covid_tidy<-covid_tidy %>% mutate(Date=gsub("X","",Date))
str(covid_tidy)

cols<- names(covid_tidy[c(1:8,11)])

#Use lapply() to coerce and replace the chosen columns:
covid_tidy[cols] <- lapply(covid_tidy[cols], factor)  ## as.factor() could also be used
covid_tidy$Date<- ymd(covid_tidy$Date)
str(covid_tidy)

covid_tidy %>% select(d)