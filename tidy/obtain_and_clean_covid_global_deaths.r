library(tidyverse)
library(lubridate)
library(here)
covid_glob_death <- read.csv(file='./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',stringsAsFactors = F)
#write.csv(covid_glob_death,file='./data/covid_global_deaths_raw.csv')

recent_date <- colnames(covid_glob_death)[length(covid_glob_death)]
head(covid_glob_death)

str(covid_glob_death)

countryID_dates<- covid_glob_death %>% select(Country.Region, Lat, Long,X1.22.20:recent_date) %>% gather(key = Date, value= count,-c(Country.Region,Lat,Long))

countryID_info<- covid_glob_death %>% select(Country.Region,-c(X1.22.20:recent_date))

covid_glob_death_tidy<- left_join(countryID_info,countryID_dates,by="Country.Region")

covid_glob_death_tidy<-covid_glob_death_tidy %>% mutate(Date=gsub("X","",Date))
str(covid_glob_death_tidy)

#Use lapply() to coerce and replace the chosen columns:
covid_glob_death_tidy$Country.Region <-  factor(covid_glob_death_tidy$Country.Region)
covid_glob_death_tidy$Date<- mdy(covid_glob_death_tidy$Date)
str(covid_glob_death_tidy)
#write.csv(covid_glob_death_tidy,file='./data/covid_global_deaths_tidy.csv')

cgd<- read.csv(here::here('data',file='./covid_global_deaths_tidy.csv'),stringsAsFactors = F)
cgd$Country.Region <- as.factor(cgd$Country.Region)
cgd$Date <- ymd(cgd$Date)

countries.of.interest <- c('Sweden', 'Korea, South','US')
sk.pop <- 51e6
US.pop <- 330e6
sw.pop <- 10e6

# Deaths per cap of south kor, us, sweden
cgd %>%
  group_by(Country.Region) %>%
  filter(Country.Region %in% countries.of.interest) %>%
  mutate(proportion=case_when(Country.Region=='Sweden'~ count/sw.pop,
                              Country.Region=='Korea, South'~ count/sk.pop,
                              Country.Region=='US'~ count/US.pop)) %>%
  ggplot(aes(x=Date,proportion))+geom_point()+facet_wrap(~Country.Region)




cgd %>%
  #group_by(Country.Region) %>%
  summarise(sum(count))


