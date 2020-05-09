covid_cases_global <- read.csv(here::here("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/","time_series_covid19_confirmed_global.csv"),stringsAsFactors = F)

cg<- covid_cases_global

head(cg)

# tidy up the data for graphing
recent_date <- colnames(cg)[length(cg)]
head(cg)

str(cg)

countryID_dates<- cg %>% select(Country.Region, Lat, Long,X1.22.20:recent_date) %>% gather(key = Date, value= count,-c(Country.Region,Lat,Long))

countryID_info<- cg %>% select(Country.Region,-c(X1.22.20:recent_date))

cg_tidy<- left_join(countryID_info,countryID_dates,by="Country.Region")

cg_tidy<-cg_tidy %>% mutate(Date=gsub("X","",Date))
str(cg_tidy)

#Use lapply() to coerce and replace the chosen columns:
cg_tidy$Country.Region <-  factor(cg_tidy$Country.Region)
cg_tidy$Date<- mdy(cg_tidy$Date)
str(cg_tidy)
#write.csv(cg_tidy,file='./data/covid_global_cases_tidy.csv')


cg<- read.csv(here::here('data',file='./covid_global_cases_tidy.csv'),stringsAsFactors = F)
cg$Country.Region <- as.factor(cg$Country.Region)
cg$Date <- ymd(cg$Date)

countries.of.interest <- c('Sweden', 'Korea, South','US')
sk.pop <- 51e6
US.pop <- 330e6
sw.pop <- 10e6

cg %>%
  group_by(Country.Region) %>%
  filter(Country.Region %in% countries.of.interest) %>%
  mutate(proportion=case_when(Country.Region=='Sweden'~ count/sw.pop,
                              Country.Region=='Korea, South'~ count/sk.pop,
                              Country.Region=='US'~ count/US.pop)) %>%
  ggplot(aes(x=Date,proportion))+geom_point()+facet_wrap(~Country.Region)

