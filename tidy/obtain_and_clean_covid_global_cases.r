library(tidyverse)
library(lubridate)

# function to strip X from date and conver to lubridate

drop_x <- function(x){
  y <- gsub("X","",x)
  y <- mdy(y)
  return(y)
  }



covid_cases_global <- read.csv(here::here("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/","time_series_covid19_confirmed_global.csv"),stringsAsFactors = F)

cg<- covid_cases_global

head(cg)

# tidy up the data for graphing
recent_date <- colnames(cg)[length(cg)]
head(cg)

str(cg)

countryID_dates<- cg %>%
#select(Country.Region, Province.State,Lat, Long,X1.22.20:recent_date) %>%
  gather(key = Date, value= count,-c(Country.Region,Lat,Long,Province.State))

#countryID_info<- cg %>% select(Country.Region,
                               #Province.State,
 #                              -c(X1.22.20:recent_date))

#cg_tidy<- left_join(countryID_info,countryID_dates,by="Country.Region")

## by including province state above, the joining actdion has been interfered with

cg_tidy<-countryID_dates %>% mutate(Date=gsub("X","",Date))
str(cg_tidy)

#Use lapply() to coerce and replace the chosen columns:
cg_tidy$Country.Region <-  factor(cg_tidy$Country.Region)
cg_tidy$Province.State <-  factor(cg_tidy$Province.State)
cg_tidy$Date<- mdy(cg_tidy$Date)
str(cg_tidy)
#write.csv(cg_tidy,file='./data/covid_global_cases_tidy.csv')


cg<- read.csv(here::here('data',file='./covid_global_cases_tidy.csv'),stringsAsFactors = F)
cg$Country.Region <- as.factor(cg$Country.Region)
cg$Province.State <-  factor(cg$Province.State)
cg$Date <- ymd(cg$Date)
str(cg)
# total as of today

today <- drop_x(recent_date)

cg %>%
  filter(Date==today) %>%
  #group_by(Date) %>%
  summarise(total=sum(count))

# comps

countries.of.interest <- c('Sweden', 'Korea, South','US',"United Kingdom","France")
sk.pop <- 51e6
US.pop <- 330e6
sw.pop <- 10e6
uk.pop <- 68e6
fr.pop <- 65e6

cg %>%
  group_by(Country.Region) %>%
  filter(Country.Region %in% countries.of.interest) %>%
  filter(Province.State=="") %>%
  mutate(proportion=case_when(Country.Region=='Sweden'~ count/sw.pop,
                              Country.Region=='Korea, South'~ count/sk.pop,
                              Country.Region=='US'~ count/US.pop,
                              Country.Region=='France'~ count/fr.pop,
                              Country.Region=='United Kingdom'~ count/uk.pop
                              )
         ) %>%
  ggplot(aes(x=Date,proportion))+geom_point(size=.1)+facet_wrap(~Country.Region)

# no proport
cg %>%
  group_by(Country.Region) %>%
  filter(Country.Region %in% countries.of.interest) %>%
  filter(Province.State=="") %>%
  # filter(Date>'2020-01-01',Date<'2020-02-05') %>%
    ggplot(aes(x=Date,count))+geom_point(size=.1)+facet_wrap(~Country.Region,scales='free')


cg %>%
  group_by(Date) %>%
  summarise(total=sum(count)) %>%
  ggplot(aes(x=Date,y=total,label=total))+geom_point(size=.5)+
  geom_text(aes(label=ifelse(Date==today,total,'')),hjust=1.5,vjust=.5)+
  ggtitle(paste0('total cases as of\n',today))+
    scale_y_continuous(labels = scales::comma)


cg %>%
  group_by(Country.Region,Date) %>% filter(Date == today) %>%
  filter(Province.State=="") %>%
  summarise(total=sum(count)) %>%
  arrange(desc(total)) %>%
   summarise(grand_tot=sum(total)) %>%
   print(.,n=400)

