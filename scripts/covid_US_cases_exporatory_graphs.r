
# read US Cases into Rstudio ----------------------------------------------

covid_cases_us <- read.csv(here::here("data","covid_US_cases_raw.csv"),stringsAsFactors = F)

# tidy up the data for graphing
recent_date <- colnames(covid_cases_us)[length(covid_cases_us)]
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





# list of counties in Washington

covid_US_cases_tidy %>% filter(Province_State=="Washington") %>% group_by(Admin2) %>%
  summarise(m=max(count)) %>% arrange(desc(m))

# total cumulative cases in Washington state, King and Spkane county

covid_US_cases_tidy %>% filter(Province_State=="Washington",Admin2=="King"|Admin2=="Spokane",count>0) %>% group_by(Admin2) %>%
  arrange(-desc(Date)) %>% select(-c(iso2:FIPS,Country_Region)) %>% group_by(Date) %>%
  ggplot(aes(Date,count,color=Admin2))+geom_point(shape=18)+
  geom_smooth(se=F)


covid_US_cases_tidy %>%filter(Province_State=='Washington',count>50) %>%
  group_by(Admin2) %>%
  arrange(-desc(Date)) %>% select(-c(iso2:FIPS,Country_Region)) %>%
  mutate(count_lag=lag(count,1),new_cases=count-count_lag) %>% group_by(Date) %>%
  ggplot(aes(Date,new_cases))+
  geom_point(shape=18)+geom_line(alpha=.2)+geom_smooth(se=F)+
  facet_wrap(~Admin2)

