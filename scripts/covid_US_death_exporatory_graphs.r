covid_us <- read.csv(here::here("data","covid_US_deaths_raw.csv"), stringsAsFactors = F)

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
covid_tidy$Date<- mdy(covid_tidy$Date)
str(covid_tidy)




# list of counties in Washington

covid_tidy %>% filter(Province_State=="Washington") %>% group_by(Admin2) %>%
  summarise(m=max(count)) %>% arrange(desc(m))

# cummulative deaths in King and Spokane Counties

covid_tidy %>% filter(Province_State=="Washington",Admin2=="King"|Admin2=="Spokane",count>0) %>% group_by(Admin2) %>%
  arrange(-desc(Date)) %>% select(-c(iso2:FIPS,Country_Region)) %>% group_by(Date) %>%
  ggplot(aes(Date,count,color=Admin2))+geom_point(shape=18)+
  geom_smooth(se=F)

# Daily reports of fatalities in King and Spokane

covid_tidy %>% filter(Province_State=="Washington",Admin2=="King"|Admin2=="Spokane"|Admin2=="Snohomish",count>0) %>% group_by(Admin2) %>%
  arrange(-desc(Date)) %>% select(-c(iso2:FIPS,Country_Region)) %>%
  mutate(count_lag=lag(count,1),new_deaths=count-count_lag) %>% group_by(Date) %>%
  ggplot(aes(Date,new_deaths,color=Admin2))+geom_point(shape=18)+
  geom_line(stat='smooth',alpha=.5)+geom_line(stat='smooth',alpha=.5)+
  ggtitle("Daily reporting of Fatalities")

drop_state <- c("New York","Massachusetts")

covid_tidy %>%
  select(-c(iso2:FIPS,Admin2,Country_Region)) %>%
  group_by(Province_State,Date) %>%
  summarise(count=sum(count,na.rm=T)) %>%
  filter(Date>"2020-04-20") %>% #,!Province_State %in% drop_state) %>%
  mutate(new_deaths= count-lag(count)) %>%
  group_by(Date,Province_State) %>%
  ggplot(aes(x=Date,new_deaths))+ geom_point(size=.2)+
  geom_line()+
  facet_wrap(~Province_State,scales = 'free')+
  ggtitle("new cases in the States since 4/21")

