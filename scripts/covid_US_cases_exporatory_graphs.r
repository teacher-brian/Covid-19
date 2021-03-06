library(tidyverse)
library(lubridate)
library(forcats)
library(viridis)
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



#covid_US_cases_tidy<- read.csv(file='./data/covid_US_cases_tidy.csv')

# list of counties in Washington

covid_US_cases_tidy %>% filter(Province_State=="Washington") %>% group_by(Admin2) %>%
  summarise(m=max(count)) %>% arrange(desc(m))

# total cumulative cases in Washington state, King and Spkane county

covid_US_cases_tidy %>% filter(Province_State=="Washington"#,
                               #Admin2=="King"|
                                # Admin2=="Spokane"|
                                 #Admin2=="Snohomish",count>01
                               ) %>%
  group_by(Admin2) %>%
  arrange(desc(Date)) %>% select(-c(iso2:FIPS,Country_Region)) %>% group_by(Date)%>%
  ggplot(aes(Date,count,color=Admin2))+geom_point(shape=18)+
  geom_line(stat='smooth',se=F,alpha=.6)+
  facet_wrap(~Admin2,scales='free')

new.cases <- c("Adams",'Benton','Grant', 'Clark', 'Franklin','Skagit','Spokane','Thurston','Walla Walla','Yakima',"King")

covid_US_cases_tidy %>% filter(#Date>today()-21,
                               Province_State=='Washington') %>%
  #filter(Admin2 == "King" |Admin2 %in% new.cases) %>%
  group_by(Admin2) %>%
  arrange(-desc(Date)) %>% select(-c(iso2:FIPS,Country_Region)) %>%
  mutate(count_lag=lag(count,1),new_cases=count-count_lag) %>% group_by(Date) %>%
  arrange(desc(Date),desc(new_cases)) %>%
  mutate(sort.Admin2=factor(Admin2))-> wa.new.cases#


# The following compares snohomish and king on average weekly new  --------

wa.new.cases %>% filter(Admin2=="King") %>% select(Admin2,count,count_lag,new_cases) %>%
  mutate(week=case_when(Date<="2020-12-31" ~ week(Date),
                        Date<='2021-12-31' ~ 53 + week(Date))) %>%
  #print(.,n=500) %>%
  group_by(week,Admin2) %>%
  summarise(mean_new=mean(new_cases,na.rm=T),sum_new=sum(new_cases,na.rm=T)) %>%
  mutate(d=ymd("2020-01-01") + weeks(week))->
  weekly_king #because "weeks(week)" probably cycles over, the weekly average stops in jan 2021

wa.new.cases %>% filter(Admin2=="King",
                        #Date<=ymd("2020-10-15")
                        ) %>%
  # consdier changing date filter above
  select(Admin2,count,count_lag,new_cases,Date) %>%
  mutate(week=week(Date)) %>%
  group_by(week,Admin2) %>%
  ggplot(aes(x=Date,y=new_cases))+
  geom_point(color='magenta',size=.1) +
  # consdier changing date filter below
    geom_point(data=weekly_king# %>% filter(d<=ymd(today()))
               ,
               aes(x=d,y=mean_new),color='royalblue3',shape=18,size=1.5)+
  geom_smooth(data=weekly_king,aes(x=d,y=mean_new),se = F, color='blue',          size=.1)+
  ggtitle("King County, WA\npink =  daily cases\n blue =  weekly average")+
  geom_vline(xintercept=ymd("2020-11-03"))+
  geom_label(label="election 11/3 --->",
             x= ymd("2020-10-10"),
             y = 750
  )+
  geom_vline(xintercept=ymd("2020-02-29"))+
  geom_label(label="<-- first stay at home order",
             x= ymd("2020-04-01"),
             y = 750
  )



  wa.new.cases[,c(3,12)] <- lapply(wa.new.cases[,c(3,12)],function(x) factor(x))
lapply(wa.new.cases[,c('Admin2','sort.Admin2')], levels)
wa.new.cases[,12] <- factor(wa.new.cases$Admin2,levels = wa.new.cases$Admin2[1:41], ordered = T)

wa.new.cases %>%
  ggplot(aes(Date,new_cases))+
  geom_point(shape=18)+geom_line(alpha=.2)+geom_smooth(se=F)+
  facet_wrap(~sort.Admin2),#scales = 'free')

# all us state and then some
drop_ship <- c("Diamond Princess","Grand Princess")
drop_state <- c("New Jersey","New York","Massachusetts","California",)
protest_states <- c("Minnesota","Texas","Colorado","Michigan","Illinois",
                    "Wisconsin","Ohio","Mississippi","North Carolina","Alabama","Arizona")

covid_US_cases_tidy %>%
  select(-c(iso2:FIPS,
            Admin2,
            Country_Region)) %>%
  group_by(Province_State,
           #Admin2,
           Date) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  filter(Date > "2020-04-20", !Province_State %in% drop_ship) %>%
  #filter(!Province_State %in% drop_state) %>%
  #filter(Province_State %in% protest_states) %>%
  mutate(new_cases = count - lag(count)) %>%
  group_by(Date, Province_State) %>% arrange(desc(Date), desc(new_cases)) %>%
  mutate(sort_state = factor(Province_State))  -> tmp

  tmp[,c(1,5)] <- lapply(tmp[,c(1,5)],function(x) factor(x))
  lapply(tmp[,c('Province_State','sort_state')], levels)
  tmp[,5] <- factor(tmp$Province_State,levels = tmp$Province_State[1:56], ordered = T)

    tmp %>%
    ggplot(aes(x=Date,new_cases))+ geom_point(size=.2)+
  geom_line()+
  facet_wrap(~sort_state,scales = 'fixed')+
  ggtitle("new cases in the States since 4/21\nBlue simple linear regression, pink with , loess")+
  geom_smooth(method='lm',color='blue',se=F)+
geom_smooth(color='pink',size=.6)

