library(scales)
arizona.pop<- read_clip()
arizona.pop<- do.call(rbind,strsplit(arizona.pop,'\t'))
arizona.pop<- gsub(',','',arizona.pop)
arizona.pop<- trimws(arizona.pop,which="right")
arizona.pop <- as.data.frame(arizona.pop)
colnames(arizona.pop) <- c("county","pop")
arizona.pop$county <- as.factor(arizona.pop$county)
arizona.pop$pop <- as.numeric(arizona.pop$pop)



covid_US_cases_tidy %>% filter(Province_State=="Arizona") %>%
  select(Admin2,Date,count) %>%
  group_by(Admin2) %>%
  summarise(m=max(count)) %>% arrange(desc(m))

covid_US_cases_tidy %>% filter(Province_State=="Arizona") %>% group_by(Admin2) %>%
  select(Admin2,Date,count)->zona
zona <- as.data.frame(zona)
zona$Date<- ymd(as.character(zona$Date ))
zona %>% left_join(x=zona,y=arizona.pop,by=c("Admin2"="county"))->zona

zona %>% filter(Date>today()-21) %>%
  group_by(Admin2) %>%
  arrange(-desc(Date)) %>%
  mutate(count_lag=lag(count,1),new_cases=count-count_lag) %>% group_by(Date) %>%
  arrange(desc(Date),desc(new_cases)) %>%
  mutate(nc.cap=new_cases/pop) %>%

ggplot(aes(x=Date,y=nc.cap))+ geom_line()+
  geom_smooth()+
  facet_wrap(~Admin2,scales='free')+
  ggtitle("New cases Proportional to county population in Arizona")




dad<- rbind(pinal,grant)
dad %>%
  ggplot(aes(x=Date,y=new_cases)) +geom_line()+
  facet_wrap(~Admin2)+geom_smooth()
