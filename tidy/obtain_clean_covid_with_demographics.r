# This is a file under construction.  It does some cleaning but do --------

df <- read.csv("./data/world_COVID_update_ 03_13_2020_with_demogrpahics.csv",skip=1,
               stringsAsFactors = F,header = T)

str(df)
df %>% filter(country!="China") %>% mutate(symptom_onset=mdy(symptom_onset),death=mdy(death)) %>%
  select(symptom_onset,death) %>% mutate(dif=death-symptom_onset) %>% na.omit() %>% summarise(m.d=mean(dif))

#So, the average day of reported cases from symptom onset to deah is 18 days.


df<- df %>% #filter(country =="China") %>%
  mutate(death=case_when(death== '1' ~ reporting.date))# %>% filter(death!='0') %>%
  group_by(country) %>%
  summarise(n())

  cols_date <-  names(df[,c(3,9,11,14:15,19,20)])
  df[,cols_date]<- df[,cols_date] %>% mutate_all(mdy)
str(df)

df %>% select(location, country,gender, age, symptom_onset,death,recovered) %>%
group_by(death) %>% filter(location =="Washington") %>% summarise(n())
