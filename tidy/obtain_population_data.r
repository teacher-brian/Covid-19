# county level state population data
base_url <-  'https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-'

# between 1 and 56 there are some 50 odd states.  Some numbers are skipped.
# the first one (above is alabama)

library(RCurl)
library(tidyverse)
page <- c(paste0(0,1:9),10:56)

df <- list(1:50)
for (i in 1:length(page)){
  url <- paste0(base_url,page[i],'.csv')
  if(url.exists(url=url)==T){data <- read.csv(file=url)}
  else{next}
data %>%
  select(2:7) %>%
  filter(YEAR==12)->data
print(data)
df[[i]] <- data
}
save(df, file = "US_county_population.RData")
load("US_county_population.RData")
df<- do.call(rbind,df)
# Missing: 3, 7, 14 43 52

# Within each file there are several columsn.  These are most of interest:
# "COUNTY" may be a candidate for a key

#   3       4        5      6     7
#   COUNTY  STNAME	CTYNAME	YEAR	POPESTIMATE
#
#   For i in each
#   1 check for legit url
#
#   Download file
#
#   Filter out rows by year == 12 # 12 is the most recent estimate
#
#   Scrape the state name the county name and the population estimate
#   delete downloaded file
#

# Compare State and County name to admin2
# make a relational database to join covid data to this relational database


covid_US_cases_tidy %>% filter(Country_Region=='US',Admin2!="") %>% select(Admin2) %>% with(Admin2,unique())
