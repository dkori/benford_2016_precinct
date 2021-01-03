library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
# read in election data

## first assign link to rdata file file
rdata_url<-"https://dataverse.harvard.edu/api/access/datafile/3345331?format=RData&gbrecs=true"
# create tempfile
temp <- tempfile()
#download file
download.file(rdata_url,temp)
# load data
load(temp)

names(x)

dat<-x%>%
  
  # limit to only trump and clinton
  filter(candidate%in%c("Hillary Clinton","Donald Trump"))%>% 
  # create unique identifier
  mutate(unique_id  = paste0(state,county_fips,precinct))%>%
  select(state,county_name,precinct,candidate,votes)%>%
  # sum to get rid of duplicates (such as two rows for trump in one precinct)
  group_by(state,county_name,precinct,candidate)%>%
  summarise(votes = sum(votes))%>%
  ungroup()%>%
  # spread the data
  spread(key = candidate,value = votes)%>%
  # mark winning candidate
  mutate(winner = case_when(`Hillary Clinton`>`Donald Trump` ~ "Clinton",
                            `Donald Trump`>`Hillary Clinton` ~ "Trump",
                            TRUE ~ "Tie"))%>%
  rowwise()%>%
  # extract the first digit
  mutate(first_digit_clinton = substr(paste0(`Hillary Clinton`),1,1),
         first_digit_trump = substr(paste0(`Donald Trump`),1,1))

# run benfords law 1st digit chart for allegheny
allegheny<-dat%>%
  filter(state=="Pennsylvania",
         county_name=="Allegheny County")%>%
  filter(first_digit_clinton %in% as.character(1:9))%>%
  # make first digit a factor
  mutate(first_digit_clinton = as.numeric(first_digit_clinton))%>%
  ggplot(aes(first_digit_clinton))+
  geom_bar(stat = "count")+
  labs(title = "Allegheny County, PA 2016 1st Digit Precinct Level",
       x = "Leading Digit")+
  scale_x_continuous(breaks = 1:9)

# run benford's law 1st digit for cook county, pa
cook<-dat%>%
  filter(state=="Illinois",
         county_name=="Cook County")%>%
  filter(first_digit_clinton %in% as.character(1:9))%>%
  # make first digit a factor
  mutate(first_digit_clinton = as.numeric(first_digit_clinton))%>%
  ggplot(aes(first_digit_clinton))+
  geom_bar(stat = "count")+
  labs(title = "Cook County, IL 2016 1st Digit Precinct Level",
       x = "Leading Digit")+
  scale_x_continuous(breaks = 1:9)

# run benford's law 1st digit for cook county, pa
milwaukee<-dat%>%
  filter(state=="Wisconsin",
         county_name=="Milwaukee County")%>%
  filter(first_digit_clinton %in% as.character(1:9))%>%
  # make first digit a factor
  mutate(first_digit_clinton = as.numeric(first_digit_clinton))%>%
  ggplot(aes(first_digit_clinton))+
  geom_bar(stat = "count")+
  labs(title = "Milwaukee County, WI 2016 1st Digit Precinct Level",
       x = "Leading Digit")+
  scale_x_continuous(breaks = 1:9)

ggsave("allegheny benford 2016.png", allegheny)
ggsave("cook benford 2016.png",cook)
ggsave("milwauke benford 2016.png",milwaukee)
