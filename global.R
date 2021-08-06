library(tidyverse)
library(shiny)
library(Cairo)

options(shiny.useragg=TRUE)

load("Alldata.RData")

#Read in case data by age processed by:
#https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/COVIDPHECasesxAgev2.R
data.cases <- data1 %>% 
  select(-c(1)) %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date>=as.Date("2021-03-01") & date<=max(date[!is.na(casesroll)]))

#Add total
data.cases <- data.cases %>% 
  group_by(date, areaName) %>% 
  summarise(pop=sum(pop), cases=sum(cases), casesroll=sum(casesroll),
            areaType=unique(areaType)) %>% 
  mutate(caserate=cases*100000/pop, caserateroll=casesroll*10000/pop,
         age="Total",  age=factor(age, levels=c("0-4", "5-9", "10-14", "15-19",
                                                "20-24", "25-29", "30-34", "35-39", 
                                                "40-44", "45-49", "50-54", "55-59", 
                                                "60-64", "65-69", "70-74", "75-79", 
                                                "80-84", "85-89", "90+", "Total"))) %>% 
  ungroup() %>% 
  bind_rows(data.cases)

