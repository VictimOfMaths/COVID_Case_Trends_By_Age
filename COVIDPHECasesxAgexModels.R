rm(list=ls())

library(tidyverse)
library(scales)
library(paletteer)
library(broom)
library(lubridate)
library(ggtext)

#Controls:
area <- "England"
FitFrom <- as.Date("2021-01-08")
FitTo <- as.Date("2021-01-14")

#Read in case data by age processed by:
#https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/COVIDPHECasesxAgev2.R
data.cases <- read.csv("COVID_Cases_By_Age/data.csv") %>% 
  select(-c(1)) %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date>as.Date("2020-12-01"))

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

tiff("Outputs/COVIDCasesxAgeDots.tiff", units="in", width=10, height=7, res=500)
data.cases %>% 
  filter(areaName==area & date>as.Date("2021-01-04")) %>% 
  ggplot()+
  geom_point(aes(x=date, y=casesroll, colour=age), show.legend = FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="Daily new cases (log scale)", 
                     labels=number_format(accuracy=1))+
  scale_colour_paletteer_d("pals::stepped")+
  facet_wrap(~age, ncol=4, scales="free_y")+
  theme_classic()+
  theme(strip.background=element_blank(),
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title=paste0("Age-specific trends in COVID-19 case rates in ", area),
       subtitle="Rolling 7-day average of new COVID-19 cases identified by age group, plotted on a log scale",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()


#Fit linear models
models <- data.cases %>% 
  filter(areaName==area & date>=FitFrom & date<=FitTo) %>% 
  mutate(daysfrom=as.numeric(difftime(date, FitFrom, units = "days"))) %>% 
  group_by(age) %>% 
  do(tidy(lm(log(casesroll+0.5)~daysfrom, .))) %>% 
  select(1:3) %>% 
  spread(term, estimate) %>% 
  rename(intercept=`(Intercept)`, slope=daysfrom)

#Merge into case data
plot.data <- data.cases %>%
  filter(date>=FitFrom & areaName==area) %>% 
  merge(models) %>% 
  mutate(daysfrom=as.numeric(difftime(date, FitFrom, units = "days")),
         baseline=exp(intercept+slope*daysfrom))

#Calculate cumulative difference
plot.labels <- plot.data %>% 
  filter(date>FitTo & areaName==area & age!="Total") %>% 
  group_by(age) %>% 
  summarise(cumdiff=sum(casesroll-baseline, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=as.Date("2021-01-20"))

#Bring in x values for labels
plot.labels <- plot.data %>% 
  filter(date==as.Date("2021-01-20") & age!="Total") %>% 
  select(age, casesroll) %>% 
  mutate(labpos=casesroll*2) %>% 
  merge(plot.labels)

tiff("Outputs/COVIDCasesxAgeDifference.tiff", units="in", width=10, height=7, res=500)
plot.data %>%
  filter(age!="Total") %>% 
  ggplot()+
  geom_line(aes(x=date, y=baseline, colour=age), show.legend=FALSE)+
  geom_ribbon(aes(x=date, ymin=casesroll, ymax=baseline, fill=age), alpha=0.3, show.legend=FALSE)+
  geom_point(aes(x=date, y=casesroll, colour=age), show.legend = FALSE)+
  geom_text(data=plot.labels, aes(x=date, y=labpos, label=paste0(round(cumdiff, 0), " cases vs. baseline")), 
            size=rel(2.5))+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="Daily new cases (log scale)", 
                     labels=number_format(accuracy=1))+
  scale_colour_paletteer_d("pals::stepped")+
  scale_fill_paletteer_d("pals::stepped")+
  facet_wrap(~age)+
  theme_classic()+
  theme(strip.background=element_blank(),
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title=paste0("Age-specific trends in COVID-19 case rates in ", area),
       subtitle=paste0("Rolling 7-day average of new COVID-19 cases identified by age group, plotted on a log scale\nBaseline estimated on data from ", FitFrom, " to ", FitTo),
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

