library(tidyverse)
library(scales)
library(paletteer)
library(broom)
library(lubridate)
library(ggtext)
library(extrafont)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
#Controls:
area <- input$area
FitFrom <- input$FitRange[1]
FitTo <- input$FitRange[2]
labpos <- FitFrom+days(floor(as.double(difftime(max(data.cases$date), FitFrom, units="days"))/5))

#Collapse age groups if necessary
if(input$plot=="Age-specific (broad)"){
  data.cases <- data.cases %>% 
    mutate(age=case_when(
      age %in% c("0-4", "5-9") ~ "0-9",
      age %in% c("25-29", "30-34", "35-39") ~ "25-40",
      age %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
      age %in% c("60-64", "65-69") ~ "60-69",
      age %in% c("70-74", "75-79") ~ "70-79",
      age %in% c("80-84", "85-89", "90+") ~ "80+",
      TRUE ~ as.character(age))) %>% 
    group_by(age, date, areaName) %>% 
    summarise(casesroll=sum(casesroll)) %>% 
    ungroup()
}

#Fit linear models
models <- data.cases %>% 
  filter(areaName==area & date>=FitFrom & date<=FitTo) %>% 
  mutate(daysfrom=as.numeric(difftime(date, FitFrom, units = "days"))) %>% 
  group_by(age) %>% 
  do(tidy(lm(log(casesroll+0.5)~daysfrom, .))) %>% 
  ungroup() %>% 
  select(1:3) %>% 
  spread(term, estimate) %>% 
  rename(intercept=`(Intercept)`, slope=daysfrom)

#Merge into case data
plot.data <- data.cases %>%
  filter(date>=FitFrom & areaName==area) %>% 
  merge(models) %>% 
  mutate(daysfrom=as.numeric(difftime(date, FitFrom, units = "days")),
         baseline=exp(intercept+slope*daysfrom),
         age=factor(age, levels=c("0-4", "0-9", "5-9", "10-14", "15-19",
                                  "20-24", "25-29", "25-40", "30-34", "35-39", 
                                  "40-44", "40-59", "45-49", "50-54", "55-59", 
                                  "60-64", "60-69",  "65-69", "70-74","70-79",  "75-79", 
                                  "80-84", "80+", "85-89", "90+", "Total")))

#Calculate cumulative difference
plot.labels <- plot.data %>% 
  filter(date>FitTo) %>% 
  group_by(age) %>% 
  summarise(cumdiff=sum(casesroll-baseline, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=labpos)

#Bring in x values for labels
plot.labels <- plot.data %>% 
  filter(date==labpos) %>% 
  select(age, casesroll, baseline) %>% 
  rowwise() %>% 
  mutate(labpos=case_when(
    input$plot=="Overall" ~ max(casesroll, baseline)*1.2,
    input$scale=="Yes" ~ max(casesroll, baseline)*1.5,
    age %in% c("0-4", "0-9") ~ max(casesroll, baseline)*1.01, 
    TRUE ~ max(casesroll, baseline)*1.2)) %>% 
  ungroup() %>% 
  merge(plot.labels)

scale <- if_else(input$scale=="Yes", "fixed", "free_y")

if (input$plot == "Overall"){
  plot.labels <- plot.labels %>% 
    filter(age=="Total") %>% 
    mutate(labtext=case_when(
      cumdiff>0 ~ paste0("+", round(cumdiff, 0), " cases vs. baseline\nsince ", format(FitTo, "%d-%b")),
      TRUE ~ paste0(round(cumdiff, 0), " cases vs. baseline\nsince ", format(FitTo, "%d-%b"))))
  
p <- plot.data %>%
  filter(age=="Total") %>% 
  ggplot()+
  geom_line(aes(x=date, y=baseline), show.legend=FALSE)+
  geom_ribbon(aes(x=date, ymin=casesroll, ymax=baseline), alpha=0.3, show.legend=FALSE)+
  geom_point(aes(x=date, y=casesroll), show.legend = FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="Daily new cases (log scale)", 
                     labels=number_format(accuracy=1))+
  theme_classic(base_size=14)+
  theme(strip.background=element_blank(),
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold", size=rel(1.8)),
        text=element_text(family="Lato"))+
  labs(title=paste0("Recent trends in COVID-19 case numbers in ", area),
       subtitle=paste0("Rolling 7-day average of new COVID-19 cases identified, plotted on a log scale\nBaseline calculated assuming a constant relative reduction in cases, based on data from ", FitFrom, " to ", FitTo, ".\nThe shaded area represents the difference between this baseline and the observed number of cases. "),
       caption="Data from PHE | Plot by @VictimOfMaths")

if(input$labels=="Yes"){
  p <- p+geom_text(data=plot.labels, aes(x=date, y=labpos, 
                                         label=labtext), 
                   size=rel(4))
}
}

if (input$plot == "Age-specific (detailed)"){
  plot.labels <- plot.labels %>% 
    filter(age!="Total") %>% 
    mutate(labtext=case_when(
      age=="0-4" & cumdiff>0 ~ paste0("+", round(cumdiff, 0), " cases vs. baseline\nsince ", format(FitTo, "%d-%b")),
      age=="0-4" ~ paste0(round(cumdiff, 0), " cases vs. baseline\nsince ", format(FitTo, "%d-%b")),
      cumdiff>0 ~ paste0("+", round(cumdiff, 0)),
      TRUE ~ as.character(round(cumdiff, 0))))
  
  
  p <- plot.data %>%
    filter(age!="Total") %>% 
    ggplot()+
    geom_line(aes(x=date, y=baseline, colour=age), show.legend=FALSE)+
    geom_ribbon(aes(x=date, ymin=casesroll, ymax=baseline, fill=age), alpha=0.3, show.legend=FALSE)+
    geom_point(aes(x=date, y=casesroll, colour=age), show.legend = FALSE)+
    scale_x_date(name="")+
    scale_y_continuous(trans="log", name="Daily new cases (log scale)", 
                       labels=number_format(accuracy=1))+
    scale_colour_paletteer_d("pals::stepped")+
    scale_fill_paletteer_d("pals::stepped")+
    facet_wrap(~age, scales=scale, ncol=4)+
    theme_classic(base_size=14)+
    theme(strip.background=element_blank(),
          strip.text=element_text(face="bold"),
          plot.title=element_text(face="bold", size=rel(1.8)),
          text=element_text(family="Lato"))+
    labs(title=paste0("Age-specific trends in COVID-19 case numbers in ", area),
         subtitle=paste0("Rolling 7-day average of new COVID-19 cases identified, plotted on a log scale\nBaseline calculated assuming a constant relative reduction in cases, based on data from ", FitFrom, " to ", FitTo, ".\nThe shaded area represents the difference between this baseline and the observed number of cases. "),
         caption="Data from PHE | Plot by @VictimOfMaths")
  
  if(input$labels=="Yes"){
    p <- p+geom_text(data=plot.labels, aes(x=date, y=labpos, 
                                           label=labtext), 
                     size=rel(3.5))
  }
}

if (input$plot == "Age-specific (broad)"){
  plot.labels <- plot.labels %>% 
    filter(age!="Total") %>% 
    mutate(labtext=case_when(
      age=="0-9" & cumdiff>0 ~ paste0("+", round(cumdiff, 0), " cases vs. baseline\nsince ", format(FitTo, "%d-%b")),
      age=="0-9" ~ paste0(round(cumdiff, 0), " cases vs. baseline\nsince ", format(FitTo, "%d-%b")),
      cumdiff>0 ~ paste0("+", round(cumdiff, 0)),
      TRUE ~ as.character(round(cumdiff, 0))))
  
  p <- plot.data %>%
    filter(age!="Total") %>% 
    ggplot()+
    geom_line(aes(x=date, y=baseline, colour=age), show.legend=FALSE)+
    geom_ribbon(aes(x=date, ymin=casesroll, ymax=baseline, fill=age), alpha=0.3, show.legend=FALSE)+
    geom_point(aes(x=date, y=casesroll, colour=age), show.legend = FALSE)+
    scale_x_date(name="")+
    scale_y_continuous(trans="log", name="Daily new cases (log scale)", 
                       labels=number_format(accuracy=1))+
    scale_colour_paletteer_d("rcartocolor::Prism")+
    scale_fill_paletteer_d("rcartocolor::Prism")+
    facet_wrap(~age, scales=scale)+
    theme_classic(base_size=14)+
    theme(strip.background=element_blank(),
          strip.text=element_text(face="bold"),
          plot.title=element_text(face="bold", size=rel(1.8)),
          text=element_text(family="Lato"))+
    labs(title=paste0("Age-specific trends in COVID-19 case numbers in ", area),
         subtitle=paste0("Rolling 7-day average of new COVID-19 cases identified, plotted on a log scale\nBaseline calculated assuming a constant relative reduction in cases, based on data from ", FitFrom, " to ", FitTo, ".\nThe shaded area represents the difference between this baseline and the observed number of cases. "),
         caption="Data from PHE | Plot by @VictimOfMaths")
  
  if(input$labels=="Yes"){
    p <- p+geom_text(data=plot.labels, aes(x=date, y=labpos, 
                                           label=labtext), 
                     size=rel(3.5))
  }
}


p
  }, height=700, width=1000)
  
}
