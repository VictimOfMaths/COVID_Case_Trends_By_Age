library(tidyverse)
library(scales)
library(paletteer)
library(broom)
library(lubridate)
library(ggtext)


server <- function(input, output) {
  
  output$plot <- renderPlot({
    
#Controls:
area <- input$area
FitFrom <- input$FitRange[1]
FitTo <- input$FitRange[2]

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
         baseline=exp(intercept+slope*daysfrom))

#Calculate cumulative difference
plot.labels <- plot.data %>% 
  filter(date>FitTo & areaName==area) %>% 
  group_by(age) %>% 
  summarise(cumdiff=sum(casesroll-baseline, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=as.Date("2021-01-20"))

#Bring in x values for labels
plot.labels <- plot.data %>% 
  filter(date==as.Date("2021-01-20")) %>% 
  select(age, casesroll) %>% 
  mutate(labpos=casesroll*2) %>% 
  merge(plot.labels)

scale <- if_else(input$scale=="Yes", "fixed", "free_y")

if (input$plot == "Overall"){
  plot.labels <- plot.labels %>% 
    filter(age=="Total") %>% 
    mutate(labpos=labpos*0.6)
  
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
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title=paste0("Recent trends in COVID-19 case rates in ", area),
       subtitle=paste0("Rolling 7-day average of new COVID-19 cases identified, plotted on a log scale\nBaseline estimated on data from ", FitFrom, " to ", FitTo),
       caption="Data from PHE | Plot by @VictimOfMaths")

if(input$labels=="Yes"){
  p <- p+geom_text(data=plot.labels, aes(x=date, y=labpos, 
                                         label=paste0(round(cumdiff, 0), " cases vs. baseline")), 
                   size=rel(4))
}
}

if (input$plot == "Age-specific"){
  plot.labels <- plot.labels %>% 
    filter(age!="Total")
  
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
    facet_wrap(~age, scales=scale)+
    theme_classic(base_size=14)+
    theme(strip.background=element_blank(),
          strip.text=element_text(face="bold"),
          plot.title=element_text(face="bold", size=rel(1.2)))+
    labs(title=paste0("Age-specific trends in COVID-19 case rates in ", area),
         subtitle=paste0("Rolling 7-day average of new COVID-19 cases identified by age group, plotted on a log scale\nBaseline estimated on data from ", FitFrom, " to ", FitTo),
         caption="Data from PHE | Plot by @VictimOfMaths")
  
  if(input$labels=="Yes"){
    p <- p+geom_text(data=plot.labels, aes(x=date, y=labpos, 
                                           label=paste0(round(cumdiff, 0), " cases vs. baseline")), 
                     size=rel(3))
  }
}
p
  }, height=700, width=1000)
  
}
