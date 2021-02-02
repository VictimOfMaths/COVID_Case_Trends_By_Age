library(shiny)

ui <- fluidPage(
  
titlePanel("Visualising age-specific patterns in recent COVID-19 case data"),

sidebarPanel(
  
  selectInput('area', 'Select Area', 
              c("England", "East of England", "East Midlands", "London", "North East",
                "North West", "South East", "South West", "West Midlands",
                "Yorkshire and The Humber",
                sort(as.character(unique(data.cases$areaName[data.cases$areaType=="ltla"])))), 
              multiple=FALSE, selected="England"),
  radioButtons('plot', 'Age groups', choices=c("Overall", "Age-specific (detailed)", 
                                               "Age-specific (broad)"), selected="Overall", inline=TRUE),
  sliderInput('FitRange', 'Select data range to fit baseline from', min=min(data.cases$date), 
              max=max(data.cases$date)-days(1), value=c(as.Date("2021-01-08"), as.Date("2021-01-14"))),
  radioButtons('scale', "Fix y-axis scales to be the same for all ages?", choices=c("Yes", "No"), inline=TRUE),
  radioButtons('labels', "Display labels showing difference between baseline and observed?", 
               choices=c("Yes", "No"), inline=TRUE)),

mainPanel(
  plotOutput('plot')
)
)