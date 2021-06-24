#Download the COVID data
COVIDdata = read.csv("owid-covid-data.csv")
library(tidyverse)
library(drc)
WeeklyCOVIDdata = subset(COVIDdata, select = c(location, 
                                               population, date, new_cases, 
                                               new_deaths, people_fully_vaccinated, 
                                               people_vaccinated)) #breaking up the data with the variables we want, we can change this
WeeklyCOVIDdata$date = as.Date(WeeklyCOVIDdata$date, format = "%Y-%m-%d")
#R doesn't read the dates right
WeeklyCOVIDdata = WeeklyCOVIDdata %>% mutate(week = format(date, format="%Y-%U"))
#making a weekly column to group the data by

special.max = function(x){ #Somehow I have to get the max function to ignore an NA when there are other values, but to value something as NA when no value is present
  ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
}
WeeklyCOVIDdata = WeeklyCOVIDdata %>%
  group_by(location, week) %>% #groups the weekly data first by location, then by week
  summarise(new_cases_weekly=sum(new_cases), #getting weeklynew cases
            new_deaths_weekly=sum(new_deaths), #getting weekly deaths
            pop_week = special.max(population), #why would I sum population
            ppl_fully_vacc_week = special.max(people_fully_vaccinated), 
            ppl_vacc_week = special.max(people_vaccinated), #these aren't new
            date_week = max(date)) #to allow for the columns to be the same length
#We can change the variables after this point to get the proportions and percentages Dr. Irazarry wanted, or select different columns initially

CleanWeekData = WeeklyCOVIDdata %>% #making the weekly data into the data we want with percentages and stuff
  mutate(new_cases_per_100k = new_cases_weekly/pop_week*100000, #weekly cases every 100,000 ppl
         new_deaths_per_100k = new_deaths_weekly/pop_week*100000, #weekly deaths for every 100,000 ppl
         percent_ppl_fully_vacc = ppl_fully_vacc_week/pop_week*100, #percent ppl fully vaccinated
         percent_ppl_vacc = ppl_vacc_week/pop_week*100) #percent ppl vaccinated
CleanWeekData = subset(CleanWeekData, select = -c(new_cases_weekly, #dropping the unmodified columns
                                                  new_deaths_weekly, ppl_fully_vacc_week, ppl_vacc_week, 
                                                  week, pop_week))

#initial date and finishing date for vaccine prob change
init = CleanWeekData$date_week[which(!is.na(CleanWeekData$date_week))]
end = CleanWeekData$date_week[length(CleanWeekData)]

library(shiny)
#Run the new weekly data into a set of plots with two axis, one for percent, one for number per 100k people
ui = fluidPage(
  selectInput(inputId = "location",
              label = "Select a Country",
              choices = unique(CleanWeekData$location)), # list of non-duplicated countries 
  plotOutput("line"),
  
  plotOutput("percentChangeVax"),
  
  mainPanel(
    verbatimTextOutput("minmaxhead"),
    verbatimTextOutput("minmax"),
    verbatimTextOutput("quantilehead"),
    verbatimTextOutput("quantile"),
    verbatimTextOutput("linehead"),
    verbatimTextOutput("linesum"),
    verbatimTextOutput("loghead"),
    verbatimTextOutput("log")
    )
) 

server = function(input, output){ 
  output$line = renderPlot( {
    ggplot(CleanWeekData %>%  
             filter(location == input$location)) + 
      geom_point(mapping = aes(x = date_week, y = new_cases_per_100k, color = "New Weekly Cases"), shape = 17, na.rm=TRUE) + #the shape makes it identifiable to which y-axis
      geom_point(mapping = aes(x = date_week, y = percent_ppl_fully_vacc, color = "People Vaccinated [Weekly %]"), na.rm=TRUE) +
      geom_point(mapping = aes(x = date_week, y = percent_ppl_vacc, color = "People Fully Vaccinated [Weekly %"), na.rm=TRUE) +
      geom_point(mapping = aes(x = date_week, y = new_deaths_per_100k, color = "New Weekly Deaths"), shape = 17, na.rm=TRUE) + #The shape makes it identifiable to which y-axis
      xlab("Date") + ylab("Count Per 100,000 [triangle]") +
      scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Percent of Population [circle]")) +
      scale_x_date(date_breaks = "8 weeks", date_minor_breaks = "4 weeks", 
                   guide = guide_axis(angle = 45)) + #This is just to give the axis ticks some cool slant 
      ggtitle("COVID-19 Pandemic") +
      labs(color = "") + theme_bw() 
  }
  )
  
  output$percentChangeVax = renderPlot({
    ggplot(
      CleanWeekData %>% filter(location == input$location)%>%
        mutate(vaxChange2 = (percent_ppl_fully_vacc-lag(percent_ppl_fully_vacc))/lag(percent_ppl_fully_vacc)*100,
               vaxChange1 = (percent_ppl_vacc-lag(percent_ppl_vacc))/lag(percent_ppl_vacc)*100
               ))+
    #this is to make sure the data starts on the first week that has vaccination data
    scale_x_date(limits= c(
      first(CleanWeekData %>% filter(location == input$location,!is.na(percent_ppl_vacc))%>%pull(date_week)),
      last(CleanWeekData %>% filter(location == input$location,!is.na(percent_ppl_vacc))%>%pull(date_week))
      ), date_breaks = "2 weeks", date_minor_breaks = "1 week", guide = guide_axis(angle = 45))+
    #y based on 25% each, includes up to 150% just in case ^_^
      scale_y_continuous(limits = c(0,150),breaks=(seq(0,150,25))) +
    geom_point(mapping = aes(x = date_week, y = vaxChange1, color = "People Vaccinated"),size=4, shape = 20, na.rm=TRUE) + 
    geom_point(mapping = aes(x = date_week, y = vaxChange2, color = "People Fully Vaccinated"),size=4, shape = 1, na.rm=TRUE) + 
    xlab("Date") + ylab("Change in %") + ggtitle("% Change in COVID-19 Vaccinations Over Weeks") +
    labs(color = "") + theme_bw() 
  })
  
  
  
  #if youre just here for the graphs, ignore all of this
  output$minmaxhead = renderPrint({"Location.............Minimum Value..........Max Value"})
  
  output$minmax = renderPrint({
    paste(
      (CleanWeekData %>% filter(location == input$location, new_cases_per_100k != 0) %>% summarize(min = min(new_cases_per_100k),max = max(new_cases_per_100k)))
    )
  })
  
  
  output$quantile = renderPrint({
    paste(
      (CleanWeekData %>% filter(location == input$location, new_cases_per_100k != 0) %>% summarize(quantile(new_cases_per_100k)))
    )
  })
  
  output$linehead = renderPrint({"Linear Analysis - % People Fully Vaccinated vs New Cases per 100k"})
  
  output$linesum = renderPrint({
    summary(lm(new_cases_per_100k ~ percent_ppl_fully_vacc,
                data = CleanWeekData[CleanWeekData$location==input$location,]))
  })
  
  output$loghead = renderPrint({"Linear Analysis - % People Fully Vaccinated vs log(New Cases per 100k)"})
  
  output$log = renderPrint({
    summary(lm(log(new_cases_per_100k) ~ percent_ppl_fully_vacc,
               data = CleanWeekData[CleanWeekData$location==input$location,]))
      })
  
}
# Run the application 
shinyApp(ui = ui, server = server) 
