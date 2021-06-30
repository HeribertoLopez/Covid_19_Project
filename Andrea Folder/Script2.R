#Download the COVID data
library(tidyverse)
library(drc)

#get data csv
casesurl = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-states.csv" 
vaxurl = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"
stateCases = read.csv(url(casesurl))
stateVax = read.csv(url(vaxurl))


#rename "location" to "state" to ease natural join
stateVax = stateVax %>% rename(state =  location)

#natural join based on equal date and equal state 
#!!! DOES NOT remove dates that don't appear on both cases and vaccine data
# so it will create a lot of empty NA columns
fullData = merge(stateCases, stateVax, by = c("state","date"), all.x = T)


WeeklyCOVIDdata = subset(fullData, select = c(state, 
                                              date, cases, cases_avg_per_100k,
                                              deaths, deaths_avg_per_100k,
                                              people_fully_vaccinated,people_fully_vaccinated_per_hundred, 
                                              people_vaccinated, people_vaccinated_per_hundred))
#breaking up the data with the variables we want, we can change this
WeeklyCOVIDdata$date = as.Date(WeeklyCOVIDdata$date, format = "%Y-%m-%d")
#R doesn't read the dates right
WeeklyCOVIDdata = WeeklyCOVIDdata %>% mutate(week = format(date, format="%Y-%U"))
#making a weekly column to group the data by

special.max = function(x){ #Somehow I have to get the max function to ignore an NA when there are other values, but to value something as NA when no value is present
  ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
}

WeeklyCOVIDdata = WeeklyCOVIDdata %>%
  group_by(state, week) %>% #groups the weekly data first by state, then by week
  summarise(new_cases_weekly=sum(cases), #getting weekly new cases
            new_deaths_weekly=sum(deaths), #getting weekly deaths
            #pop_week = special.max(population) ////not using this
            ppl_fully_vacc_week = special.max(people_fully_vaccinated), 
            ppl_fully_vacc_week_per_hundred = special.max(people_fully_vaccinated_per_hundred),
            ppl_vacc_week = special.max(people_vaccinated), #these aren't new
            ppl_vacc_week_per_hundred = special.max(people_vaccinated_per_hundred),
            avg_new_cases_per_100k = mean(cases_avg_per_100k,na.rm=T),
            avg_deaths_per_100k = mean(deaths_avg_per_100k,na.rm=T),
            date_week = max(date) #to allow for the columns to be the same length
  )
#We can change the variables after this point to get the proportions and percentages Dr. Irazarry wanted, or select different columns initially

CleanWeekData = WeeklyCOVIDdata %>% #making the weekly data into the data we want with percentages and stuff
  rename(new_cases_per_100k = avg_new_cases_per_100k, #weekly cases every 100,000 ppl
         new_deaths_per_100k = avg_deaths_per_100k, #weekly deaths for every 100,000 ppl
         percent_ppl_fully_vacc = ppl_fully_vacc_week_per_hundred, #percent ppl fully vaccinated
         percent_ppl_vacc = ppl_vacc_week_per_hundred) #percent ppl vaccinated
CleanWeekData = subset(CleanWeekData, select = -c(
  #new_cases_weekly, #dropping the unmodified columns
  #new_deaths_weekly, ppl_fully_vacc_week, ppl_vacc_week,    ||removing these bc i wanna keep data
  week))

CleanWeekData = CleanWeekData %>% # I moved this so that the data frame is all nice and I can pull out infinite values
  mutate(vaxChange2 = (percent_ppl_fully_vacc-lag(percent_ppl_fully_vacc))/lag(percent_ppl_fully_vacc)*100,
         vaxChange1 = (percent_ppl_vacc-lag(percent_ppl_vacc))/lag(percent_ppl_vacc)*100,
         change_ppl_fully_vacc = percent_ppl_fully_vacc-lag(percent_ppl_fully_vacc),
         change_ppl_vacc = percent_ppl_vacc-lag(percent_ppl_vacc))
CleanWeekData[mapply(is.infinite, CleanWeekData)] <- NA  #gets rid of any infinite values and stores as NA instead




library(shiny)
#Run the new weekly data into a set of plots with two axis, one for percent, one for number per 100k people
ui = fluidPage(
  selectInput(inputId = "state",
              label = "Select a State",
              choices = unique(CleanWeekData$state),
              selected = "Alabama"), # list of non-duplicated countries 
  plotOutput("Cases"),
  
  plotOutput("Vax"),
  
  plotOutput("CasesVax"),
  
  plotOutput("percentChangeVax"),
  
  plotOutput("ChangeVax"),
  
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
  output$Cases = renderPlot( {
    ggplot(CleanWeekData %>%  
             filter(state %in% input$state)) + 
      geom_point(mapping = aes(x = date_week, y = new_cases_per_100k, 
                               color = "New Weekly Cases"), shape = 17, na.rm=TRUE) + #the shape makes it identifiable to which y-axis
      xlab("Date") + ylab("New Weekly Cases") +
      scale_x_date(date_breaks = "8 weeks", date_minor_breaks = "4 weeks", 
                   guide = guide_axis(angle = 45)) + #This is just to give the axis ticks some cool slant 
      ggtitle("New Weekly Cases of COVID-19") +
      scale_colour_manual(values = c("dodgerblue2")) +
      labs(color = "") + theme_bw() + facet_wrap(state~ .)
  }
  )
  output$Vax = renderPlot( {
    ggplot(CleanWeekData %>%  
             filter(state %in% input$state)) + 
      geom_point(mapping = aes(x = date_week, y = percent_ppl_fully_vacc, 
                               color = "People Fully Vaccinated [Weekly %]"), size = 3, na.rm=TRUE) +
      geom_point(mapping = aes(x = date_week, y = percent_ppl_vacc, 
                               color = "People Vaccinated [Weekly %]"), size = 3, na.rm=TRUE) +
      xlab("Date") + ylab("Percent Vaccination") +
      scale_x_date(date_breaks = "8 weeks", date_minor_breaks = "4 weeks", 
                   guide = guide_axis(angle = 45)) + #This is just to give the axis ticks some cool slant 
      ggtitle("Weekly Vaccinations for COVID-19") +
      scale_x_date(limits= c(
        first(CleanWeekData 
              %>% filter(state %in% input$state, 
                         !is.na(percent_ppl_vacc))%>%pull(date_week)),
        last(CleanWeekData 
             %>% filter(state %in% input$state,
                        !is.na(percent_ppl_vacc))%>%pull(date_week)))) +
      scale_colour_manual(values=c("seagreen3", "purple2")) +
      labs(color = "") + theme_bw() +  facet_wrap(state~ .)
  }
  )
  output$CasesVax = renderPlot( {
    ggplot(CleanWeekData %>%  
             filter(state %in% input$state)) + 
      geom_point(mapping = aes(x = percent_ppl_fully_vacc, color = "People Fully Vaccinated [Weekly %]", 
                               y= new_cases_per_100k), size = 3, na.rm=TRUE) +
      geom_point(mapping = aes(x = percent_ppl_vacc, color = "People Vaccinated [Weekly %]", 
                               y = new_cases_per_100k), size = 3, na.rm=TRUE) +
      xlab("Vaccinations") + ylab("New Cases Per 100,000") +
      ggtitle("New Cases of COVID-19 vs Vaccination Rates ") +
      scale_colour_manual(values=c("blue3", "gold")) +
      labs(color = "") + theme_bw() +  facet_wrap(state~ .)
  }
  )
  output$percentChangeVax = renderPlot({
    ggplot(
      CleanWeekData %>% filter(state %in% input$state)) + #this is to make sure the data starts on the first week that has vaccination data
      scale_x_date(limits= c(
        first(CleanWeekData %>% filter(state %in% input$state,!is.na(percent_ppl_vacc))%>%pull(date_week)),
        last(CleanWeekData %>% filter(state %in% input$state,!is.na(percent_ppl_vacc))%>%pull(date_week))
      ), date_breaks = "2 weeks", date_minor_breaks = "1 week", guide = guide_axis(angle = 45))+
      #y based on 25% each, includes up to 150% just in case ^_^
      scale_y_continuous(limits = c(0,150),breaks=(seq(0,150,25))) +
      geom_point(mapping = aes(x = date_week, y = vaxChange1, color = "People Vaccinated"), 
                 size=3, shape = 17, na.rm=TRUE) + 
      geom_point(mapping = aes(x = date_week, y = vaxChange2, color = "People Fully Vaccinated"), 
                 size=3, shape = 17, na.rm=TRUE) + 
      xlab("Date") + ylab("% Change") + ggtitle("% Change in COVID-19 Vaccinations Over Weeks") +
      scale_colour_manual(values=c("lightcoral", "lightblue2")) +
      labs(color = "") + theme_bw() +  facet_wrap(state~ .)
  })
  
  output$ChangeVax = renderPlot({
    ggplot(
      CleanWeekData %>% filter(state %in% input$state)) + #this is to make sure the data starts on the first week that has vaccination data
      scale_x_date(limits= c(
        first(CleanWeekData %>% filter(state %in% input$state,!is.na(percent_ppl_vacc))%>%pull(date_week)),
        last(CleanWeekData %>% filter(state %in% input$state,!is.na(percent_ppl_vacc))%>%pull(date_week))
      ), date_breaks = "2 weeks", date_minor_breaks = "1 week", guide = guide_axis(angle = 45))+
      #y based on 25% each, includes up to 150% just in case ^_^
      geom_point(mapping = aes(x = date_week, y = change_ppl_vacc, color = "People Vaccinated"), 
                 size=3, shape = 17, na.rm=TRUE) + 
      geom_point(mapping = aes(x = date_week, y = change_ppl_fully_vacc, color = "People Fully Vaccinated"), 
                 size=3, shape = 17, na.rm=TRUE) + 
      xlab("Date") + ylab("Change in % Vaccinations") + ggtitle("Change in % COVID-19 Vaccinations Over Weeks") +
      scale_colour_manual(values=c("cornflowerblue", "firebrick1")) +
      labs(color = "") + theme_bw() +  facet_wrap(state~ .)
  })
  
  #if youre just here for the graphs, ignore all of this
  output$minmaxhead = renderPrint({"state.............Minimum Value..........Max Value"})
  
  output$minmax = renderPrint({
    paste(
      (CleanWeekData %>% filter(state %in% input$state, new_cases_per_100k != 0) %>% summarize(min = min(new_cases_per_100k),max = max(new_cases_per_100k)))
    )
  })
  
  
  output$quantile = renderPrint({
    paste(
      (CleanWeekData %>% filter(state %in% input$state, new_cases_per_100k != 0) %>% summarize(quantile(new_cases_per_100k)))
    )
  })
  
  output$linehead = renderPrint({"Linear Analysis - % People Fully Vaccinated vs New Cases per 100k"})
  
  output$linesum = renderPrint({
    summary(lm(new_cases_per_100k ~ percent_ppl_fully_vacc,
               data = CleanWeekData[CleanWeekData$state %in% input$state,]))
  })
  
  output$loghead = renderPrint({"Linear Analysis - % People Fully Vaccinated vs log(New Cases per 100k)"})
  
  output$log = renderPrint({
    summary(lm(log(new_cases_per_100k) ~ percent_ppl_fully_vacc,
               data = CleanWeekData[CleanWeekData$state %in% input$state,]))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
