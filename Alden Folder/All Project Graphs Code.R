#link for latest dataset
urlfile = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv" 
#reloading dataset 
COVIDdata = read.csv(url(urlfile))

library(tidyverse)
library(drc)
library(lubridate)

WeeklyCOVIDdata = subset(COVIDdata, select = c(location, 
                                               population, date, new_cases, 
                                               new_deaths, people_fully_vaccinated, 
                                               people_vaccinated, hosp_patients, weekly_hosp_admissions)) #breaking up the data with the variables we want, we can change this
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
            date_week = max(date), 
            weekly_hospitalizations = sum(hosp_patients))#to allow for the columns to be the same length
#We can change the variables after this point to get the proportions and percentages Dr. Irazarry wanted, or select different columns initially

CleanWeekData = WeeklyCOVIDdata %>% #making the weekly data into the data we want with percentages and stuff
  mutate(new_cases_per_100k = new_cases_weekly/pop_week*100000, #weekly cases every 100,000 ppl
         new_deaths_per_100k = new_deaths_weekly/pop_week*100000, #weekly deaths for every 100,000 ppl
         percent_ppl_fully_vacc = ppl_fully_vacc_week/pop_week*100, #percent ppl fully vaccinated
         percent_ppl_vacc = ppl_vacc_week/pop_week*100, 
         new_hosp_per_100k = weekly_hospitalizations/pop_week*100000) #percent ppl vaccinated
CleanWeekData = subset(CleanWeekData, select = -c(new_cases_weekly, #dropping the unmodified columns
                                                  new_deaths_weekly, ppl_fully_vacc_week, ppl_vacc_week, 
                                                  week, pop_week, weekly_hospitalizations))

#initial date and finishing date for vaccine prob change
init = CleanWeekData$date_week[which(!is.na(CleanWeekData$date_week))]
end = CleanWeekData$date_week[length(CleanWeekData)]

CleanWeekData = CleanWeekData %>% # I moved this so that the data frame is all nice and I can pull out infinite values
  mutate(vaxChange2 = (percent_ppl_fully_vacc-lag(percent_ppl_fully_vacc))/lag(percent_ppl_fully_vacc)*100,
         vaxChange1 = (percent_ppl_vacc-lag(percent_ppl_vacc))/lag(percent_ppl_vacc)*100,
         change_ppl_fully_vacc = percent_ppl_fully_vacc-lag(percent_ppl_fully_vacc),
         change_ppl_vacc = percent_ppl_vacc-lag(percent_ppl_vacc))
CleanWeekData[mapply(is.infinite, CleanWeekData)] <- NA  #gets rid of any infinite values and stores as NA instead

# fully_vaccinated
CleanWeekData %>%
  filter(location %in% c("Canada","Italy", "Europe", "United Kingdom", "India", "United States")) %>% 
         ggplot() + 
  geom_point(aes(x = date_week, y = percent_ppl_fully_vacc, color = location),
             size = 3, na.rm = TRUE) + 
  xlab("Date") + ylab("Percent People fully Vaccinated") + 
  geom_smooth(aes(x = date_week, y = percent_ppl_fully_vacc, color = location), se = F) + 
  #scale_x_date(date_breaks = "8 weeks", date_minor_breaks = "4 weeks", 
   #            guide = guide_axis(angle = 45)) +   
  ggtitle("Weekly Vaccinations for COVID-19")  +
  scale_x_date(limits= c(
    first(CleanWeekData 
          %>% filter(location %in% c("Canada","Italy", "Europe", "United Kingdom", "India", "United States"), 
                     !is.na(percent_ppl_fully_vacc))%>%pull(date_week)),
    last(CleanWeekData 
         %>% filter(location %in% c("Canada","Italy", "Europe", "United Kingdom", "India", "United States"),
                    !is.na(percent_ppl_fully_vacc))%>%pull(date_week))),  date_labels = "%B/%d", breaks = scales::pretty_breaks(n = 20), 
    guide = guide_axis(angle = 60))


ggsave("~/HBSP/Covid_19_Project/Heri Folder/Vaccinations1.png", width = 11, height = 5.5) 

#ggsave("~/Desktop/covid-deaths-image.png", width = 11, heigh = 5.5) 

# at least one dose 
CleanWeekData %>%
  filter(location %in% c("Canada","Italy", "Europe", "United Kingdom", "India", "United States")) %>% 
  ggplot() + 
  geom_point(aes(x = date_week, y = percent_ppl_vacc, color = location),
             size = 3, na.rm = TRUE) + 
  xlab("Date") + ylab("Percent People with only one dose") + 
  geom_smooth(aes(x = date_week, y = percent_ppl_vacc, color = location), se = F) + 
  #scale_x_date(date_breaks = "8 weeks", date_minor_breaks = "4 weeks", 
  #            guide = guide_axis(angle = 45)) +   
  ggtitle("Weekly Vaccinations for COVID-19")  +
  scale_x_date(limits= c(
    first(CleanWeekData 
          %>% filter(location %in% c("Canada","Italy", "Europe", "United Kingdom", "India", "United States"), 
                     !is.na(percent_ppl_vacc))%>%pull(date_week)),
    last(CleanWeekData 
         %>% filter(location %in% c("Canada","Italy", "Europe", "United Kingdom", "India", "United States"),
                    !is.na(percent_ppl_vacc))%>%pull(date_week))),  date_labels = "%B/%d", breaks = scales::pretty_breaks(n = 20), 
    guide = guide_axis(angle = 60)) 

ggsave("~/HBSP/Covid_19_Project/Heri Folder/Vaccinations2.png", width = 11, height = 5.5) 



#fully vaccinated 
CleanWeekData %>% 
  filter(location %in% c("Canada", "Italy", "Europe", "United Kingdom", "India", "United States")) %>%  
  ggplot() + 
  geom_point(aes(x = percent_ppl_fully_vacc, y = new_cases_per_100k, color = location), size = 3, na.rm = TRUE) + 
  xlab("Percent of People Fully Vaccinated") + ylab("New Cases per 100k") + 
  geom_smooth(aes(x = percent_ppl_fully_vacc, y = new_cases_per_100k, color=location), span = 0.5,se = FALSE) + 
  ggtitle("Weekly Vaccinations for COVID-19") 

ggsave("~/HBSP/Covid_19_Project/Heri Folder/CasvVax1.png", width = 11, height = 5.5) 

#At least one dose 
CleanWeekData %>% 
  filter(location %in% c("Canada", "Italy", "Europe", "United Kingdom", "India", "United States")) %>%  
  ggplot() + 
  geom_point(aes(x = percent_ppl_vacc, y = new_cases_per_100k, color = location), size = 3, na.rm = TRUE) + 
  xlab("Percent of People Vaccinated with only one dose") + ylab("New Cases per 100k") + 
  geom_smooth(aes(x = percent_ppl_vacc, y = new_cases_per_100k, color=location), span = 0.5,se = FALSE) + 
  ggtitle("Weekly Vaccinations for COVID-19") 

ggsave("~/HBSP/Covid_19_Project/Heri Folder/CasvVax2.png", width = 11, height = 5.5) 


# fully_vaccinated
CleanWeekData %>%
  filter(location %in% c("Canada","Italy", "Europe", "United Kingdom", "India", "United States")) %>% 
  ggplot() + 
  geom_point(aes(x = date_week, y = new_cases_per_100k, color = location),
             size = 3, na.rm = TRUE) + 
  xlab("Date") + ylab("New Weekly Cases per 100k") + 
  geom_smooth(aes(x = date_week, y = new_cases_per_100k, color = location), se = F, span =0.1) + 
  #scale_x_date(date_breaks = "8 weeks", date_minor_breaks = "4 weeks", 
  #            guide = guide_axis(angle = 45)) +   
  ggtitle("New Weekly Cases for COVID-19 over time")  +
  scale_x_date(limits= c(
    first(CleanWeekData 
          %>% filter(location %in% c("Canada","Italy", "Europe", "United Kingdom", "India", "United States"), 
                     !is.na(new_cases_per_100k))%>%pull(date_week)),
    last(CleanWeekData 
         %>% filter(location %in% c("Canada","Italy", "Europe", "United Kingdom", "India", "United States"),
                    !is.na(new_cases_per_100k))%>%pull(date_week))),  date_labels = "%B/%d", breaks = scales::pretty_breaks(n = 20), 
    guide = guide_axis(angle = 60))


ggsave("~/HBSP/Covid_19_Project/Heri Folder/Weekly_Cases.png", width = 11, height = 5.5)



library(shiny)
#Run the new weekly data into a set of plots with two axis, one for percent, one for number per 100k people
ui = fluidPage(
  selectInput(inputId = "location",
              label = "Select a Country",
              choices = unique(CleanWeekData$location),
              selected = "United States", 
              multiple = TRUE
              ), # list of non-duplicated countries 
  plotOutput("Cases"),
  
  plotOutput("Vax"),
  
  plotOutput("CasesVax"),
  
  plotOutput("percentChangeVax"),
  
  plotOutput("ChangeVax"),
  
  plotOutput("Hospitalizations"),
  
  plotOutput("CasesHospitalizations"), 
  
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
             filter(location %in% input$location)) + 
      geom_point(mapping = aes(x = date_week, y = new_cases_per_100k, 
                               color = "New Weekly Cases"), shape = 17, na.rm=TRUE) + #the shape makes it identifiable to which y-axis
      xlab("Date") + ylab("New Weekly Cases") +
      scale_x_date(date_breaks = "8 weeks", date_minor_breaks = "4 weeks", 
                   guide = guide_axis(angle = 45)) + #This is just to give the axis ticks some cool slant 
      ggtitle("New Weekly Cases of COVID-19") +
      scale_colour_manual(values = c("dodgerblue2")) +
      labs(color = "") + theme_bw() + facet_wrap(location~ .)
  }
  )
  output$Vax = renderPlot( {
    ggplot(CleanWeekData %>%  
             filter(location %in% input$location)) + 
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
              %>% filter(location %in% input$location, 
                         !is.na(percent_ppl_vacc))%>%pull(date_week)),
        last(CleanWeekData 
             %>% filter(location %in% input$location,
                        !is.na(percent_ppl_vacc))%>%pull(date_week)))) +
      scale_colour_manual(values=c("seagreen3", "purple2")) +
      labs(color = "") + theme_bw() +  facet_wrap(location~ .)
  }

  )
  output$CasesVax = renderPlot( {
    ggplot(CleanWeekData %>%  
             filter(location %in% input$location)) + 
      geom_point(mapping = aes(x = percent_ppl_fully_vacc, color = "People Fully Vaccinated [Weekly %]", 
                               y= new_cases_per_100k), size = 3, na.rm=TRUE) +
      geom_point(mapping = aes(x = percent_ppl_vacc, color = "People Vaccinated [Weekly %]", 
                               y = new_cases_per_100k), size = 3, na.rm=TRUE) +
      xlab("Vaccinations") + ylab("New Cases Per 100,000") +
      ggtitle("New Cases of COVID-19 vs Vaccination Rates ") +
      scale_colour_manual(values=c("blue3", "gold")) +
      labs(color = "") + theme_bw() +  facet_wrap(location~ .)
  }

  )
  output$percentChangeVax = renderPlot({
    ggplot(
      CleanWeekData %>% filter(location %in% input$location)) + #this is to make sure the data starts on the first week that has vaccination data
      scale_x_date(limits= c(
        first(CleanWeekData %>% filter(location %in% input$location,!is.na(percent_ppl_vacc))%>%pull(date_week)),
        last(CleanWeekData %>% filter(location %in% input$location,!is.na(percent_ppl_vacc))%>%pull(date_week))
      ), date_breaks = "2 weeks", date_minor_breaks = "1 week", guide = guide_axis(angle = 45))+
      #y based on 25% each, includes up to 150% just in case ^_^
      scale_y_continuous(limits = c(0,150),breaks=(seq(0,150,25))) +
      geom_point(mapping = aes(x = date_week, y = vaxChange1, color = "People Vaccinated"), 
                 size=3, shape = 17, na.rm=TRUE) + 
      geom_point(mapping = aes(x = date_week, y = vaxChange2, color = "People Fully Vaccinated"), 
                 size=3, shape = 17, na.rm=TRUE) + 
      xlab("Date") + ylab("% Change") + ggtitle("% Change in COVID-19 Vaccinations Over Weeks") +
      scale_colour_manual(values=c("lightcoral", "lightblue2")) +
      labs(color = "") + theme_bw() +  facet_wrap(location~ .)
  })
  
  output$ChangeVax = renderPlot({
    ggplot(
      CleanWeekData %>% filter(location %in% input$location)) + #this is to make sure the data starts on the first week that has vaccination data
      scale_x_date(limits= c(
        first(CleanWeekData %>% filter(location %in% input$location,!is.na(percent_ppl_vacc))%>%pull(date_week)),
        last(CleanWeekData %>% filter(location %in% input$location,!is.na(percent_ppl_vacc))%>%pull(date_week))
      ), date_breaks = "2 weeks", date_minor_breaks = "1 week", guide = guide_axis(angle = 45))+
      #y based on 25% each, includes up to 150% just in case ^_^
      geom_point(mapping = aes(x = date_week, y = change_ppl_vacc, color = "People Vaccinated"), 
                 size=3, shape = 17, na.rm=TRUE) + 
      geom_point(mapping = aes(x = date_week, y = change_ppl_fully_vacc, color = "People Fully Vaccinated"), 
                 size=3, shape = 17, na.rm=TRUE) + 
      xlab("Date") + ylab("Change in % Vaccinations") + ggtitle("Change in % COVID-19 Vaccinations Over Weeks") +
      scale_colour_manual(values=c("cornflowerblue", "firebrick1")) +
      labs(color = "") + theme_bw() +  facet_wrap(location~ .)
  })
  
  output$Hospitalizations = renderPlot({
    ggplot(CleanWeekData %>%  filter(location %in% input$location))  + 
      geom_point(mapping = aes(x = date_week, y = new_hosp_per_100k, 
                               color = "New People Hospitalized per 100k"), size = 3, na.rm=TRUE) +
      xlab("Date") + ylab("Hospitalizations per 100k") +
      scale_x_date(date_breaks = "8 weeks", date_minor_breaks = "4 weeks", 
                   guide = guide_axis(angle = 45)) + #This is just to give the axis ticks some cool slant 
      ggtitle("Weekly Hospitalizations  for COVID-19") +
      ylab("New Hohspitalizations Per 100,000") +
      scale_colour_manual(values = c("dodgerblue2")) +
      labs(color = "") + theme_bw() + facet_wrap(location~ .) 
      
      
    
  })
  
  output$CasesHospitalizations = renderPlot( {
    ggplot(CleanWeekData %>%  
             filter(location %in% input$location)) + 
      geom_point(mapping = aes(x = percent_ppl_fully_vacc, color = "People Fully Vaccinated [Weekly %]", 
                               y= new_hosp_per_100k), size = 3, na.rm=TRUE) +
      geom_point(mapping = aes(x = percent_ppl_vacc, color = "People Vaccinated [Weekly %]", 
                               y = new_hosp_per_100k), size = 3, na.rm=TRUE) +
      xlab("Vaccinations") + ylab("New Hospitalizations Per 100,000") +
      ggtitle("New Cases of COVID-19 Hospitalizations vs Vaccination Rates ") +
      scale_colour_manual(values=c("blue3", "gold")) +
      labs(color = "") + theme_bw() +  facet_wrap(location~ .)
  }
  )
  
  
  #if youre just here for the graphs, ignore all of this
  output$minmaxhead = renderPrint({"Location.............Minimum Value..........Max Value"})
  
  output$minmax = renderPrint({
    paste(
      (CleanWeekData %>% filter(location %in% input$location, new_cases_per_100k != 0) %>% summarize(min = min(new_cases_per_100k),max = max(new_cases_per_100k)))
    )
  })
  
  
  output$quantile = renderPrint({
    paste(
      (CleanWeekData %>% filter(location %in% input$location, new_cases_per_100k != 0) %>% summarize(quantile(new_cases_per_100k)))
    )
  })
  
  output$linehead = renderPrint({"Linear Analysis - % People Fully Vaccinated vs New Cases per 100k"})
  
  output$linesum = renderPrint({
    summary(lm(new_cases_per_100k ~ percent_ppl_fully_vacc,
               data = CleanWeekData[CleanWeekData$location %in% input$location,]))
  })
  
  output$loghead = renderPrint({"Linear Analysis - % People Fully Vaccinated vs log(New Cases per 100k)"})
  
  output$log = renderPrint({
    summary(lm(log(new_cases_per_100k) ~ percent_ppl_fully_vacc,
               data = CleanWeekData[CleanWeekData$location %in% input$location,]))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
