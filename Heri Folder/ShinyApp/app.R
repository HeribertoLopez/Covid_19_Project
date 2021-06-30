#Packages 
library(shiny)
library(tidyverse)
library(lubridate)

#link to raw data file in github
urlfile = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv" 

#reads the most recent updated data from the link above 
owid.df <- read.csv(url(urlfile))

#formatting dates for r to read better 
owid.df$date = as.Date(owid.df$date, format = "%Y-%m-%d") 

#Filtering out potential variables of interest for the plots we want to create 

owid.df <- owid.df %>%
    select( iso_code, continent, location, date, 
           new_cases, total_cases, 
           new_deaths, total_deaths, icu_patients, hosp_patients,
           people_vaccinated, people_fully_vaccinated, new_vaccinations,
           people_vaccinated_per_hundred,
           people_fully_vaccinated_per_hundred, population)  


#Creating a month, week day, and percent with one dose variable 

owid.df <- owid.df %>%  
    mutate(
        month = month(date, label = T), 
        wday = wday(date), 
        prcent_one_dose = people_vaccinated / population, 
        week = format(date, format ="%Y-%U")
    )  

# Somehow I have to get the max function to ignore an 
# NA when there are other values, but to value 
#  something as NA when no value is present

special.max = function(x){
    ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
}

owid.df.summaries <- owid.df %>%
    group_by(continent, location, week) %>% #groups the weekly data first by location, then by week
    summarise(new_cases_weekly=sum(new_cases), #getting weeklynew cases
              new_deaths_weekly=sum(new_deaths), #getting weekly deaths
              pop_week = special.max(population), #why would I sum population
              ppl_fully_vacc_week = special.max(people_fully_vaccinated), 
              ppl_vacc_week = special.max(people_vaccinated), #these aren't new
              date_week = max(date)) #to allow for the columns to be the same length


clean.owid.df <- owid.df.summaries %>% #making the weekly data into the data we want with percentages and stuff
    mutate(new_cases_per_100k = new_cases_weekly/pop_week*100000, #weekly cases every 100,000 ppl
           new_deaths_per_100k = new_deaths_weekly/pop_week*100000, #weekly deaths for every 100,000 ppl
           percent_ppl_fully_vacc = ppl_fully_vacc_week/pop_week*100, #percent ppl fully vaccinated
           percent_ppl_vacc = ppl_vacc_week/pop_week*100) #percent ppl vaccinated


weekly.owid.df = subset(clean.owid.df, select = -c(new_cases_weekly, #dropping the unmodified columns
                                                  new_deaths_weekly, ppl_fully_vacc_week, ppl_vacc_week, 
                                                  week, pop_week))

# Define UI for application t
ui <- fluidPage(
    selectInput(inputId = "location",
                label = "Choose a Country",
                choices = unique(weekly.owid.df$location)),
    # list of non-duplicated countries 
    plotOutput("line")
) 

server <- function(input, output) { 
    output$line <- renderPlot({
        ggplot(weekly.owid.df %>%  
                   filter(location == input$location)) + 
            geom_point(mapping = aes(x = date_week, y = new_cases_per_100k, 
                                     color = "New Weekly Cases"),
                       shape = 17, na.rm=TRUE) + #the shape makes it identifiable to which y-axis
                        geom_point(mapping = aes(x = date_week,
                                                 y = percent_ppl_fully_vacc, 
                                                 color = "People Vaccinated [Weekly %]"), na.rm=TRUE) +
                        geom_point(mapping = aes(x = date_week, y = percent_ppl_vacc, 
                                                 color = "People Fully Vaccinated [Weekly %"), na.rm=TRUE) +
                        geom_point(mapping = aes(x = date_week, y = new_deaths_per_100k,
                                                 color = "New Weekly Deaths"),
                                   shape = 17, na.rm=TRUE) + #The shape makes it identifiable to which y-axis
                        xlab("Date") + ylab("Count Per 100,000 [triangle]") +
                        scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Percent of Population [circle]")) +
                        scale_x_date(date_breaks = "8 weeks", date_minor_breaks = "4 weeks", 
                                     guide = guide_axis(angle = 45)) + #This is just to give the axis ticks some cool slant
                        ggtitle("COVID-19 Pandemic") +
                        labs(color = "") + theme_bw() 
            
            #geom_line(mapping = aes(x = date, y = total_cases, color = "Total Cases")) +
            #geom_line(mapping = aes(x = date, y = population, color = "Population")) +
            #geom_line(mapping = aes(x = date, y = people_vaccinated, color = "Total Vaccinations")) +
            #geom_line(mapping = aes(x = date, y = total_deaths, color = "Total Deaths"))  
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server) 



#library(shiny)
#Run the new weekly data into a set of plots with two axis, one for percent, one for number per 100k people
#ui = fluidPage(
#   selectInput(inputId = "location",
##                label = "Select a Country",
  #              choices = unique(CleanWeekData$location)), # list of non-duplicated countries 
#   plotOutput("line")
#) 

#server = function(input, output){ 
#   output$line = renderPlot( {
#        ggplot(CleanWeekData %>%  
#                   filter(location == input$location)) + 
#            geom_point(mapping = aes(x = date_week, y = new_cases_per_100k, color = "New Weekly Cases"), shape = 17, na.rm=TRUE) + #the shape makes it identifiable to which y-axis
#            geom_point(mapping = aes(x = date_week, y = percent_ppl_fully_vacc, color = "People Vaccinated [Weekly %]"), na.rm=TRUE) +
#            geom_point(mapping = aes(x = date_week, y = percent_ppl_vacc, color = "People Fully Vaccinated [Weekly %"), na.rm=TRUE) +
#            geom_point(mapping = aes(x = date_week, y = new_deaths_per_100k, color = "New Weekly Deaths"), shape = 17, na.rm=TRUE) + #The shape makes it identifiable to which y-axis
#            xlab("Date") + ylab("Count Per 100,000 [triangle]") +
#            scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Percent of Population [circle]")) +
#            scale_x_date(date_breaks = "8 weeks", date_minor_breaks = "4 weeks", 
#                         guide = guide_axis(angle = 45)) + #This is just to give the axis ticks some cool slant
#            ggtitle("COVID-19 Pandemic") +
#            labs(color = "") + theme_bw() 
#    }
#    )
#}
# Run the application 
#shinyApp(ui = ui, server = server) 