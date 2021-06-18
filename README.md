# Covid-19_Project
Summer HBS Program Project  

### Let's input our sources below so that we all know where we are getting our data from 

Sources: 

+ library(tidycensus) #For population estimates 
+ *Our world in data covid-19 datasets* (https://github.com/owid/covid-19-data/tree/master/public/data)


### List of What we need to know how to do for week of 6/14/21   
+ Create plots for as many countries 

### List of what we need to do for week of 6/17/21 
+ Plat cases per 100,000 per week 
+ We don't want to do days or Day of the week 
  + We could do the sum of each week and calculate per week totals 
+ Start learning some "dplyr" and "lubridate" package for dates 
  + Join functions 
+ Check to see if the table agrees with the ourworld data that we are using all we want from the data package that lizzie's doing is the table. 
 +  wrangle file - that is basically we run it and organize everything so that we are ready to make plots from it 
  + Maybe, create a function that grabs the table defines per person rates 
  + rda or rds saves as 
  + when we do this we might have to use a join function 
  + join tables so that population and vaccinations for each country are on the same line 
+ Our world data has a column for people with two doses and one dose 
  + We dont want total vaccinations 
+ Find out if na's mean zero 
  + If it doesnt have na.rm
  + We'll have to think about for contries that are missng data for a day of the week how what we'll do with those possible get the average amount of cases for that week 
  
 ###### Lilu's notes: 
 + You probably took notes but here are my notes based on what Rafa said for next steps (in order of what I think the priority should be to complete):
    + For each country, do one plot for percent with one dose and one plot with percent fully vaccinated. 
    + Write a wrangle function that grabs the latest version of the data and processes it into a form ready to be plotted. (I think this may not be super necessary since most of the data is ready to go when you download it)
    + Investigate what NAs mean (this should be on the website somewhere)
