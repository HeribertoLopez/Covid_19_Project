coviddata = read.csv("us-states.csv")
vaccinations = read.csv("us_state_vaccinations.csv")
names(vaccinations)[2] = "state"
total = merge(vaccinations,coviddata,by=c("date","state"))
states = unique(total$state)
library(tidyverse)

for(i in states)
{
  selection = total[total$state==i,]
  rownames(selection)=NULL
  par(mfrow=c(2,2))
  attach(selection)
  plot(cases,ylab="COVID19 cases",xlab="Days since 2021-01-12",pch=20,main=paste("Cases in ",i))
  plot(total_vaccinations,cases,xlab="Total vaccinations",ylab="Cases",main="Cases vs Vaccines",pch=20)
  plot(daily_vaccinations,xlab="Days since 2021-01-12",ylab="Daily Vaccinations",main="Vaccinations per day",pch=20)
  plot(people_fully_vaccinated_per_hundred,xlab="Days since 2021-01-12",ylab="People fully vaccinated",
       pch=20,main="Full vaccinations")
  readline(prompt="Press [enter] to continue")

}
par(mfrow=c(1,1))
#total data by state
ggplot(data = total)+
  geom_point(mapping = aes(x = total_vaccinations, y = cases, color = state) ) +
  xlab("People vaccinated") +
  ylab("COVID-19 Cases") +
  ggtitle("Vaccination vs Case data in all states")

#/////////////
#/////////////
# ignore
# above this
#/////////////
#/////////////

coviddata = read.csv("owid-covid-data.csv")
SAcovid = coviddata[coviddata$continent=="South America",]
#drop unnecessary columns for current stuff
SAdata = SAcovid[-c(1,2,17:25,50:60)]
countries = unique(SAdata$location)
#clean missing data (falkland islands is missing a lot)
countries = countries[-7]


for(i in countries)
{
  selection = SAdata[SAdata$location==i,]
  rownames(selection)=NULL
  par(mfrow=c(2,2))

  #plot 1
  plot(selection$new_cases,xlab="Days since 2020-01-01",ylab = "New COVID-19 Cases",main=paste("Cases in",i),col="lightgreen",pch=20)
  points(selection$new_cases_smoothed,col="lightgoldenrod",pch=20)
  legend(1,max(selection$new_cases,na.rm = T),c("New cases","New cases smoothed"),fill=c("lightgreen","lightgoldenrod"))
  #plot 2
  plot(selection$total_cases,xlab="Days since 2020-01-01",ylab = "Total COVID-19 Cases",main=paste("Cases over time"),col="lightskyblue",pch=20)
  #plot 3
  plot(selection$people_vaccinated,selection$new_cases,pch=20,xlab="People vaccinated",ylab="New COVID-19 Cases",main="Cases vs Vaccinations",col="violetred")
  points(selection$people_vaccinated,selection$new_cases_smoothed,col="violet",pch=20)
  legend(0,max(selection$new_cases,na.rm = T),c("New cases","New cases smoothed"),fill=c("violetred","violet"))
  #plot 4
  plot(selection$people_vaccinated,selection$total_cases,pch=20,xlab="People vaccinated",ylab="Total COVID-19 Cases",main="Cases vs Vaccinations",col="orange")
  
  readline(prompt="Press [enter] to continue")
  
  #plot 5, regression
  par(mfrow=c(1,1))
  plot(selection$people_vaccinated,selection$new_cases,xlab="People vaccinated",ylab="New cases",main=paste("New cases in",i),col="navy",pch=20)
  lines = lm(new_cases ~ people_vaccinated, data = selection)
  abline(lines,col="maroon")
  print(summary(lines))
  
  readline(prompt="Press [enter] to continue")
  
}

ggplot(data = SAdata)+
  geom_point(mapping = aes(x = total_vaccinations, y = new_cases, color = location) ) +
  xlab("People vaccinated") +
  ylab("New COVID-19 Cases") +
  ggtitle("Vaccination vs Case data in all South American countries")
