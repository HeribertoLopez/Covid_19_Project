#from Alden

COVIDdata=read.csv("owid-covid-data.csv")
COVIDdataUS=subset(COVIDdata,location == "United States")
install.packages("tidyverse")
library(tidyverse)

CleanCOVIDdataUS = subset(COVIDdataUS, select = c(date, population, total_cases, people_vaccinated, total_deaths))
CleanCOVIDdataUS[is.na(CleanCOVIDdataUS)] = 0
CleanCOVIDdataUS$date = as.Date(CleanCOVIDdataUS$date, format = "%Y-%m-%d")
USCOVID_Plot = ggplot(data = CleanCOVIDdataUS) +
  geom_line(mapping = aes(x = date, y = total_cases, color = "Total Cases")) +
  geom_line(mapping = aes(x = date, y = population, color = "Population")) +
  geom_line(mapping = aes(x = date, y = people_vaccinated, color = "Total Vaccinations")) +
  geom_line(mapping = aes(x = date, y = total_deaths, color = "Total Deaths")) +
  xlab("Date") +
  ylab("Number of People") +
  ggtitle("COVID Pandemic in the US") + theme_bw() + labs(color = "") + theme(legend.position = "top")
USCOVID_Plo



library(tidycensus)


library(tidyverse)
census_api_key("1e0ffbf997d1225d26b4181067ec724a4bd55410")
states %>% 
  filter(NAME != "Alaska",
         NAME != "Hawaii",
         !str_detect(NAME, "Puerto")) %>% 
  ggplot(aes(fill = total_pop)) +
  geom_sf() +
  scale_fill_viridis_c("Total Population")
variables_dec <- load_variables(year = 2020, dataset = "sf1", cache = TRUE)
library(tidyverse)
library(tidycensus)

texas_pop <- get_acs(geography = "county", 
                     variables = "B01003_001", 
                     state = "TX",
                     geometry = TRUE) 

texas_pop
library(leaflet)
library(stringr)
library(sf)
pal <- colorQuantile(palette = "viridis", domain = texas_pop$estimate, n = 10)

texas_pop %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "Population percentiles",
            opacity = 1)


#Population 

utah_pop <- get_acs(geography = "county", 
                    variables = "B01003_001", 
                    state = "UT",
                    geometry = TRUE)

pal <- colorNumeric(palette = "plasma", 
                    domain = utah_pop$estimate)

utah_pop %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "County Populations",
            opacity = 1)

jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_19-covid-Confirmed.csv", sep = "")

us_confirmed_long_jhu <- read_csv(jhu_url) %>% rename(province = "Province/State", 
                                                      country_region = "Country/Region") %>% pivot_longer(-c(province, 
                                                                                                             country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
  # adjust JHU dates back one day to reflect US time, more or
  # less
  mutate(Date = mdy(Date) - days(1)) %>% filter(country_region == 
                                                  "US") %>% arrange(province, Date) %>% group_by(province) %>% 
  mutate(incident_cases = c(0, diff(cumulative_cases))) %>% 
  ungroup() %>% select(-c(country_region, Lat, Long, cumulative_cases)) %>% 
  filter(str_detect(province, "Diamond Princess", negate = TRUE))

#With our world in data

data <- read.csv("https://github.com/owid/covid-19-data/blob/master/public/data/vaccinations/us_state_vaccinations.csv")

file <- "https://github.com/owid/covid-19-data/blob/master/public/data/vaccinations/us_state_vaccinations.csv"
data <- read.csv(file)


csv_data<-read.csv("https://github.com/owid/covid-19-data/blob/master/public/data/vaccinations/us_state_vaccinations.csv")

library(ggplot2)


data<-read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"))
print(data)


plot(data$total_vaccinations , data$location, xlim=c(70000 , 1000000), ylim=c(Alabama))


plot(data$total_vaccinations, as.factor(data$location))
ggplot(data %>% filter(location=='United States'), aes(x = date,  y = daily_vaccinations_per_million)) + geom_point()

Alabama=1
Ohio=2

plot(us_state_vaccinations, cases)
filter(location=='United States')



#This is ourworldindata covid vaccination set: 
df<-read.csv("owid-covid-data.csv")
print(df)
us<-df%>%
filter(location=='United States')

BiocManager::install(covid19.analystucs)
data<-covid19.JHU.data("ts-dep-confirmed")
covid19Explorer(locn=NULL)

data <- covid19.data("ts-confirmed")
generate.SIR.model(data,"Hubei", t0=1,t1=15)
generate.SIR.model(data,"Germany",tot.population=83149300)
generate.SIR.model(data,"Uruguay", tot.population=3500000)
generate.SIR.model(data,"Ontario",tot.population=14570000)
generate.SIR.model(data,"Canada",tot.population=37590000)
world.SIR.model <- generate.SIR.model(data,"ALL", t0=1,t1=15, tot.population=7.8e9, staticPlt=FALSE)
ts.data <- covid19.data("ts-confirmed")
mtrends(ts.data, geo.loc=c("Canada","Ontario","Uruguay","Italy"))

ts.data <- covid19.data("ts-confirmed")
mtrends(ts.data, geo.loc=c("Canada"))

covid19.vaccination(tgt="us",data.fmt = "orig", disclaimer=TRUE)
# retrieve aggregated data
data <- covid19.data("aggregated")
# interactive map of aggregated cases -- with more spatial resolution
live.map(data)
totals.plt(data0=NULL, geo.loc0 = NULL, one.plt.per.page = FALSE, log.plt = TRUE, with.totals=FALSE, interactive.fig=TRUE, fileName = NULL, interactive.display=TRUE)
totals.plt(data0=NULL, geo.loc0 = "us", "Canada", "Mexico", one.plt.per.page = FALSE, log.plt = TRUE, with.totals=FALSE, interactive.fig=TRUE, fileName = NULL, interactive.display=TRUE)

#get time series data
TS.data <- covid19.data("ts-ALL")
#Plot all data 
totals.plt(TS.data , "ALL" , fileName = "totals-all")
#totals for Canada, Ontario, Italy, and Uruguay
totals.plt(TS.data , c("Canada" ,"Ontario" ,"Italy" ,"Uruguay" ) , with.totals = TRUE , one.plt.per.page=FALSE)

#new graph
data <- covid19.data("ts-confirmed")

testVaccCtry <- function(Ctry="Canada"){
  c19.testing.data <- covid19.testing.data ()
  c19.vacc.data <- covid19.vaccination ()
  c19.cases.data <- covid19.data("ts-confirmed")
  
  # data processing
  # # testing data
  # ## select cases for a particular country , given by Ctry
  filter.ctry <- grepl(Ctry , c19.testing.data [ , "Entity" ])
  # ## select columns : date and ( short term ) positive rate
  cols <- c("Date" ,"Short.term.positive.rate")
  tstDta <- na.omit(c19.testing.data [filter.ctry,cols])
  names(tstDta) <- cols
  # ## sort by date
  tstDta <- tstDta[order(tstDta[,1]),]
  # # vaccination data
  # ## remove NAs
  vaccs <- na.omit(c19.vacc.data)
  # ## select specific " Ctry "
  vacc.Ctry <-vaccs[vaccs$location==Ctry,]
  # # confirmed cases
  conf.Ctry<-c19.cases.data[c19.cases.data$Country.Region==Ctry,5:(length(c19.cases.data)-1)]
  # #### Graphics #####
  # plot daily vaccination per million , for every location
  # par ( mfrow =c (5 ,5) )
  # tapply ( vaccs $ daily _ vaccinations _ per _ million , vaccs $ location , plot )
  # par ( mfrow =c (1 ,1) )
  # ##
  # mosaic plot combining testing / vaccination and confirmed cases data
  34
  par(mfrow=c(3,1))
  par(mar=c(1 ,5 ,2 ,5))
  # ## subplot #1
  minX <- as.Date(names(conf.Ctry)[1])
  maxX <- as.Date(names(conf.Ctry)[length(conf.Ctry)])
  # plot positive testing rate vs date
  plot(as.Date(tstDta$Date),tstDta[,2],'l',ylab = "Positive_Testing_Rate", xlim=c(minX,maxX))
  title(Ctry)
  par(new=TRUE)
  # add vaccination data
  plot(as.Date(vacc.Ctry$date),(vacc.Ctry$people_vaccinated) ,
         type = 'l' , col = 'blue' , xlab = NA , xaxt = 'n' , ylab = NA , yaxt = 'n' , xlim =c(minX,maxX ) )
  axis (4 , col.axis = 'blue' , line = -3.5 , las =1 , lwd =0)
  # axis . Date (3 , as . Date ( vacc . Ctry $ date ) , col . axis = ’ blue ’)
  par ( new = TRUE )
  plot ( as.Date ( tstDta $ Date ) , tstDta [ ,2] , 'l' , ylab = NA , yaxt = 'n' , xlim = c ( minX , maxX )
  )
  # axis . Date (3 , as . Date ( vacc . Ctry $ date ) , col . axis = ’ red ’)
  par ( new = TRUE )
  plot ( as.Date ( names ( conf.Ctry ) ) , as.numeric ( conf.Ctry ) , type = 's' , col = 'red' ,
         ylab = NA , yaxt = 'n')
  # axis . Date (3 , as . Date ( vacc . Ctry $ date ) , col . axis = ’ red ’)
  # axis . Date (1 , as . Date ( names ( conf . Ctry ) ) ) # , at = seq ( as . Date ( names ( conf .
  Ctry [1] as.Date ( names ( conf.Ctry [ length ( conf.Ctry ) ]) ) 
axis (4 , col.axis = 'red')
mtext ( " Confirmed ␣ cases " , 4 , line =2 , cex =.65 , col= 'red')

legend ( " top " ,c ( " Pos . Testing ␣ Rate " ," Vaccination " ," Confirmed ␣ Cases " ) , col = c ( 'black', 'blue' , 'red') , lty = c (1 ,1 ,1) , bty = 'n')
rect ( as.Date ( vacc.Ctry $ date ) [1] ,1 , as.Date ( vacc.Ctry $ date ) [ length ( vacc.Ctry $
                                                                                               date ) -1] , as.numeric ( conf.Ctry ) [ length ( conf.Ctry ) -1] ,
       border = 'darkgray' , lty =4 , lwd =1.5)
par ( mar = c (1 ,5 ,1 ,5) )
# ## subplot #2
# adjust limits to match testing / vaccination ranges ...
minX <- max ( as.Date ( tstDta $ Date ) [1] , as.Date ( vacc.Ctry $ date ) [1])
maxX <- min ( as.Date ( vacc.Ctry $ date ) [ length ( vacc.Ctry $ date ) ] , as.Date ( tstDta $
                                                                                                 Date ) [ length ( tstDta $ Date ) ])
plot ( as.Date ( tstDta $ Date ) , tstDta [ ,2] , 'l' , xlim = c ( minX , maxX ) , xlab = NA , ylab = "
Positive ␣ Testing ␣ Rate " )
par ( new = TRUE )
plot ( as.Date ( names ( conf.Ctry ) ) ,( as.numeric ( conf.Ctry ) ) , xlim = c ( minX , maxX ) , type
       = 'l' , col = 'red' , xlab = NA , ylab = NA , yaxt = 'n')
axis (4 , col.axis = " red " )
mtext ( " Confirmed ␣ cases " , 4 , line = 2 , cex =.65 , col ='red')
par ( mar = c (2.5 ,5 ,1 ,5) )
# ## subplot 3
minY = min ( vacc.Ctry $ people_vaccinated )
35
maxY = max ( vacc.Ctry $ people_vaccinated )
plot ( as.Date ( vacc.Ctry $ date ) ,( vacc.Ctry $ people_vaccinated ) ,
       xlim = c ( minX , maxX ) , ylim = c ( minY , maxY ) ,
       type = 'l' , col = 'blue' , ylab = " Vaccinations " )
axis (2 , col.axis = 'blue' , line = -3.5 , las =1 , lwd =0)
par ( new = TRUE )
plot ( as.Date ( vacc.Ctry$date ) , vacc.Ctry$people_fully_vaccinated ,
       xlim = c ( minX , maxX ) , ylim = c ( minY , maxY ) ,
       type = 'l' , col = 'darkgreen' ,
       yaxt = " n " , ylab = NA )
axis (2 , col.axis = 'black')
par ( new = TRUE )
plot ( as.Date ( vacc.Ctry$date ) , vacc.Ctry $ daily_vaccinations_per_million ,
       xlim = c ( minX , maxX ) ,
       type = 'l' , col = 'blue' , lty =2 ,
       yaxt = " n " , ylab = NA )
axis (4 , col.axis = 'blue')
legend ( " top " ,c ( " people ␣ vaccinated " ," fully ␣ vaccinated " ," daily ␣ vacc . ␣ per ␣ M " ) , col =
           c ( 'blue' , 'darkgreen' , 'blue') , lty = c (1 ,1 ,2) , bty = 'n')
par ( new = FALSE )
par ( mfrow = c (1 ,1) )
}

  
  
  