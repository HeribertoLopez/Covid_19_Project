library(tidycensus)
library(tidyverse)
library(tigris)
library(lubridate)
options(tigris_use_cache=TRUE)

census_api_key("8ce6395e8ec025d0c06764b840e7e5fe70221625", overwrite = TRUE, install=TRUE)

data("county_laea") #Dataset with county geometry for use when shifting Alaska and Hawaii 
 
class(county_laea)
 
#fips_codes is a built-in daataset for smart state and county lookup. To access
# directly use: 
#data(fips_codes)  

get_decennial()

us_components <- get_estimates(geography = "state", product = "components")
unique(us_components$variable)

us_pop <- get_estimates(geography = "county", product = "population", year = 2019, geometry = TRUE, resolution = "20m") %>% 
  shift_geometry()

unique(us_pop$variable) 

us_pop <- us_pop %>% 
  filter(variable == "POP")

order = c("10,000,000 +", "1,000,000 to 10,000,000", "100,000 to 1,000,000", "10,000 to 100,000", "10,000 and below")

us_pop <- us_pop %>%
  mutate(groups = case_when(
    value > 10000000 ~ "10,000,000 +",
    value > 1000000 ~ "1,000,000 to 10,000,000",
    value > 100000 ~ "100,000 to 1,000,000",
    value > 10000 ~ "10,000 to 100,000",
    value > 0 ~ "10,000 and below"
  )) %>%
  mutate(groups = factor(groups, levels = order))

state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()

ggplot() +
  geom_sf(data = us_pop, aes(fill = groups, color = groups), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = FALSE) +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto") +
  labs(title = "US Population",
       subtitle = "US Census Bureau 2019 Population Estimates",
       fill = "Population Grouping"
  ) 

# Attempt at Making a plot for cases per day 
covid.dt  <- read_csv("C:/Users/hlop5/Downloads/owid-covid-data.csv") 

head(covid.dt)

dim(covid.dt) 

str(covid.dt)

names(covid.dt)

# selecting our varibales of interest 
asia.covid.df <- covid.dt %>%  
  select(iso_code, continent, location, date, new_deaths, total_deaths,
         new_cases, total_cases, 
         icu_patients, hosp_patients, 
         new_tests, total_tests, 
         new_vaccinations, total_vaccinations, population)

# Editing the format of the data variable 
asia.covid.df$date = as.Date(covid.dt$date, format = "%Y-%m-%d")

# Filtering out the continent of Asia  
asia.covid.df <- covid.dt %>%  
  filter(continent == "Asia") 

#Creating a month variable 
asia.covid.df <- asia.covid.df %>%  
  mutate(
    month = month(date, label = T), 
    wday = wday(date)
  ) 

#Creating a csv file to use for the shiny web app 
#write.csv(asia.covid.df, "~/HBSP/Covid_19_Project/Heri Folder/asia.covid.df", row.names = FALSE)

#variable.order = c("hosp_patients", "icu_patients", 
                   #"new_tests","new_vaccinations",
                   #"new_cases","new_deaths") 

#unique(asia.covid.df$location) 

#function(location){ 
  
#  }



#CleanCOVIDdataUS = 
#  subset(COVIDdataUS, select = c(date, population, 
#                                 total_cases, people_vaccinated, 
#                                 total_deaths)) 

#asia.covid.df %>%  
# filter(!is.na(total_cases) | !is.na(population) | is.na(total_vaccinations))

#CleanCOVIDdataUS[is.na(CleanCOVIDdataUS)] = 0
#CleanCOVIDdataUS$date = as.Date(CleanCOVIDdataUS$date, format = "%Y-%m-%d")
#USCOVID_Plot = ggplot(data = CleanCOVIDdataUS) +
#  geom_line(mapping = aes(x = date, y = total_cases, color = "Total Cases")) +
#  geom_line(mapping = aes(x = date, y = population, color = "Population")) +
#  geom_line(mapping = aes(x = date, y = people_vaccinated, color = "Total Vaccinations")) +
#  geom_line(mapping = aes(x = date, y = total_deaths, color = "Total Deaths")) +
#  xlab("Date") +
#  ylab("Number of People") +
#  ggtitle("COVID Pandemic in the US") + theme_bw() + labs(color = "") + theme(legend.position = "top")
#USCOVID_Plot

#covid.mn.tbl %>%
#  pivot_longer(c(pcr.cases:hosp.cases, deaths)) %>%
#  mutate(name = factor(name, levels=variable.order)) %>%
#  ggplot(aes(x=date, y=value, color=month))+
#  geom_point()+
#  labs(x="Date", y="Logarithmic scale")+
#  facet_grid(.~name)+
#  scale_y_log10()

