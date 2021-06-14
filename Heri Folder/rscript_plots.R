library(tidycensus)
library(tidyverse)
library(tigris)
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

names(covid.dt)

