library(tidyverse)
library(tidycensus)
library(lubridate)
library(dplyr)

pop  <-  get_estimates(geography = "state",
                       year = 2019,
                       product = "population") %>% 
  filter(variable == "POP") %>% 
  select(NAME, value) %>%
  rename(state = NAME, population = value)

url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"

vac <- read_csv(url) %>% group_by(location) %>%
  mutate(location = recode(location, `New York State` = "New York")) %>% 
  summarize(people_fully_vaccinated = people_fully_vaccinated[which.max(date)]) 

url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
dat <- read_csv(url) %>% left_join(pop, by = "state") %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate(cases = pmax(0,diff(c(0, cases)))) %>%  
  ungroup() %>% 
  group_by(state, date = floor_date(date, "week")) %>% 
  summarize(rate = mean(cases, na.rm=TRUE) / population * 100000, .groups = "drop", 
            population = population)

dat <- dat %>%
  filter(dat$state %in% pop$state) %>% 
  left_join(vac, by = c("state" ="location")) %>% 
  mutate(state = reorder(state, people_fully_vaccinated), 
         percent_pop_fully_vaccianted = people_fully_vaccinated/population)

jet.colors <- colorRampPalette(c("#F0FFFF", "cyan", "#007FFF", "yellow", "#FFBF00", "orange", "red", "#7F0000"), bias = 2.25)

dat %>% filter(date >= lubridate::make_date(2020, 4, 1)) %>%
  ggplot(aes(percent_pop_fully_vaccianted, state, fill = rate)) +
  geom_tile(color = "white", size = 0.35) +
  geom_tile(aes(date, state)) + 
  scale_x_date(expand = c(0,0))
  scale_fill_gradientn(colors = jet.colors(16), na.value = 'white') +
  geom_vline(xintercept = lubridate::make_date(2020, 12, 15), col = "black") +
  theme_minimal() + 
  theme(panel.grid = element_blank()) +
  coord_cartesian(clip = 'off') +
  ggtitle("COVID19") +
  ylab("") +
  xlab("") +  labs(fill = "Rate (detected cases per 100,000 per day)") +
  theme(legend.position = "bottom", text = element_text(size = 8)) + 
  annotate(geom = "text", x = lubridate::make_date(2020, 12, 15), y = 53.5, label = "Vaccine introduced", size = 3, hjust = 0)

ggsave("~/Desktop/covid-image.png", width = 11, heigh = 5.5)


dat <- read_csv(url) %>% left_join(pop, by = "state") %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(cases = pmax(0,diff(c(0, deaths)))) %>%
  ungroup() %>%
  group_by(state, date = floor_date(date, "week")) %>%
  summarize(rate = mean(cases, na.rm=TRUE) / population * 100000, .groups = "drop")

dat <- dat %>%
  filter(dat$state %in% pop$state) %>% 
  left_join(vac, by = c("state" ="location")) %>%
  mutate(state = reorder(state, people_fully_vaccinated_per_hundred))  

jet.colors <- colorRampPalette(c("#F0FFFF", "cyan", "#007FFF", "yellow", "#FFBF00", "orange", "red", "#7F0000"), bias = 2.25)

dat %>% filter(date >= lubridate::make_date(2020, 4, 1)) %>%
  mutate(rate = pmin(3, rate)) %>%
  ggplot(aes(date, state, fill = rate)) +
  geom_tile(color = "white", size = 0.35) +
  scale_x_date(expand = c(0,0)) +
  scale_fill_gradientn(colors = jet.colors(16), na.value = 'white', breaks = c(0, 1, 2, 3), labels = c("0", "1", "2", "3+")) +
  geom_vline(xintercept = lubridate::make_date(2020, 12, 15), col = "black") +
  theme_minimal() + 
  theme(panel.grid = element_blank()) +
  coord_cartesian(clip = 'off') +
  ggtitle("COVID19") +
  ylab("") +
  xlab("") +  labs(fill = "Rate (deaths per 100,000 per day)") +
  theme(legend.position = "bottom", text = element_text(size = 8)) + 
  annotate(geom = "text", x = lubridate::make_date(2020, 12, 15), y = 53.5, label = "Vaccine introduced", size = 3, hjust = 0) 
ggsave("~/Desktop/covid-deaths-image.png", width = 11, heigh = 5.5)

