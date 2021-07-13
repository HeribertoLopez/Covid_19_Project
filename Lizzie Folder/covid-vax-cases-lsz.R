library(tidyverse)
library(tidycensus)
library(lubridate)

pop  <-  get_estimates(geography = "state",
                       year = 2019,
                       product = "population") %>%
  filter(variable == "POP") %>%
  select(NAME, value) %>%
  rename(state = NAME, population = value)

url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"
vac <- read_csv(url) %>% 
  mutate(location = recode(location, `New York State` = "New York")) %>%
  mutate(state = location) %>%
  select(date, state, people_fully_vaccinated_per_hundred)

url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
dat <- read_csv(url) %>% left_join(pop, by = "state") %>%
  left_join(vac, by = c('date','state')) %>%
  filter(date >= lubridate::make_date(2020,12,1)) %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(cases = pmax(0,diff(c(0, cases)))) %>%
  ungroup() %>%
  mutate(people_fully_vax_bin = cut(people_fully_vaccinated_per_hundred, breaks=100)) %>%
  group_by(state, people_fully_vax_bin) %>%
  summarize(rate = mean(cases, na.rm=TRUE) / population * 100000, .groups = "drop") %>%
  distinct()

lastbin <- unique(dat$people_fully_vax_bin)[length(unique(dat$people_fully_vax_bin))]

dat <- dat %>%
  filter(people_fully_vax_bin!=lastbin) %>%
  filter(rate < 200) %>% # get rid of big outlier
  separate(people_fully_vax_bin, into=c('fully_vax_start','fully_vax_end'), sep=',') %>%
  mutate(fully_vax_start = as.numeric(gsub('\\(','',fully_vax_start)))

dat$fully_vax_start[which(dat$fully_vax_start==-0.0665)] <- 0

dat %>%
  ggplot(aes(x=as.factor(fully_vax_start), y=as.factor(state), fill=rate)) +
  geom_tile(color = "white", size = 0.35) +
  scale_fill_gradientn(colors = jet.colors(16), na.value = 'white') +
  theme_minimal() + 
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle=90, hjust=1)) +
  coord_cartesian(clip = 'off') +
  ggtitle("COVID19") +
  ylab("") +
  xlab("percent fully vaccinated") +  
  labs(fill = "Rate (detected cases per 100,000 per day)") +
  theme(legend.position = "bottom", text = element_text(size = 8))
  