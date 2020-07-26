rm(list=ls())

base::library(conflicted)
base::library(tidyverse)
base::library(dplyr)
conflict_prefer("filter","dplyr")
conflict_prefer("select", "dplyr")
base::library(here)
conflict_prefer("here", "here")
base::library(ggplot2)
base::library(lessR)
base::library(plm)
base::library(zoo)
conflict_prefer("lag", "stats")
library(plotly)
conflict_prefer("layout", "plotly")


who_data <- read.csv(here("WHO-COVID-19-global-data.csv"), 
                  stringsAsFactors = F)
glimpse(who_data)

table(who_data$WHO_region)
table(who_data$Country)
who_data <- who_data %>% filter(WHO_region == "EURO" |
                                  Country == "United States of America" |
                                  Country == "Brazil" |
                                  Country == "The United Kingdom") 

who_data <- who_data %>% rename(deaths = New_deaths,
                                 cases = New_cases,
                                 date = Date_reported, 
                                country = Country,
                                cumulative_deaths = Cumulative_deaths,
                                cumulative_cases = Cumulative_cases) %>% 
  select(-Country_code, -WHO_region)

table(who_data$country)

who_data <- who_data %>% filter(!(country == "Albania")) %>% 
  filter(!(country == "Armenia")) %>% 
  filter(!(country == "Andorra")) %>%
  filter(!(country == "Azerbaijan")) %>% 
  filter(!(country == "Belarus")) %>% 
  filter(!(country == "Belarus")) %>% 
  filter(!(country == "Belarus")) %>% 
  filter(!(country == "Bosnia and Herzegovina")) %>% 
  filter(!(country == "Faroe Islands")) %>% 
  filter(!(country == "Georgia")) %>% 
  filter(!(country == "Greenland")) %>% 
  filter(!(country == "Gibraltar")) %>% 
  filter(!(country == "Holy See")) %>%
  filter(!(country == "Iceland")) %>%
  filter(!(country == "Isle of Man")) %>%
  filter(!(country == "Israel")) %>%
  filter(!(country == "Jersey")) %>%
  filter(!(country == "Kazakhstan")) %>%
  filter(!(country == "Kosovo[1]")) %>%
  filter(!(country == "Kyrgyzstan")) %>%
  filter(!(country == "Liechtenstein")) %>%
  filter(!(country == "Monaco")) %>%
  filter(!(country == "Montenegro")) %>%
  filter(!(country == "North Macedonia")) %>%
  filter(!(country == "Republic of Moldova")) %>%
  filter(!(country == "Russian Federation")) %>%
  filter(!(country == "San Marino")) %>%
  filter(!(country == "Serbia")) %>%
  filter(!(country == "Tajikistan")) %>%
  filter(!(country == "Turkey")) %>%
  filter(!(country == "Ukraine")) %>%
  filter(!(country == "Uzbekistan"))
  
europe_data <- who_data %>% filter(!(country == "The United Kingdom")) %>% 
  filter(!(country == "Brazil")) %>% 
  filter(!(country == "United States of America"))

others_data <- who_data %>% filter(country == "The United Kingdom" |
                                     country == "Brazil" |
                                     country == "United States of America")
table(others_data$country)

glimpse(europe_data)
europe_data <- europe_data %>% group_by(date) %>% 
  summarise(country = "Europe",
       cases = sum(cases, na.rm = T),
       deaths = sum(deaths, na.rm = T),
       cumulative_cases = sum(cumulative_cases, na.rm = T),
       cumulative_deaths = sum(cumulative_deaths, na.rm = T)) %>% 
  ungroup

my_data <- rbind(europe_data, others_data)
remove(europe_data, others_data, who_data)

my_data <- my_data %>% mutate(country = ifelse(country == "United States of America",
                                               "US", country),
                              country = ifelse(country == "The United Kingdom",
                                               "UK", country))
table(my_data$country)

my_data <- my_data %>% mutate(population = ifelse(country == "Europe", 
                                                  446000000, NA),
                              population = ifelse(country == "US",
                                                  331002651, population),
                              population = ifelse(country == "Brazil",
                                                  212559417, population),
                              population = ifelse(country == "UK",
                                                  67886011, population))
glimpse(my_data)
my_data <- my_data %>% mutate(date = as.Date(date))

base::library("zoo")
brazil <- my_data %>% filter(country == "Brazil") %>% 
  mutate(country = as.character(country)) %>% 
  mutate(new_deaths = as.numeric(deaths)) %>% 
  mutate(new_cases = as.numeric(cases)) %>% 
  mutate(roll_deaths = rollmean(deaths, 7, na.pad = T, align = "right")) %>% 
  mutate(roll_cases = rollmean(cases, 7, na.pad = T, align = "right"))

us <- my_data %>% filter(country == "US") %>% 
  mutate(country = as.character(country)) %>% 
  mutate(new_deaths = as.numeric(deaths)) %>% 
  mutate(new_cases = as.numeric(cases)) %>% 
  mutate(roll_deaths = rollmean(deaths, 7, na.pad = T, align = "right")) %>% 
  mutate(roll_cases = rollmean(cases, 7, na.pad = T, align = "right")) 

europe <- my_data %>% filter(country == "Europe") %>% 
  mutate(country = as.character(country)) %>% 
  mutate(new_deaths = as.numeric(deaths)) %>% 
  mutate(new_cases = as.numeric(cases)) %>% 
  mutate(roll_deaths = rollmean(deaths, 7, na.pad = T, align = "right")) %>% 
  mutate(roll_cases = rollmean(cases, 7, na.pad = T, align = "right")) 

uk <- my_data %>% filter(country == "UK") %>% 
  mutate(country = as.character(country)) %>% 
  mutate(new_deaths = as.numeric(deaths)) %>% 
  mutate(new_cases = as.numeric(cases)) %>% 
  mutate(roll_deaths = rollmean(deaths, 7, na.pad = T, align = "right")) %>% 
  mutate(roll_cases = rollmean(cases, 7, na.pad = T, align = "right")) 

my_data <- rbind(brazil, europe, us, uk)
remove(brazil, europe, us, lag_data, uk)
glimpse(my_data)
my_data <- my_data %>% mutate(cases_permil = roll_cases/1000000) %>% 
  mutate(deaths_permil = roll_deaths/100000) %>% 
  mutate(cases_ratepmil = (roll_cases/population)*1000000) %>% 
  mutate(deaths_ratepmil = (roll_deaths/population)*1000000) 

glimpse(my_data)
my_data <- my_data %>% mutate(roll_cases = ifelse(roll_cases < 0, 0, roll_cases)) %>% 
  mutate(roll_deaths = ifelse(roll_deaths < 0, 0, roll_deaths)) %>% 
  mutate(cases = ifelse(cases < 0, 0, cases)) %>% 
  mutate(deaths = ifelse(deaths < 0, 0, deaths)) %>% 
  mutate(cases_ratepmil = ifelse(cases_ratepmil < 0, 0, cases_ratepmil)) %>% 
  mutate(deaths_ratepmil = ifelse(deaths_ratepmil < 0, 0, deaths_ratepmil))

table(my_data$country)

f1 <- ggplot(my_data, aes(x = date, y = roll_cases, color = country)) +
  geom_line(size = .5) + theme_light() + 
  labs(title = "Daily Registered COVID-19 Cases",
       subtitle = "Rolling 7-day average",
       caption = "Source: Johns Hopkins Coronavirus Resource Center") +
  scale_colour_manual(name = NULL,
                      values = c("seagreen4", "wheat4", "cornflowerblue", "tan1"), 
                      labels = c("Brazil", "European Union", "UK", "US")) +
  ylab(NULL) + xlab(NULL) + 
  theme(axis.text.x = element_text(size = 9),
        legend.position = "bottom") +
  scale_y_continuous(breaks = c(1000,10000, 20000, 30000, 40000, 50000, 60000, 70000))

fig1 <- ggplotly(f1)  %>%
  layout(annotations = 
           list(x = 1, y = -0.1, text = "Source: Johns Hopkins Coronavirus Resource Center", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=11, color="black")),
         legend = list(orientation = 'h', x = .003, y = .00005)
         )
fig1

f2 <- ggplot(my_data, aes(x = date, y = cases_ratepmil, color = country)) +
  geom_line(size = .5) + theme_light() + 
  labs(title = "Daily Registered COVID-19 Cases per Million People",
       subtitle = "Rolling 7-day average",
       caption = "Source: Johns Hopkins Coronavirus Resource Center") +
  scale_colour_manual(name = NULL,
                      values = c("seagreen4", "wheat4", "cornflowerblue", "tan1"), 
                      labels = c("Brazil", "European Union", "UK", "US")) +
  ylab(NULL) + xlab(NULL) + 
  theme(axis.text.x = element_text(size = 9),
        legend.position = "bottom") +
  scale_y_continuous(breaks = c(25, 50, 75, 100, 125, 150, 175, 200)) 
f2
