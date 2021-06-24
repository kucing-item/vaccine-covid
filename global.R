# load libraries ----------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)
library(highcharter)

library(formattable)
library(glue)
library(lubridate)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(sever)
library(jsonlite)
library(httr)
library(shinyalert)
library(shinycssloaders)

options(scipen = 999)


mapdata <- read_csv("www/mapdata.csv")

# get data ----------------------------------------------------------------

covid <- fromJSON("https://api.kawalcorona.com/", flatten = T)
colnames(covid) <- c("OBJECTID", "Country", "Last_Update", "Lat", "Log", "Confirmed", "Deaths", "Recovered", "Active")
colnames(covid)

dat_covid <- covid %>% 
  select(Country, Confirmed, Recovered, Deaths)

df_map <- dat_covid %>%
  mutate(
    Country = case_when(
      Country == "US" ~ "United States of America",
      Country == "Korea, South" ~ "South Korea",
      Country == "Czechia" ~ "Czech Republic",
      Country == "Serbia" ~ "Republic of Serbia",
      Country == "Taiwan*" ~ "Taiwan",
      TRUE ~ Country
    )
  ) %>%
  left_join(mapdata, by = c("Country" = "name"))

# confirmed cases ---------------------------------------------------------


today_confirmed <- dat_covid %>%
  select(Country, Confirmed) %>%
  mutate(datetime = Sys.Date() %>% as.character(),
         Country = as.factor(Country))

colnames(today_confirmed) <-
  colnames(today_confirmed) %>% str_to_lower()

ts_confirmed <-
  read.csv(
    url(
      "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    )
  )

date_seq <- seq(
  from = ymd("2020-01-22"),
  length.out = ncol(ts_confirmed) - 4,
  by = "day"
)

colnames(ts_confirmed) <-
  c("province_state",
    "country",
    "lat",
    "long",
    as.character(date_seq))



ts_confirmed_long <- pivot_longer(
  ts_confirmed,
  cols = -c(province_state, country, lat, long),
  names_to = "datetime",
  values_to = "confirmed"
) %>%
  select(country, confirmed, datetime) %>%
  bind_rows(today_confirmed) %>%
  arrange(country)




# recovered ---------------------------------------------------------------


today_recovered <- dat_covid %>%
  select(Country, Recovered) %>%
  mutate(datetime = Sys.Date() %>% as.character(),
         Country = as.factor(Country))

colnames(today_recovered) <-
  colnames(today_recovered) %>% str_to_lower()

ts_recovered <-
  read.csv(
    url(
      "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
    )
  )

date_seq_recov <- seq(
  from = ymd("2020-01-22"),
  length.out = ncol(ts_recovered) - 4,
  by = "day"
)

colnames(ts_recovered) <-
  c("province_state",
    "country",
    "lat",
    "long",
    as.character(date_seq_recov))



ts_recovered_long <- pivot_longer(
  ts_recovered,
  cols = -c(province_state, country, lat, long),
  names_to = "datetime",
  values_to = "recovered"
) %>%
  select(country, recovered, datetime) %>%
  bind_rows(today_recovered) %>%
  arrange(country)



# deaths ------------------------------------------------------------------

today_deaths <- dat_covid %>%
  select(Country, Deaths) %>%
  mutate(datetime = Sys.Date() %>% as.character(),
         Country = as.factor(Country))

colnames(today_deaths) <- colnames(today_deaths) %>% str_to_lower()

ts_deaths <-
  read.csv(
    url(
      "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    )
  )


date_seq_deaths <- seq(
  from = ymd("2020-01-22"),
  length.out = ncol(ts_deaths) - 4,
  by = "day"
)

colnames(ts_deaths) <-
  c("province_state",
    "country",
    "lat",
    "long",
    as.character(date_seq_deaths))

ts_deaths_long <- pivot_longer(
  ts_deaths,
  cols = -c(province_state, country, lat, long),
  names_to = "datetime",
  values_to = "deaths"
) %>%
  select(country, deaths, datetime) %>%
  bind_rows(today_deaths) %>%
  arrange(country)


# vaksin dataset -------------------------------------------

vak <- read.csv(
  url(
    "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
    )
)

vak_name <- read.csv(
  url(
    "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv"
  )
)

vak <- vak %>% 
  mutate(date = ymd(date),
         across(.cols = c(location, iso_code), .fns = as.factor))
vak_name <- vak_name %>% 
  mutate(date = ymd(last_observation_date),
         source_name = as.factor(source_name))

vak_clean <- vak %>% 
  left_join(vak_name, by = "location") %>% 
  select(-iso_code.y, -date.y, -last_observation_date, -source_name, -source_website) %>% 
  rename(country = location,
         iso_code = iso_code.x,
         date = date.x)

vak_clean <- vak_clean %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(vaccines = replace_na(vaccines, "unknown"))

choose_country <- vak_clean %>% 
  distinct(country) %>% pull(country)
