library(tidyverse)

#country profile data
ctrprof <- read_csv("data/country_profile.csv") %>% 
  select(country, percent_urban) %>% 
  mutate(cat_pct_urban = cut(percent_urban, breaks = c(0, 25, 50, 75, 100), labels = c("Low", "Medium", "High", "Very High"))) %>% 
  filter(!is.na(percent_urban))

#read in database
air_quality <- read_csv("data/air_quality_estimates.csv") %>% 
  janitor::clean_names() %>%
  rename(city_region = city_or_region_if_applicable, month = month_if_applicable) %>% 
  select(country:unit)  

air_quality_annual <- air_quality %>% 
  filter(resolution == "Annual") %>% 
  select(-city_region, -month)
air_quality_annual$unit[air_quality_annual$unit == "??g/m3"] <- "ug/m3"
air_quality_monthly <- air_quality %>% 
  filter(resolution == "Monthly")
air_quality_calculated <- air_quality_monthly %>%
  group_by(country, year, pollutant) %>% 
  summarize(concentration = mean(concentration))

air_quality_calculated %>% 
  filter(country == "Nepal", pollutant == "PM2.5") %>% 
  ggplot() +
  geom_point(aes(year, concentration))

health_effects <- read_csv("data/health_effects.csv") %>% 
  janitor::clean_names()
health_effects_deaths <- health_effects %>% 
  filter(measure == "Deaths", age == "All ages")

#Download .shp file on the web:
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip", destfile = "world_shape_file.zip")
#system("unzip world_shape_file.zip")

#Read the shp file with the rgdal library in R
library(rgdal)
world_spdf <- readOGR(dsn = paste(getwd(), "shapefile", sep = "/"), layer = "TM_WORLD_BORDERS_SIMPL-0.3")

#subset asia shp
world_spdf <- subset(world_spdf, REGION == 142)

#add in our data to shapefile
world_spdf@data <- world_spdf@data %>% 
  mutate(id = 1:nrow(world_spdf@data), NAME = recode(NAME, Burma = "Myanmar", "Viet Nam" = "Vietnam"))
tmp <- left_join(world_spdf@data, air_quality_annual, by = c("NAME" = "country")) %>%
  select(NAME, AREA, LON:concentration, unit)

#ctrprof <- left_join(world_spdf@data, ctrprof, by = c("NAME" = "country"))
#ctrprof$NAME[ctrprof$NAME == "Viet Nam"] <- "Vietnam"
#ctrprof <- ctrprof %>% 
#  mutate(id = 1:nrow(ctrprof))

world_spdf@data <- tmp

#load sources data
sources <- read_csv("data/sources.csv") %>% 
  janitor::clean_names() %>% 
  filter(!is.na(country), !is.na(study_year)) %>% 
  rename_at(.vars = vars(ends_with("_percent")), .funs = funs(gsub("_percent", "", .))) %>% 
  select(site_location:other)

#tidy sources data
sources_tidy <- sources %>%
  select(country, site_typology, season, methodology, reference_author, study_year, pollutant, sea_salt:residential_fuel_combustion) %>% 
  gather(source, value, sea_salt:residential_fuel_combustion) %>% 
  # filter(study_year_end != 2002)
  arrange(study_year) %>% 
  mutate(study_year = as.factor(study_year)) %>% 
  filter(season == "year", pollutant == "PM2.5")

#load monitor data
measurements <- read_csv("data/WHO_AirQuality_Database_2018.csv") %>% 
  janitor::clean_names() %>% 
  select(country:longitude, region, -contains("type"), -reference, -(conc_pm25:color_pm10)) %>% 
  rename(lat = latitude, lng = longitude) %>%
  filter(region == "Wpr_LM" | region == "Sear", country != "China")

measurements_tidy <- measurements %>% 
  gather(pollutant, level, pm25, pm10) %>% 
  mutate(pollutant = recode(pollutant, "pm25" = "PM2.5", "o3" = "Ozone", "no2" = "NOx", "so2" = "SO2", "co" = "CO"))

monitors <- measurements %>% 
  distinct(country, city, lat, lng)

