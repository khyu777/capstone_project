library(tidyverse)

#read in availability data and obtain aggregate
df_orig <- read_csv("data/test.csv") %>% 
  select(Country:CO) %>% 
  rename("PM2.5" = pm25)
df <- aggregate(. ~ Country, df_orig, sum)



#country profile data
ctrprof <- read_csv("data/country_profile.csv") %>% 
  select(country, percent_urban) %>% 
  mutate(cat_pct_urban = cut(percent_urban, breaks = c(0, 25, 50, 75, 100), labels = c("Low", "Medium", "High", "Very High"))) %>% 
  filter(!is.na(percent_urban))

#read in database
air_quality <- read_csv("data/air_quality.csv") %>% 
  janitor::clean_names() %>%
  rename(city_region = city_or_region_if_applicable, month = month_if_applicable) %>% 
  select(country:unit) %>% 
  mutate(year = as.factor(year))

air_quality_annual <- air_quality %>% 
  filter(resolution == "Annual")
air_quality_monthly <- air_quality %>% 
  filter(resolution == "Monthly")
air_quality_calculated <- air_quality_monthly %>%
  group_by(country, year, pollutant) %>% 
  summarize(concentration = mean(concentration))

air_quality_calculated %>% 
  filter(country == "Nepal", pollutant == "PM2.5") %>% 
  ggplot() +
  geom_point(aes(year, concentration))

#Download .shp file on the web:
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip", destfile = "world_shape_file.zip")
#system("unzip world_shape_file.zip")

#Read the shp file with the rgdal library in R
library(rgdal)
world_spdf <- readOGR(dsn = paste(getwd(), "shapefile", sep = "/"), layer = "TM_WORLD_BORDERS_SIMPL-0.3")

#subset asia shp
world_spdf <- subset(world_spdf, REGION == 142 | NAME == "Taiwan")

#add in our data to shapefile
tmp <- left_join(world_spdf@data, df, by = c("NAME" = "Country")) %>% 
  select(NAME, AREA, LON:CO) 
tmp <- tmp %>% 
  mutate(id = 1:nrow(tmp))
ctrprof <- left_join(world_spdf@data, ctrprof, by = c("NAME" = "country"))
ctrprof$NAME[ctrprof$NAME == "Viet Nam"] <- "Vietnam"
ctrprof <- ctrprof %>% 
  mutate(id = 1:nrow(ctrprof))

world_spdf@data <- ctrprof

#put dataframe in a tidy format
tidy_world <- tmp %>%
  gather(pollutant, num_sources, PM2.5:CO)

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

