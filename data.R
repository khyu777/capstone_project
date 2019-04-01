library(tidyverse)

#read in availability data and obtain aggregate
df_orig <- read_csv("data/test.csv") %>% 
  select(Country:CO) %>% 
  rename("PM2.5" = pm25)
df <- aggregate(. ~ Country, df_orig, sum)

#country profile data
ctprof <- read_csv("data/country_profile.csv") %>% 
  select(country, percent_urban) %>% 
  mutate(cat_pct_urban = cut(percent_urban, breaks = c(0, 25, 50, 75, 100), labels = c("Low", "Medium", "High", "Very High"))) %>% 
  filter(!is.na(percent_urban))
#read in database


#combine actual pollutant data
BD <- read_csv("data/bangladesh.csv") %>% 
  mutate(country = recode(country, "BD" = "Bangladesh"), value = replace(value, which(value < 0), NA))
IN <- read_csv("data/india.csv") %>% 
  mutate(country = recode(country, "IN" = "India"))
TH <- read_csv("data/thailand.csv") %>% 
  mutate(country = recode(country, "TH" = "Thailand"))
pm <- do.call(rbind, list(BD, IN, TH)) %>% 
  select(country, local, parameter, value) %>% 
  mutate(parameter = recode(parameter, "pm25" = "PM2.5", "o3" = "Ozone", "no2" = "NOx", "so2" = "SO2", "co" = "CO"))

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
tmp <- left_join(tmp, ctprof, by = c("NAME" = "country"))
tmp <- tmp %>% 
  mutate(id = 1:nrow(tmp))   
world_spdf@data <- tmp

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

