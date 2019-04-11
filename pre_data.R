library(tidyverse)
library(plotly)

#air quality data processing
air_quality <- read_csv("data/air_quality_estimates.csv") %>% 
  janitor::clean_names() %>%
  rename(city_region = city_or_region_if_applicable, month = month_if_applicable) %>% 
  select(country:unit)

air_quality_annual <- air_quality %>% 
  filter(resolution == "Annual") %>% 
  select(-city_region, -month)
air_quality_monthly <- air_quality %>% 
  filter(resolution == "Monthly")
#annual
summary(air_quality_annual)
head(air_quality_annual)
air_quality_annual %>% 
  ggplot(aes(x=concentration)) +
  geom_histogram(bins = 40)

#monthly
summary(air_quality_monthly)
head(air_quality_monthly)
air_quality_monthly %>% 
  ggplot(aes(x=concentration)) +
  geom_histogram(bins = 40)

#data quality data processing
data_quality <- read_csv("data/data_quality.csv") %>% 
  janitor::clean_names() %>% 
  select(-reference)

summary(data_quality)
head(data_quality)

data_quality %>% 
  ggplot(aes(x=pm2_5)) +
  geom_histogram(bins = 40)
data_quality %>% 
  ggplot(aes(x=pm10)) +
  geom_histogram(bins = 40)

#health effects data processing
health_effects <- read_csv("data/health_effects.csv") %>% 
  janitor::clean_names()
health_effects_deaths <- health_effects %>% 
  filter(measure == "Deaths", age == "All ages")
health_effects_dalys <- health_effects%>% 
  filter(measure == "DALYs")

summary(health_effects)
head(health_effects)

health_effects_dalys %>% 
  ggplot(aes(x=value)) +
  geom_histogram(bins = 30)

health_effects %>% 
  group_by(country, cause, measure) %>% 
  summarise(avg = mean(value))

#policy data processing
policy <- read_csv("data/policy.csv") %>% 
  janitor::clean_names() %>% 
  rename(year = year_enacted_enforced, standard_type = type_of_standard_ambient_vehicle_emissions_etc) %>% 
  select(country, year:unit, ministries_sectors_responsible)

summary(policy)
head(policy)

policy %>% 
  filter(standards == 0)

#sources data processing
sources <- read_csv("data/sources.csv") %>% 
  janitor::clean_names() %>% 
  filter(!is.na(country), !is.na(study_year)) %>% 
  rename_at(.vars = vars(ends_with("_percent")), .funs = funs(gsub("_percent", "", .))) %>% 
  rename(traffic = traffic_percent_includes_motor_vehicles_two_stroke_engines_diesel_vehicles_zn_sources_usually_from_diesel_or_two_stroke_vehicles) %>% 
  select(site_location:other_unspecified_human_origin, -reference_author)
#  separate(study_year, c("study_year_start", "study_year_end"), sep = "/") %>% 
#  mutate(study_year_start = as.numeric(study_year_start), study_year_end = as.numeric(study_year_end))
summary(sources)
head(sources)

#tidy sources data
sources_tidy <- sources %>%
  filter(country == "Bangladesh", pollutant == "PM2.5", site_location == "Dhaka", methodology == "PMF", site_typology == "semi-residential", season == "year") %>% 
  select(study_year, pollutant, sea_salt:other_unspecified_human_origin) %>% 
  gather(source, value, sea_salt:other_unspecified_human_origin) %>% 
 # filter(study_year_end != 2002)
  arrange(study_year) %>% 
  mutate(study_year = as.factor(study_year)) %>% 
  filter(study_year != "2001/2002")

### Plots
#plotly sources
sources_tidy %>% 
  #filter(study_year == "2010/2015") %>% 
  filter(country == "India") %>% 
  plot_ly(x = ~study_year, y = ~value, color = ~source, type = 'bar') %>% 
  layout(barmode = 'stack')

sources_tidy %>% 
  filter(country == "India") %>% 
  ggplot() +
  geom_bar(aes(study_year, value, fill = source), stat = "identity")

sources_tidy %>% 
  filter(study_year == "2010/2015") %>% 
  plot_ly(labels = ~source, values = ~value,
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text',
          text = ~paste("hello"),
          showlegend = FALSE) %>% 
  add_pie(hole = 0.4)

sources_tidy %>% 
  ggplot(aes(x = study_year, y = value)) +
  geom_area(aes(group = source, fill = source))

sources_tidy %>% 
  filter(study_year == "2010/2015") %>% 
  ggplot(aes(x="", y = value, fill = source)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0)

#air quality estimates plot (annual)
air_quality_annual %>% 
  filter(country == "Bangladesh", pollutant == "Ambient PM2.5") %>% 
  ggplot() +
  geom_line(aes(year, concentration), group = 1) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) + 
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", size = 1)
air_quality_calculated <- air_quality_monthly %>%
  group_by(country, year, pollutant) %>% 
  summarize(concentration = mean(concentration)) %>% 
  filter(country == "India")
corr_standard <- policy %>% 
  filter(country == "India", pollutant == "PM10", averaging_period == "Annual")

air_quality_calculated %>% 
  ggplot() +
  geom_point(aes(year, concentration)) +
  geom_hline(yintercept = corr_standard$standards, linetype = "dashed", color = "red", size = 1) +
  annotate("text", min(air_quality_calculated$year), corr_standard$standards, hjust = 0, vjust = -1, label = paste(corr_standard$year, "Standard", sep = " "))

#health effects vs pollution level
dalys_bangladesh <- health_effects_dalys %>% 
  filter(country == "Bangladesh", cause == "Lower respiratory infections", age=="All ages", pollutant == "Ambient PM")
pm25_bangladesh <- air_quality_annual %>% 
  filter(country == "Bangladesh", pollutant == "Ambient PM2.5")
bangladesh <- dalys_bangladesh %>% 
  right_join(dalys_bangladesh, pm25_bangladesh, by = c("year" = "year"))
bangladesh <- bangladesh %>% 
  select(year, value.x, life_expectancy_state_of_global_air.x)
ggplot(bangladesh, aes(x=year)) +
  geom_line(aes(y=value, colour = "Health")) +
  geom_line(aes(y=concentration, colour = "Conc")) +
  scale_y_continuous(sec.axis = sec_axis(~.*.2))

health_effects_deaths %>% 
  filter(country == "Bangladesh", pollutant == "Ambient PM") %>% 
  ggplot() +
  geom_line(aes(year, value, color = cause))
