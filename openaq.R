library(httr)
library(jsonlite)
cities <- GET("https://api.openaq.org/v1/cities")
content <- rawToChar(cities$content)
data <- fromJSON(content)
head(data)

countries <- c("BD", "IN", "ID", "NP", "PH", "SG", "LK", "TH", "VN")
measurements <- list()

for (country in countries) {
  print(paste("getting data for ", country))
  measurements[[country]] <- ropenaq::aq_measurements(country = country)
}  

all_countries <- bind_rows(measurements)

monitors_by_location <- all_countries %>% 
  group_by(country, city) %>% 
  summarize(n = n_distinct(location))

total_monitors <- monitors_by_location %>% 
  group_by(country) %>% 
  summarize(n = sum(n))



locations <- GET("https://api.openaq.org/v1/locations?limit=10000&order_by[]=country&order_by[]=city&country[]=BD&country[]=IN&country[]=ID&country[]=NP&country[]=PH&country[]=SG&country[]=LK&country[]=TH&country[]=VN")
content <- rawToChar(locations$content)
data <- fromJSON(content)
monitors <- data$results
monitors_tidy <- monitors %>%
  select(country, city, location, sourceNames, count, firstUpdated, lastUpdated) %>% 
  separate(firstUpdated, c("firstUpdated", "extra_1"), sep = "T", remove = TRUE) %>% 
  separate(lastUpdated, c("lastUpdated", "extra_2"), sep = "T", remove = TRUE) %>% 
  select(-c(extra_1, extra_2)) %>% 
  arrange(country)
num_monitors_city <- monitors_tidy %>% 
  group_by(country, city) %>% 
  summarize(n = n_distinct(location))
num_monitors_country <- num_monitors_city %>% 
  group_by(country) %>% 
  summarize(n = sum(n))

monitors_tidy$sourceNames <- sapply(monitors_tidy$sourceNames, paste0, sep = ", ", collapse="")
monitors_tidy$sourceNames <- substr(monitors_tidy$sourceNames, 1, nchar(monitors_tidy$sourceNames)-2)

write_csv(monitors_tidy, path = "data/openaq/monitors.csv")
