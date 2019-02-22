library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)

'''
BD <- read_csv("data/bangladesh.csv") %>% 
  select(-location, -utc, -attribution)

ID <- read_csv("data/india.csv") %>% 
  select(-location, -utc, -attribution)

TH <- read_csv("data/thailand.csv") %>% 
  select(-location, -utc, -attribution)

df <- rbind(BD, ID, TH) %>% 
  mutate(country = recode(country, BD = "bangladesh", IN = "india", TH = "thailand")) %>% 
  rename(lng = longitude, lat = latitude)
'''

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df_orig = read_csv("data/test.csv") %>% 
  janitor::clean_names() %>% 
  select(country:co)
df <- aggregate(. ~ country, df_orig, sum)

#Download .shp file on the web:
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip", destfile = "world_shape_file.zip")
#system("unzip world_shape_file.zip")

#Read the file with the rgdal library in R
library(rgdal)
world_spdf=readOGR(dsn = getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
tmp <- left_join(world_spdf@data, df, by = c("NAME" = "country"))
world_spdf@data <- tmp
world_spdf <- world_spdf[grep("Bangladesh|India", world_spdf$NAME),]

#basic leaflet
bins <- c(0,1,2,3)
pal <- colorBin("YlOrRd", domain = world_spdf@data$ozone, bins = bins, na.color = "transparent")

m <- leaflet(world_spdf) %>% 
  addProviderTiles("Stamen.Toner") %>% 
  setView(lat = 20, lng = 95, zoom = 4) 

m %>% 
  addPolygons(
    fillColor = ~pal(world_spdf@data$ozone),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = world_spdf@data$NAME,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    ) %>% 
  addLegend(
    pal = pal, 
    values = ~world_spdf@data$ozone, 
    opacity = 0.7, 
    title = NULL, 
    position = "bottomright"
  )

ui <- fluidPage(
  titlePanel("PM2.5 values"),
  selectInput(inputId = "country", label = "Choose a country", choices = unique(df$country)),
  plotOutput("plot"),
  leafletOutput("mymap")
)

server <- function(input, output) {
  df_subset = reactive({
    a <- subset(df, country == input$country)
  })
  
  output$plot <- renderPlot({
    ggplot(df_subset(), aes(local, value)) +
      geom_point()
    
  })
  output$mymap <- renderLeaflet({
    binpal <- colorBin("Blues", df_subset()$value, 6, pretty = FALSE)
    
    leaflet() %>%
      setView(lng = unique(df_subset()$lng), lat = unique(df_subset()$lat), zoom = 15) %>% 
      addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
      addCircles(data = df, lng = unique(df_subset()$lng), lat = unique(df_subset()$lat), radius = 5, color = ~binpal(value))
  })
}

shinyApp(ui = ui, server = server)