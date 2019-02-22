#load libraries
library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)

#set working directory to file source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read in data and obtain aggregate
df_orig = read_csv("data/test.csv") %>% 
  janitor::clean_names() %>% 
  select(country:co)
df <- aggregate(. ~ country, df_orig, sum)

#Download .shp file on the web:
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip", destfile = "world_shape_file.zip")
#system("unzip world_shape_file.zip")

#Read the shp file with the rgdal library in R
library(rgdal)
world_spdf=readOGR(dsn = getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")

#add in our data to shapefile
tmp <- left_join(world_spdf@data, df, by = c("NAME" = "country")) %>% 
  select(NAME, LON:co)
world_spdf@data <- tmp

#create dataframe in a tidy format
tidy_world <- tmp %>% 
  gather(pollutant, num_sources, pm25:co)

#shiny stuff
ui <- fluidPage(
  selectInput(inputId = "pollutant", label = "Choose a pollutant", choices = unique(df_tidy$pollutant)),
  
  #plotOutput("plot"),
  leafletOutput("mymap", height = "85vh")
)

server <- function(input, output) {
#  df_subset = reactive({
#    a <- subset(df, country == input$country)
#  })
#  
#  output$plot <- renderPlot({
#    ggplot(df_subset(), aes(local, value)) +
#      geom_point()
#    
#  })
  #subset data based on input pollutant
  df_subset = reactive({
    a <- subset(tidy_world, pollutant == input$pollutant)
  })
  
  output$mymap <- renderLeaflet({
    
    #set bin and color category
    bins <- c(0,2,4,6,8,10)
    pal <- colorBin("YlOrRd", domain = df_subset()$num_sources, bins = bins, na.color = "transparent")
    
    #set text popup
    mytext=paste("Country: ", df_subset()$NAME,"<br/>", "# of Sources: ", df_subset()$num_sources) %>%
      lapply(htmltools::HTML)
    
    #create leaflet map
    leaflet(world_spdf) %>% 
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      setView(lat = 20, lng = 95, zoom = 4) %>% 
      addPolygons(
        fillColor = ~pal(df_subset()$num_sources),
        weight = 2,
        opacity = 1,
        color = "black",
        dashArray = "",
        fillOpacity = .6,
        highlight = highlightOptions(
          weight = 5,
          color = "black",
          dashArray = "",
          fillOpacity = .6,
          bringToFront = TRUE),
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      addLegend(
        pal = pal, 
        values = ~world_spdf@data$ozone, 
        opacity = 0.7, 
        title = "Number of sources", 
        position = "bottomright"
      )
  })
}

shinyApp(ui = ui, server = server)