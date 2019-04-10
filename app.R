#check for missing packages and install
list.of.packages <- c("shiny", "shinydashboard", "tidyverse", "leaflet", "rgdal", "httr", "jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

#load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(RColorBrewer)

#set working directory to file source directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("data.R", local = TRUE)

#create dashboard header
header <- dashboardHeader(
  title = "South and Southeast Asia Air Pollution", titleWidth = "100vw"
)

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                 height: 100px;
                                 -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 2;    /* Firefox */ 
                                 column-count: 2; 
                                 -moz-column-fill: auto;
                                 -column-fill: auto;
                                 }
                                 .checkbox{
                                 margin-top: 0px !important;
                                 padding-left: 20px;
                                 -webkit-margin-after: 0px !important; 
                                 }
                                 
                                 ")) 
  ))

#create dashboard body
body <- dashboardBody(
  tweaks,
  fluidRow(
    column(width = 8,
           box(width = NULL, 
               leafletOutput("mymap", height = "81vh"),
               column(width = 12, actionButton("reset_button", "Reset View"), align = "center")
           ) 
    ), 
    column(width = 4,
           box(width = NULL,
               selectInput(
                 inputId = "pollutant", label = "Choose a pollutant", choices = unique(air_quality_annual$pollutant)
               )),
           box(width = NULL, height = 350,
               title = "Trends", status = "primary", 
               textOutput("country"),
               textOutput("nodata"),
               tags$head(tags$style("#country{font-size: 20px;
                                    font-weight: bold;
                                    text-align: center}"),
                         tags$style("#nodata{text-align:center}")),
               uiOutput("data")
               ), 
           box(width = NULL,
               selectInput(
                 inputId = "year", label = "Choose a year", choices = unique(sort(air_quality_annual$year))
               )),
           box(width = NULL,  height = 150, solidHeader = TRUE,
               title = "Information",
               tags$div(align = "left",
                        class = "multicol",
                        checkboxGroupInput("info",
                                           label = NULL,
                                           choices = list("Monitors" = "monitors",
                                                          "Pollutant Level" = "pol_lvl")
                        )
               )
           )
               ) 
    )
)

#initiate UI
ui <- dashboardPage(
  skin = "blue",
  header,
  dashboardSidebar(disable = TRUE),
  body
)

server <- function(input, output, session) {
  
  #subset data based on input pollutant
  df_subset <- reactive({
    a <- subset(world_spdf@data, pollutant %in% c(input$pollutant, NA) & year %in% c(input$year, NA))
  })
  
  #create leaflet map output
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Esri.WorldGrayCanvas", options = providerTileOptions(minZoom = 4)) %>% 
      # fitBounds(min(df_with_data()$LON)+2, min(df_with_data()$LAT-10), max(df_with_data()$LON)+8, max(df_with_data()$LAT)+7)
      setView(lat = 13, lng = 101, zoom = 4)
  })
  
  #set bins for color
  bins <- c(1,2,4,6,8,10)
  
  observe({
    #set bin and color category
    #pal <- colorBin("YlOrRd", domain = df_subset()$num_sources, bins = bins, na.color = "transparent")
    pal <- colorNumeric("YlOrRd", df_subset()$concentration)
    print(df_subset())  
    #set text popup
    mytext = paste("Country: ", df_subset()$NAME,"<br/>", "Level (ug/m3): ", df_subset()$concentration, df_subset()$year) %>%
      lapply(htmltools::HTML)
    
    #add data to map
    #country_specific <- subset(world_spdf, NAME == "Burma")
    #leafletProxy("mymap") %>% 
    #  addPolygons(data = country_specific,
    #              fillColor = "Blue",
    #              weight = 1.5,
    #              opacity = 1,
    #              color = "grey",
    #              fillOpacity = .3)
    leafletProxy("mymap") %>%
      clearShapes() %>%
      addMapPane("polygons", zIndex = 410) %>% 
      addMapPane("pollution", zIndex = 420) %>% 
      addPolygons(data = world_spdf,
                  fillColor = ~pal(df_subset()$concentration),
                  weight = 1.5,
                  opacity = 1,
                  color = "grey",
                  dashArray = "",
                  fillOpacity = .6,
                  layerId = df_subset()$id,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "black",
                    dashArray = "",
                    fill = NULL,
                    bringToFront = TRUE),
                  label = mytext,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  options = pathOptions(pane = "polygons")
      )  
  })
  
  #create legend separately
  observe({
    proxy <- leafletProxy("mymap", data = df_subset())
    proxy %>% clearControls()
    proxy %>%
      addLegend(
        pal <- colorNumeric("YlOrRd", df_subset()$concentration),
        values = ~df_subset()$concentration,
        opacity = 0.7,
        title = paste(input$pollutant, "Level", sep = " "),
        position = "bottomleft"
      )
  })
  
  rv <- reactiveValues(data = NULL)
  
  #output plot based on click
  observeEvent(input$mymap_shape_click, {
    event <- input$mymap_shape_click
    print(event$id)
    
    name <- df_subset()$NAME[df_subset()$id == event$id] #get country name based on ID
    country_sp <- df_subset() %>% mutate(AREA = 5.8 * (1-((AREA-min(AREA))/(max(AREA)-min(AREA))))) %>% 
      filter(NAME == name)
    
    leafletProxy("mymap") %>% 
      setView(country_sp$LON, country_sp$LAT, zoom = country_sp$AREA)
    
    #filter data based on country and pollutant
    rv$tb <- df_subset() %>%
      filter(NAME == name)
    
    output$plot <- renderPlot({
      rv$tb %>% ggplot() +
        geom_bar(aes(year, concentration), stat = "identity", width = 0.5)
    })
    
    output$country <- renderText({
      paste(name, " (", input$pollutant, ")", sep = "")
    })
    
    if (name %in% rv$tb$country & input$pollutant %in% rv$tb$pollutant){
      output$data <- renderUI({
        plotOutput("plot", height = 250)
      })
      output$nodata <- NULL
    } else {
      output$nodata <- renderText({
        "No data available"
      })
      output$data <- NULL
    }
    
  })
  
  #clear plot if input changes
  observeEvent(input$pollutant, {
    change <- input$pollutant
    output$plot <- NULL
    output$data <- NULL
    output$nodata <- NULL
    output$country <- NULL
    leafletProxy("mymap") %>% 
      clearMarkers()
  })
  
  #reset view button
  observeEvent(input$reset_button, {
    leafletProxy("mymap") %>%
      # fitBounds(min(df_with_data()$LON)+2, min(df_with_data()$LAT-10), max(df_with_data()$LON)+8, max(df_with_data()$LAT)+7)
      setView(lat = 13, lng = 101, zoom = 4) 
  })
  
  #create reactive measurments dataset
  ms <- reactive({
    c <- measurements_tidy %>% 
      filter(year == input$year, pollutant == input$pollutant)
  })
  
  #add/remove components w/ checkbox
  observe({
    proxy <- leafletProxy("mymap", data = ms())
    proxy %>% clearMarkers() %>% removeControl("level") 
    
    checkbox <- input$info
    
    m <- "monitors" %in% checkbox
    s <- "pol_lvl" %in% checkbox
    
    #set pm25 color
    pal <- colorBin(c("#3c9b01", "#c60000"), ms()$level, bins = c(0, 10, 15, 25, 35, 50, ceiling(max(ms()$level))))
    
    #add monitor markers
    addmonitor <- function(){
      proxy %>%
        addMarkers(~lng, ~lat, popup = ~city)
    }
    
    #add pm25 data
    addlevel <- function(x) {
      leafletProxy("mymap", data = x) %>% 
        addCircleMarkers(~lng, ~lat,
                         radius = ~level / 10,
                         color = ~pal(level),
                         fillOpacity = 0.5,
                         opacity = 0.7,
                         popup = ~paste(sep = "<br/>",
                                        paste("<strong>Country: </strong>", country),
                                        paste("<strong>City: </strong>", city),
                                        paste("<strong>PM2.5: </strong>", round(level), " ug/m<sup>3</sup>")),
                         options = pathOptions(pane = "pollution")) %>% 
        removeControl("level") %>% 
        addLegend(
          title = paste(input$pollutant, "(ug/m<sup>3</sup>)"),
          pal = pal,
          values = ~level,
          opacity = 0.7,
          position = "bottomright",
          labels = c("0", "1", "2", "3", "4", "5"),
          layerId = "level")
    }
    
    if (m) {
      addmonitor()
    } else if (s){
      addlevel(ms())
    }
    if (m&s){
      addmonitor()
      addlevel(ms())
    }
  })
}
shinyApp(ui = ui, server = server)
