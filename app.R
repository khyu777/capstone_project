#check for missing packages and install
list.of.packages <- c("shiny", "shinydashboard", "tidyverse", "leaflet", "rgdal", "httr", "jsonlite", "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

#load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(plotly)

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
                                 height: 40px;
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
                                 ")
                            ) 
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
           box(width = NULL, height = "10vh",
               status = "primary",
               selectInput(
                 inputId = "pollutant", label = "Choose a pollutant", choices = unique(air_quality_annual$pollutant)
               )),
            box(width = NULL, height = "10vh",
                status = "primary",
                selectInput(
                 inputId = "year", label = "Choose a year", choices = unique(sort(air_quality_annual$year))
               )),
           tabBox(width = NULL, height = "41vh",
               title = span("Annual Trends",
                            style = "font-weight: bold"),
               tabPanel(
                 "Health",
                 textOutput("graph_instruction"),
                 textOutput("death_plot_title"),
                 textOutput("nodata"),
                 tags$head(tags$style("#death_plot_title{font-size: 15px;
                                      font-weight: bold;
                                      text-align: center}"),
                           tags$style("#nodata{text-align:center}"),
                           tags$style("#graph_instruction{text-align:center}")),
                 uiOutput("dp")
               ),
               tabPanel(
                 "Pollution",
                 textOutput("pollution_plot_title"),
                 textOutput("nodata_pp"),
                 tags$head(tags$style("#pollution_plot_title{font-size: 15px;
                                      font-weight: bold;
                                      text-align: center}"),
                           tags$style("#nodata_pp{text-align:center}")),
                 uiOutput("pp")
               ),
               tabPanel(
                 "Sources",
                 textOutput("sources_plot_title"),
                 textOutput("nodata_sp"),
                 tags$head(tags$style("#sources_plot_title{font-size: 15px;
                                      font-weight: bold;
                                      text-align: center}"),
                           tags$style("#nodata_sp{text-align:center}")),
                 uiOutput("sp")
               )),
          box(width = NULL,  height = "20vh", status = "primary",
              column(
                12,
                div("Information", style="font-size:150%; font-weight: bold"),
                tags$div(align = "left",
                         class = "multicol",
                         checkboxGroupInput("info",
                                            label = NULL,
                                            choices = list("Monitoring Stations" = "monitors",
                                                           "Pollutant Level" = "pol_lvl")
                         )
                ),
                div("NOTE: Monitoring station data is only avilable between 2009-2016", style = "padding:7px"), 
                div(style = "border-top: 1px solid black; padding:6px"),
                div(actionButton(inputId='ab1', label="Explore Database", 
                             onclick ="window.open('https://docs.google.com/spreadsheets/d/1GtNEIDimJ1mGutMAVG9udTnHNXJWuN3oQGJDx2Nnmk0/edit?zx=bn3fh2b3rczn#gid=973186796', '_blank')"),
                    align = "center"
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
  
  #graph instruction
  output$graph_instruction <- renderText({
    "Click on a country to see graphs"
  })
  #subset data based on input pollutant
  df_subset <- reactive({
    a <- subset(world_spdf@data, pollutant %in% c(input$pollutant, NA) & year %in% c(input$year, NA))
  })
  df_subset_data <- reactive({
    df_subset() %>%
      filter(!is.na(concentration))
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
    pal <- colorNumeric("YlOrRd", c(floor(min(df_subset_data()$concentration)), ceiling(max(df_subset_data()$concentration))))
    #set text popup
    mytext = paste("Country: ", df_subset()$NAME,"<br/>", "Level (ug/m3): ", df_subset()$concentration) %>%
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
                  fillOpacity = .7,
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
        pal <- colorBin("YlOrRd", 
                            c(floor(min(df_subset_data()$concentration)), ceiling(max(df_subset_data()$concentration))),
                            reverse = TRUE),
        values = c(floor(min(df_subset_data()$concentration)), ceiling(max(df_subset_data()$concentration))),
        opacity = 0.9,
        title = paste("<center> Annual </br>", input$pollutant, "</center>"),
        position = "bottomleft",
        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      )
    print(df_subset_data()$concentration)
  })
  
  rv <- reactiveValues(data = NULL)
  
  #output plot based on click
  observeEvent(input$mymap_shape_click, {
    event <- input$mymap_shape_click
    
    output$graph_instruction <- NULL
    
    name <- df_subset()$NAME[df_subset()$id == event$id] #get country name based on ID
    country_sp <- df_subset() %>% mutate(AREA = 5.4 * (1-((AREA-min(AREA))/(max(AREA)-min(AREA))))) %>% 
      filter(NAME == name)
    
    leafletProxy("mymap") %>% 
      setView(country_sp$LON, country_sp$LAT, zoom = country_sp$AREA)
    
    #create deaths_plot output
    rv$deaths <- health_effects_deaths %>%
      filter(country == name, pollutant == "Ambient PM")
    
    output$death_plot <- renderPlot({
      rv$deaths %>%
        ggplot() +
        geom_line(aes(year, value, color = cause)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_x_continuous(breaks = seq(1990, 2020, by = 5))
    })
    
    output$death_plot_title <- renderText({
      paste("Ambient PM Attributable Deaths by Cause (", name, ")", sep = "")
    })
    
    #create pollution_plot output
    rv$pollution <- air_quality_annual %>% 
      filter(country == name, pollutant == "Ambient PM2.5")
    
    output$pollution_plot <- renderPlot({
      rv$pollution %>% 
        ggplot() +
        geom_line(aes(year, concentration), group = 1) + 
        scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) + 
        geom_hline(yintercept = 20, linetype = "dashed", color = "red", size = 1)+
        geom_text(aes(min(rv$pollution$year),20,label = "WHO Standard", vjust = -1, hjust = -0.05))
    })
    
    output$pollution_plot_title <- renderText({
      paste("Annual Ambient PM2.5 Concentration (", name, ")", sep = "")
    })
    
    #create source_plot output
    rv$sources <- sources_tidy %>% 
      filter(country == name)
    print(rv$sources)
    output$sources_plot <- renderPlot({
      rv$sources %>% 
        ggplot() +
        geom_bar(aes(study_year, value, fill = source), stat = "identity")
    })
    
    output$sources_plot_title <- renderText({
      paste("Sources from PM2.5 (", name, ")", sep = "")
    })
    
    #output death_plot
    if (name %in% rv$deaths$country){
      output$dp <- renderUI({
        plotOutput("death_plot", height = "32vh")
      })
      output$nodata <- NULL
    } else {
      output$nodata <- renderText({
        "No data available"
      })
      output$dp <- NULL
    }
    
    #output pollution_plot
    if (name %in% rv$pollution$country){
      output$pp <- renderUI({
        plotOutput("pollution_plot", height = "32vh")
      })
      output$nodata_pp <- NULL
    } else {
      output$nodata_pp <- renderText({
        "No data available"
      })
      output$pp <- NULL
    }
    
    #output sources_plot
    if (name %in% rv$sources$country){
      output$sp <- renderUI({
        plotOutput("sources_plot", height = "32vh")
      })
      output$nodata_sp <- NULL
    } else {
      output$nodata_sp <- renderText({
        "No data available"
      })
      output$sp <- NULL
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
      filter(year == input$year, pollutant == "PM2.5")
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
