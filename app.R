#check for missing packages and install
list.of.packages <- c("shiny", "shinydashboard", "tidyverse", "leaflet", "rgdal")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

#load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)

#set working directory to file source directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read in availability data and obtain aggregate
df_orig <- read_csv("data/test.csv") %>% 
  select(Country:CO) %>% 
  rename("PM2.5" = pm25)
df <- aggregate(. ~ Country, df_orig, sum)

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
  select(NAME, LON:CO) 
tmp <- tmp %>% 
  mutate(id = 1:nrow(tmp))   
  #mutate_at(c("PM2.5", "Household Air Pollution", "Ozone", "NOx", "SO2", "CO", "VOCs"), funs(case_when(. > 7 ~ "High", . > 3 & . <= 7 ~ "Med", . <= 3 ~ "Low")))
world_spdf@data <- tmp

#put dataframe in a tidy format
tidy_world <- tmp %>%
  gather(pollutant, num_sources, PM2.5:CO)

#create dashboard header
header <- dashboardHeader(
  title = "South and Southeast Asia Air Pollution", titleWidth = "100vw"
)

#create dashboard body
body <- dashboardBody(
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
                 inputId = "pollutant", label = "Choose a pollutant", choices = unique(tidy_world$pollutant)
               )),
           box(width = NULL, height = 350,
               title = "Trends", status = "primary", 
               textOutput("country"),
               textOutput("nodata"),
               tags$head(tags$style("#country{font-size: 20px;
                                    font-weight: bold;
                                    text-align: center}"),
                         tags$style("#nodata{text-align:center}")),
               uiOutput("data"))
           ) #sidebar
  )
)

#initiate UI
ui <- dashboardPage(
  skin = "blue",
  header,
  dashboardSidebar(disable = TRUE),
  body
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
  df_subset <- reactive({
    a <- subset(tidy_world, pollutant == input$pollutant) %>% 
      mutate(cat_sources = cut(num_sources, breaks = c(0, 3, 7, 10), labels = c("Low", "Medium", "High")))
  })
  
  df_with_data <- reactive({
    b <- subset(tidy_world, pollutant == input$pollutant) %>% 
      filter(!is.na(num_sources))
  })
  
  #create leaflet map output
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      # fitBounds(min(df_with_data()$LON)+2, min(df_with_data()$LAT-10), max(df_with_data()$LON)+8, max(df_with_data()$LAT)+7)
      setView(lat = 13, lng = 101, zoom = 4) 
  })
  
  #set bins for color
  bins <- c(1,2,4,6,8,10)
  
  observe({
    #set bin and color category
    #pal <- colorBin("YlOrRd", domain = df_subset()$num_sources, bins = bins, na.color = "transparent")
    pal <- colorFactor("YlOrRd", df_subset()$cat_sources)
    
    #set text popup
    mytext = paste("Country: ", df_subset()$NAME,"<br/>", "# of Sources: ", df_subset()$num_sources) %>%
      lapply(htmltools::HTML)
    
    #add data to map
    leafletProxy("mymap") %>%
                   clearShapes() %>%
                   addPolygons(data = world_spdf,
                     fillColor = ~pal(df_subset()$cat_sources),
                     weight = 1.5,
                     opacity = 1,
                     color = "grey",
                     dashArray = "",
                     fillOpacity = .6,
                     layerId = df_subset()$id,
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
                     )
    })
  
  #create legend separately
  observe({
    proxy <- leafletProxy("mymap", data = df_subset())
    proxy %>% clearControls()
    proxy %>%
      addLegend(
        pal <- colorFactor("YlOrRd", df_subset()$cat_sources),
        values = ~df_subset()$cat_sources,
        opacity = 0.7,
        title = "Number of sources",
        position = "bottomleft"
      )
  })
  
  rv <- reactiveValues(data = NULL)
  
  #output plot based on click
  observeEvent(input$mymap_shape_click, {
    event <- input$mymap_shape_click
  
    name <- df_subset()$NAME[df_subset()$id == event$id] #get country name based on ID
    print(df_subset())
    leafletProxy("mymap") %>% 
      setView(df_subset()$LON[df_subset()$NAME == name], df_subset()$LAT[df_subset()$NAME == name], zoom = 5)
    
    #filter data based on country and pollutant
    rv$tb <- pm %>%
      select(country, parameter, value, local) %>%
      filter(country == name, parameter == input$pollutant)

    output$plot <- renderPlot({
      rv$tb %>% ggplot() +
        geom_line(aes(local, value))
    })

    output$country <- renderText({
      paste(name, " (", input$pollutant, ")", sep = "")
    })

    if (name %in% rv$tb$country & input$pollutant %in% rv$tb$parameter){
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
  })
  
  #reset view button
  observeEvent(input$reset_button, {
    leafletProxy("mymap") %>%
      # fitBounds(min(df_with_data()$LON)+2, min(df_with_data()$LAT-10), max(df_with_data()$LON)+8, max(df_with_data()$LAT)+7)
      setView(lat = 13, lng = 101, zoom = 4)
  })

    #create plot in popup if data available
    # if (name %in% rv$tb$country & input$pollutant %in% rv$tb$parameter) {
    #   output$plot <- renderPlot({
    #   rv$tb %>% ggplot() +
    #     geom_point(aes(local, value))
    #   })
    #   showModal(
    #     modalDialog(
    #       title = paste(name, " ", input$pollutant),
    #       plotOutput("plot"),
    #       footer = actionButton("dismiss_modal",label = "Close")
    #     )
    #   )
    # } else {
    #   showModal(
    #     modalDialog(
    #       "No data avilable"
    #     )
    #   )
    # }
#  })

  # #reset plot if modal dismissed
  # observeEvent(input$dismiss_modal, {
  #   output$plot <- NULL
  #   removeModal()
  # })
}
shinyApp(ui = ui, server = server)
