#load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)

#set working directory to file source directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read in data and obtain aggregate
df_orig = read_csv("data/test.csv") %>% 
  select(Country:CO) %>% 
  rename("PM2.5" = pm25)
df <- aggregate(. ~ Country, df_orig, sum)

BD <- read_csv("data/bangladesh.csv") %>% 
  mutate(country = recode(country, "BD" = "Bangladesh"))
IN <- read_csv("data/india.csv") %>% 
  mutate(country = recode(country, "IN" = "India"))
TH <- read_csv("data/thailand.csv") %>% 
  mutate(country = recode(country, "TH" = "Thailand"))
pm <- do.call(rbind, list(BD, IN, TH)) %>% 
  select(country, local, parameter, value) %>% 
  mutate(parameter = recode(parameter, "pm25" = "PM2.5", "o3" = "Ozone", "no2" = "NOx", "so2" = "SO2", "co" = "CO"  ))

#Download .shp file on the web:
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip", destfile = "world_shape_file.zip")
#system("unzip world_shape_file.zip")

#Read the shp file with the rgdal library in R
library(rgdal)
world_spdf=readOGR(dsn = getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")

#add in our data to shapefile
tmp <- left_join(world_spdf@data, df, by = c("NAME" = "Country")) %>% 
  select(NAME, LON:CO) 
tmp <- tmp %>% 
  mutate(id=1:nrow(tmp))
world_spdf@data <- tmp

#create dataframe in a tidy format
tidy_world <- tmp %>%
  gather(pollutant, num_sources, PM2.5:CO)

#create header
header <- dashboardHeader(
  title = "South and Southeast Asia Air Pollution", titleWidth = "100vw"
)

#create body
body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("mymap", height = "85vh")
               )
           ),
    column(width = 3,
           box(width = NULL,
               selectInput(
                 inputId = "pollutant", label = "Choose a pollutant", choices = unique(tidy_world$pollutant)
               )),
           box(width = NULL,
               title = "Trends", status = "primary", solidHeader = TRUE,
               textOutput("country"),
               textOutput("nodata"),
               tags$head(tags$style("#country{font-size: 20px;
                                    font-weight: bold;
                                    text-align: center}"),
                         tags$style("#nodata{text-align:center}")),
               uiOutput("plt"))
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
    
    #create leaflet map
    leaflet(world_spdf) %>% 
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      fitBounds(88, -12, 152, 32)
      #setView(lat = 15, lng = 105, zoom = 4) 
  })
  
  bins <- c(0,2,4,6,8,10)
  
  observe({
    #set bin and color category
    pal <- colorBin("YlOrRd", domain = df_subset()$num_sources, bins = bins, na.color = "transparent")
    
    #set text popup
    mytext=paste("Country: ", df_subset()$NAME,"<br/>", "# of Sources: ", df_subset()$num_sources) %>%
      lapply(htmltools::HTML)
    
    #add data
    leafletProxy("mymap", data = world_spdf) %>%
                   clearShapes() %>% 
                   addPolygons(
                     fillColor = ~pal(df_subset()$num_sources),
                     weight = 1.5,
                     opacity = 1,
                     color = "grey",
                     dashArray = "",
                     fillOpacity = .7,
                     layerId = df_subset()$id,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "black",
                       dashArray = "",
                       fillOpacity = .7,
                       bringToFront = TRUE),
                     label = mytext,
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto")
                     )
    })
  
  #create legend
  observe({
    proxy <- leafletProxy("mymap", data = df_subset())
    proxy %>% clearControls()
    proxy %>% 
      addLegend(
        pal <- colorBin("YlOrRd", domain = df_subset()$num_sources, bins = bins, na.color = "transparent"), 
        values = ~df_subset()$num_sources, 
        opacity = 0.7, 
        title = "Number of sources", 
        position = "bottomleft"
      )
  })
  #create empty reactive value
  rv <- reactiveValues(data = NULL)
  
  #output plot based on click
  observeEvent(input$mymap_shape_click, {
    event <- input$mymap_shape_click
    
    #get country name based on ID
    name <- df_subset()$NAME[df_subset()$id == event$id]
    
    #filter data based on country and pollutant
    rv$tb <- pm %>% 
      select(country, parameter, value, local) %>% 
      filter(country == name, parameter == input$pollutant)
    output$plot <- renderPlot({
      rv$tb %>% ggplot() +
        geom_point(aes(local, value))
    })
    output$country <- renderText({
      name
    })
    
    if (name %in% rv$tb$country & input$pollutant %in% rv$tb$parameter){
      output$plt <- renderUI({
        plotOutput("plot")
      })
      output$nodata <- NULL
    } else {
      output$nodata <- renderText({
        "No data available"
      })
      output$plt <- NULL
    }
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