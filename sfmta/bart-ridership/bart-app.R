library(sf)
library(leaflet)

# Read and process the KML data
all_routes_data <- st_read("bart-ridership/BART_System_2020/Layers.kml", layer="Routes", quiet = TRUE)
stations_sf <- st_read("bart-ridership/BART_System_2020/doc.kml", layer="BART Station", quiet = TRUE)
#routes_data <- st_read("bart-ridership/BART_System_2020/doc.kml", layer="BART Track")
#routes_sf <- st_zm(routes_sf, drop = T, what = "ZM")
all_routes <- st_zm(all_routes_data, drop = T, what = "ZM")
# Create a Leaflet map
my_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = stations_sf, radius = 8, color = "red", fill = TRUE, fillOpacity = 0.7) %>%
  #addPolylines(data = routes_sf, color = "blue", weight = 2)
  addPolylines(data = all_routes, color = "blue", weight = 2)
  #addPolylines(data = routes_sf, color = "blue", weight = 2)%>% 
  #addPolylines(data = routes_sf[2,], color = "green", weight = 2)
# Display the map
my_map

# cleaner version to use: just data visualization
# line strings of individual routes
all_routes_data <- st_read("bart-ridership/BART_System_2020/Layers.kml", layer="Routes", quiet = TRUE)
all_routes <- st_zm(all_routes_data, drop = T, what = "ZM")
ridership_numbers <- read.csv("bart-ridership/BART_System_2020/all-stations.csv")
# connecting stations I made
routes_ridership <- merge(all_routes, ridership_numbers, by="Name")

#c("#6585bf", "#527bc7", "#3c5e9e", "#33518a", "#1b2c4d")
my_map <- leaflet() %>%
  addTiles(options = tileOptions(opacity = 0.9)) %>%
  addPolylines(data = routes_ridership, 
               color = ~colorNumeric("Blues", 
                                     routes_ridership$April.2023)(April.2023),
               weight = 5, opacity = 1.0)

my_map

# trying app stuff here
library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(lubridate)

all_routes_data <- st_read("bart-ridership/BART_System_2020/Layers.kml", layer="Routes", quiet = TRUE)
all_routes <- st_zm(all_routes_data, drop = T, what = "ZM")

ridership_numbers <- read.csv("bart-ridership/BART_System_2020/all-stations.csv")
routes_ridership <- merge(all_routes, ridership_numbers, by="Name")

ui = fluidPage(
  
  titlePanel("SFMTA Map"),
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(
      sliderInput("date_range", "Date Range:",
                  min = as.Date("2023-04-01"),
                  max = as.Date("2023-06-01"),
                  value = c(as.Date("2023-04-01"), as.Date("2023-06-01")),
                  step = 1,
                  timeFormat = ("%b %Y"),
                  ticks = FALSE
      )
    ),
    mainPanel = mainPanel(
      leafletOutput(outputId = 'map')
    )
  )
  
)

server = function(input, output){
  
  ridership_nums <- reactive({
    start_date <- as.Date(input$date_range[1])
    end_date <- as.Date(input$date_range[2])
    exclude_columns <- c("Name", "Description", "geometry")
    date_cols <- setdiff(colnames(routes_ridership), exclude_columns)
    date_cols <- mdy(substring(date_cols, 2))
    # format: datetime
    selected_columns <- date_cols[date_cols >= start_date & date_cols <= end_date]
    # need to convert back to "X04.01.2023" form
    og_date_columns <- format(selected_columns, format = "X%m.%d.%Y")
    
    total_in_period <- rowSums(st_drop_geometry(routes_ridership[, og_date_columns]))
    routes_ridership <- merge(routes_ridership, total_in_period)
    selected_data <- routes_ridership %>%
      mutate(
        selected_cols = select(., og_date_columns),
        total = rowSums(st_drop_geometry(selected_cols))
      ) %>%
      select("Name", "geometry", total)
    return(selected_data)
    
  })
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles(options = tileOptions(opacity = 0.9)) %>%
      addPolylines(data = ridership_nums(), 
                   color = ~colorNumeric("Blues", 
                                         total)(total),
                   weight = 5, opacity = 1.0)
  })
}

shinyApp(ui, server)

