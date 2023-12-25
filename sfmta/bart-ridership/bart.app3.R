# install.packages("tidyverse")
# install.packages("shiny")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("lubridate")
# trying app stuff here
library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(lubridate)


all_routes_data <- st_read("/Users/yagishinnosuke/Documents/2023-2024 Stanford/DAMS/SFMTA_prototype/dams-sfmta-main/sfmta/bart-ridership/BART_System_2020/Layers.kml", layer="Routes", quiet = TRUE)
all_routes <- st_zm(all_routes_data, drop = T, what = "ZM")

ridership_numbers <- read.csv("/Users/yagishinnosuke/Documents/2023-2024 Stanford/DAMS/SFMTA_prototype/dams-sfmta-main/sfmta/bart-ridership/BART_System_2020/all-stations.csv")
routes_ridership <- merge(all_routes, ridership_numbers, by="Name")

# Load station data
station_data <- st_read("/Users/yagishinnosuke/Documents/2023-2024 Stanford/DAMS/SFMTA_prototype/dams-sfmta-main/sfmta/bart-ridership/BART_System_2020/BART_Station.kml", quiet = TRUE)

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
      ),
      selectInput("dropdown_menu", "Select Routes:",
                  choices = c("All", all_routes$Name)
      )
    ),
    mainPanel = mainPanel(
      leafletOutput(outputId = 'map', width = "100%", height = "600px")
    )
  )
  
)
  

server = function(input, output){
  
  ridership_nums <- reactive({
    #Date
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
  
  particular_route <- reactive(as.character(input$dropdown_menu))
  
  output$map = renderLeaflet({
    map <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    pal <- colorNumeric(
      palette = "Blues",
      domain = ridership_nums()$total
    )
    
    if (particular_route() == "All") {
      map %>%
        addPolylines(data = ridership_nums(),
                     color = ~pal(total),
                     weight = 5, opacity = 1.0, stroke = TRUE) %>%
        addLegend("bottomright", pal = pal, values = ridership_nums()$total,
                  title = "Ridership",
                  opacity = 1
        ) %>% addCircleMarkers(
          data = station_data, radius = 2, label = station_data$Name)
    } else {
      map %>% addPolylines(data = ridership_nums()[ridership_nums()$Name == particular_route(), ],
                           color = ~pal(total),
                           weight = 5, opacity = 1.0, stroke = TRUE) %>%
        addLegend("bottomright", pal = pal, values = ridership_nums()$total,
                  title = "Ridership",
                  opacity = 1
        ) %>% addCircleMarkers(
          data = station_data, radius = 2, label = station_data$Name)
    }
  })
}

shinyApp(ui, server)