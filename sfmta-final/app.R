# install.packages("tidyverse")
# install.packages("shiny")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("lubridate")
# install.packages("plotly")
# install.packages("DT")
# trying app stuff here
library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(lubridate)
library(plotly)
library(DT)


all_routes_data <- st_read("data/Layers.kml", layer="Routes", quiet = TRUE)
all_routes <- st_zm(all_routes_data, drop = T, what = "ZM")

ridership_numbers <- read.csv("data/df_stations_all_counts.csv")
routes_ridership <- merge(all_routes, ridership_numbers, by="Name")

#dummy <- read.csv("/Users/yagishinnosuke/Documents/2023-2024 Stanford/DAMS/SFMTA_prototype/dams-sfmta-main/sfmta/bart-ridership/BART_System_2020/dummy.csv")
#dummy_stations <- c("-","RM", "EN", "EP", "NB", "BK")

dummy <- read.csv("data/all-paths-data-2.csv")
unique_elements <- unique(unlist(strsplit(dummy$Routes, split = "[~-]")))

dummy_stations <- c("-", unique_elements)

# Load station data
station_data <- st_read("data/BART_Station.kml", quiet = TRUE)

ui = fluidPage(
  
  titlePanel("SFMTA Map"),
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(
      sliderInput("date_range", "Date Range:",
                  min = as.Date("2018-05-01"),
                  max = as.Date("2023-12-01"),
                  value = c(as.Date("2018-04-01"), as.Date("2024-01-01")),
                  step = 1,
                  timeFormat = ("%b %Y"),
                  ticks = FALSE
      ),
      selectInput("start", "Select Routes:",
                  choices = dummy_stations
      ),
      selectInput("end", "Select Routes:",
                  choices = dummy_stations
      )
    ),
    mainPanel = mainPanel(
      leafletOutput(outputId = 'map', width = "100%", height = "600px"),
      DTOutput("table")
    )
  )
  
)

server = function(input, output){
  
  ridership_nums <- reactive({
    #Date
    start_date <- as.Date(input$date_range[1])
    start_date <- start_date - months(1)
    end_date <- as.Date(input$date_range[2])
    start_date <- start_date - months(1)
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
  
  ridership_nums_limited <- reactive({
    #Get station names
    start_station <- input$start
    end_station <- input$end
    input_string <- paste(start_station, end_station, sep = "-")
    string <- dummy[dummy$Name == input_string, 2]
    substrings <- unlist(strsplit(string, "~"))
    # Extract rows from df based on the first column values
    routes_ridership_selected <- routes_ridership[routes_ridership$Name %in% substrings, ]
    copied_columns <- dummy[dummy$Name == input_string, c(3:72)]
    routes_ridership_selected[, c(3:72)] <- copied_columns
    #print(routes_ridership_selected)
    
    #Date
    start_date <- as.Date(input$date_range[1])
    start_date <- start_date - months(1)
    end_date <- as.Date(input$date_range[2])
    exclude_columns <- c("Name", "Description", "geometry")
    date_cols <- setdiff(colnames(routes_ridership_selected), exclude_columns)
    date_cols <- mdy(substring(date_cols, 2))
    # format: datetime
    selected_columns <- date_cols[date_cols >= start_date & date_cols <= end_date]
    # need to convert back to "X04.01.2023" form
    og_date_columns <- format(selected_columns, format = "X%m.%d.%Y")
    
    total_in_period <- rowSums(st_drop_geometry(routes_ridership_selected[, og_date_columns]))
    routes_ridership_selected <- merge(routes_ridership_selected, total_in_period)
    selected_data <- routes_ridership_selected %>%
      mutate(
        selected_cols = select(., og_date_columns),
        total = rowSums(st_drop_geometry(selected_cols))
      ) %>%
      select("Name", "geometry", total)
    return(selected_data)
  })
  
  #particular_route <- reactive(as.character(input$dropdown_menu))
  start <- reactive(as.character(input$start))
  end <- reactive(as.character(input$end))
  
  output$table <- renderDT({
    # Load and return the CSV data
    datatable(read.csv("data/Station_names.csv"), options = list(pageLength = 5)) # Adjust 'pageLength' as needed
  })
  
  output$map = renderLeaflet({
    map <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    pal <- colorNumeric(
      palette = "Blues",
      domain = ridership_nums()$total
    )
    
    if (start() == "-" | end() == "-") {
      map %>%
        addPolylines(data = ridership_nums(),
                     color = ~pal(total),
                     weight = 5, opacity = 1.0, stroke = TRUE,
                     label = ~paste("Total Ridership:", total), # Add labels showing total ridership
                     labelOptions = labelOptions(noHide = F, direction = 'auto') # Options for the label appearance
        ) %>%
        addLegend("bottomright", pal = pal, values = ridership_nums()$total,
                  title = "Ridership",
                  opacity = 1
        ) %>% addCircleMarkers(
          data = station_data, radius = 2, label = station_data$Name)
    } else {
      map %>%
        addPolylines(
          data = ridership_nums_limited(),
          color = "red",
          weight = 5, opacity = 1.0, stroke = TRUE,
          label = ~paste("Total Ridership:", total), # Add labels showing total ridership
          labelOptions = labelOptions(noHide = F, direction = 'auto') # Options for the label appearance
        ) %>%
        addCircleMarkers(
          data = station_data, radius = 2, label = ~Name
        )
    }
  })
}

shinyApp(ui, server)
