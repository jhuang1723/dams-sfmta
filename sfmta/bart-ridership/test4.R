# install.packages("tidyverse")
# install.packages("shiny")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("lubridate")
#install.packages("dbplyr")
# trying app stuff here
library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(lubridate)
library(dbplyr)

root_dir = "/Users/yagishinnosuke/Documents/2023-2024 Stanford/DAMS/SFMTA_prototype/dams-sfmta-main/sfmta/bart-ridership/BART_System_2020/"
all_routes_data <- st_read(paste0(root_dir, "Layers.kml") , layer="Routes", quiet = TRUE)
all_routes <- st_zm(all_routes_data, drop = T, what = "ZM")

ridership_numbers <- read.csv(paste0(root_dir, "all-stations.csv"))
routes_ridership <- merge(all_routes, ridership_numbers, by="Name")

dummy <- read.csv(paste0(root_dir, "dummy.csv"))
dummy_stations <- c("RM", "EN", "EP", "NB", "BK")
dummy_stations_two <- c("EN", "EP", "NB", "BK")

# Load station data
station_data <- st_read(paste0(root_dir, "BART_Station.kml"), quiet = TRUE)

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
      selectInput("start", "Select Routes:",
                  choices = dummy_stations
      ),
      selectInput("end", "Select Routes:",
                  choices = dummy_stations_two
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
        selected_cols = select(., all_of(og_date_columns)),
        total = rowSums(st_drop_geometry(selected_cols))
      ) %>%
      select("Name", "geometry", total)
    print(selected_data)
    View(selected_data)
    #####################
    
    ####################
    return(selected_data)
  })
  
  ridership_nums_limited <- reactive({
    start_station <- input$start
    end_station <- input$end
    input_string <- paste(start_station, end_station, sep = "-")
    print(input_string)
    
    #Get selected route
    split_names <- sub("-.*", "", dummy$Name) # probably separate columns honestly
    dummy_selected <- dummy[grep(input$start, split_names), ]
    print(dummy_selected) # gets all routes that start with input$start
    
    #Date
    start_date <- as.Date("2023-04-01")
    end_date <- as.Date("2023-06-01")
    exclude_columns <- c("Name", "Routes")
    date_cols <- setdiff(colnames(dummy_selected), exclude_columns)
    date_cols <- mdy(substring(date_cols, 2))
    # Change the column names in dummy_selected
    colnames(dummy_selected)[which(!names(dummy_selected) %in% exclude_columns)] <- as.character(date_cols)
    # format: datetime
    selected_columns <- date_cols[date_cols >= start_date & date_cols <= end_date]
    print(selected_columns)
    
    split_names <- sub(".*-(.*)", "\\1", dummy_selected$Name)  #yeah definitely reformat data lol
    select_row <- dummy_selected[grep(input$end, split_names), ]
    particular_route <- lapply(select_row$Routes, function(route) strsplit(route, "~"))
    
    print("route")
    particular_route_true <- particular_route[[1]][[1]]
    print(particular_route_true)
    
    num_rows <- length(particular_route_true)
    # Create a DataFrame
    df <- data.frame(Name = rep(input_string, num_rows), total = NA, geometry = NA)
    print(df)
    for (i in c(1:num_rows)){
      df$Name[i] <- unlist(all_routes[all_routes$Name == particular_route_true[i], 1])
    }
    # for (i in c(1:num_rows)){
    #   df$geometry[i] <- all_routes[all_routes$Name == particular_route_true[i], 3]
    # }
    print(df)
    
    # Inner join the data frames based on the "Name" column
    merged_sf <- inner_join(all_routes, df,by = "Name")
    
    # Display the merged sf object
    print(merged_sf)
    
    merged_sf$geometry <- merged_sf$geometry.x
    print(merged_sf)
    
    # Delete the "Score" column
    merged_sf$geometry.x <- NULL
    merged_sf$geometry.y <- NULL
    merged_sf$Description <- NULL
    # Display the modified dataframe
    merged_sf$total <- 3000
    print(merged_sf)
    # Create an sf object from the dataframe
    sf_object <- st_as_sf(merged_sf, wkt = "geometry")
    # Display the sf object
    print(sf_object)
    View(sf_object)
    return(sf_object)
  })
  
  output$map = renderLeaflet({
    map <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    pal <- colorNumeric(
      palette = "Blues",
      domain = ridership_nums()$total
    )
    
    map %>%
      addPolylines(data = ridership_nums_limited(),
                   color = ~pal(total),
                   weight = 5, opacity = 1.0, stroke = TRUE) %>%
      addLegend("bottomright", pal = pal, values = ridership_nums()$total,
                title = "Ridership",
                opacity = 1
      ) %>% addCircleMarkers(
        data = station_data, radius = 2, label = station_data$Name)
  })
}

shinyApp(ui, server)
