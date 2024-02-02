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

root_dir = "~/projects/dams-sfmta/sfmta/bart-ridership/BART_System_2020/"
all_routes_data <- st_read(paste0(root_dir, "Layers.kml") , layer="Routes", quiet = TRUE)
all_routes <- st_zm(all_routes_data, drop = T, what = "ZM")

ridership_numbers <- read.csv(paste0(root_dir, "all-stations.csv"))
routes_ridership <- merge(all_routes, ridership_numbers, by="Name")

dummy <- read.csv(paste0(root_dir, "dummy.csv"))
dummy_stations <- c("RM", "EN", "EP", "NB", "BK")
dummy_stations_two <- c("EN", "EP", "NB", "BK")

# Load station data
# station_data <- st_read(paste0(root_dir, "BART_Station.kml"), quiet = TRUE)

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
  
  start_station <- reactive(as.character(input$start))
  end_station <- reactive(as.character(input$end))
  
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
    return(selected_data)
  })
  
  ridership_nums_limited <- reactive({
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
    og_date_columns <- selected_columns
    
    split_names <- sub(".*-(.*)", "\\1", dummy_selected$Name)  #yeah definitely reformat data lol
    select_row <- dummy_selected[grep(input$end, split_names), ]
    particular_route <- lapply(select_row$Routes, function(route) strsplit(route, "~"))

    # START CODE REVIEW FROM HERE
    #updated_geometry <- all_routes[all_routes$Name==particular_routes[[1]][1], 3]
    print("route")
    print(particular_route)
    for (i in c(2:length(particular_routes))){
      updated_geometry <- st_union(updated_geometry, all_routes[all_routes$Name==particular_routes[[1]][i], 3])
    }
    total_in_period <- 0
    for (i in c(1:length(og_date_columns))){
      print(i)
      name <- format(og_date_columns[i], "%Y-%m-%d")
      print(name)
      total_in_period <- total_in_period + dummy_selected[, name]
    }
    # Convert geometry to a valid type
    combined_data <- merge(dummy_selected, updated_geometry)
    combined_data <- merge(combined_data, total_in_period)
    print(combined_data$geometry)
    combined_data <- combined_data %>%
      mutate(
        selected_cols = select(., as.character(og_date_columns)),
        total = rowSums(st_drop_geometry(selected_cols))
      ) %>%
      select("Name", "geometry", total)
    print(unique(st_geometry_type(combined_data$geometry)))
    combined_data<- st_as_sf(combined_data, wkt = "geometry", crs = 4326)
    print(combined_data) # NOTHING HERE -- FIX THIS PART
    return(combined_data)
  })
  
  output$map = renderLeaflet({
    
    map <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    pal <- colorNumeric(
      palette = "Blues",
      #domain = ridership_nums_limited()$total
      domain = if (nrow(ridership_nums_limited()) > 0) {
        ridership_nums_limited()$total
      } else {
    # Provide a default value or handle the case when there's no data
        print("no data...")
      }
    )
    # map %>%
    #   addPolylines(data = ridership_nums(),
    #                color = ~pal(total),
    #                weight = 5, opacity = 1.0, stroke = TRUE) %>%
    #   addLegend("bottomright", pal = pal, values = ridership_nums()$total,
    #             title = "Ridership",
    #             opacity = 1
    #   ) %>% addCircleMarkers(
    #     data = station_data, radius = 2, label = station_data$Name)
    
    map %>%
      addPolylines(data = ridership_nums_limited(),
      color = ~pal(ridership_nums_limited()),
      weight = 5, opacity = 1.0, stroke = TRUE) %>%
      addLegend("bottomright", pal = pal, values = ridership_nums()$total,
                title = "Ridership",
                opacity = 1
      ) #%>%
      #addCircleMarkers( # add back once you find station_data kml file
      #  data = station_data, radius = 2, label = station_data$Name)
  })
}

shinyApp(ui, server)

