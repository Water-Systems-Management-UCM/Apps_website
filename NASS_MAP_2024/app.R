library(shiny)
library(readr)
library(DT)
library(leaflet)
library(dplyr)
library(sf)
library(tigris)

# Specify the name of your CSV file here
csv_filename <- "../NASS_DATA_AND_SCRIPTS/data/final_mapped_data/mapped_combineddata_2024_4.csv"

# Read the CSV file once at startup
initial_data <- read_csv(csv_filename)

# Load California counties
options(tigris_use_cache = TRUE)
ca_counties <- counties(state = "CA", cb = TRUE)

ui <- fluidPage(
  titlePanel("NASS Data Map"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("crop", "Crop Category",
                  choices = sort(unique(initial_data$`Crop Name`))),  # Added sort() here
      
      selectInput("year", "Year",
                  choices = sort(unique(initial_data$Year[!is.na(initial_data$Year)]))),
      
      selectInput("variable", "Variable",
                  choices = c("Harvested Acres", "Yield", "Production", "Price P/U", "Value")),
      
      # Put download buttons in a horizontal layout
      fluidRow(
        column(5, downloadButton("downloadData", "Download")),
        column(5, downloadButton("downloadAllData", "Download All"))
      )
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px"),
      textOutput("debug")
    )
  )
)

server <- function(input, output, session) {
  # Filter data based on selections
  filtered_data <- reactive({
    initial_data %>%
      filter(`Crop Name` == input$crop,
             Year == input$year)
  })
  
  # Summarize data by county
  summarized_data <- reactive({
    filtered_data() %>%
      group_by(County) %>%
      summarise(Value = sum(as.numeric(!!sym(input$variable)), na.rm = TRUE))
  })
  
  # Create the map
  output$map <- renderLeaflet({
    req(summarized_data())
    
    # Join the data with county polygons
    ca_counties_data <- left_join(ca_counties, 
                                  summarized_data(), 
                                  by = c("NAME" = "County"))
    
    # Get value range for the selected variable
    valid_values <- ca_counties_data$Value[!is.na(ca_counties_data$Value)]
    
    if(length(valid_values) == 0) {
      return(leaflet(ca_counties) %>%
               addTiles() %>%
               addPolygons(fillColor = "gray",
                           weight = 2,
                           opacity = 1,
                           color = "white",
                           fillOpacity = 0.7) %>%
               setView(lng = -119.4179, lat = 36.7783, zoom = 6))
    }
    
    # Create color palette
    pal <- colorNumeric(
      palette = "Blues",
      domain = valid_values,
      na.color = "#CCCCCC"
    )
    
    # Create map
    leaflet(ca_counties_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Value),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(
          NAME, ": ", 
          ifelse(is.na(Value), 
                 "No data", 
                 formatC(Value, format = "f", big.mark = ",", digits = 2))
        )
      ) %>%
      addLegend(
        pal = pal,
        values = valid_values,
        opacity = 0.7,
        title = input$variable,
        position = "bottomright",
        na.label = "No data"
      ) %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 6)
  })
  
  # Add debug output
  output$debug <- renderText({
    req(filtered_data())
    paste("Records found:", nrow(filtered_data()))
  })
  
  # Download handler for selected data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("nass_data_", input$crop, "_", input$year, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for all data
  output$downloadAllData <- downloadHandler(
    filename = function() {
      paste("nass_data_all_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(initial_data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)