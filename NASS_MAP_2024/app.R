library(shiny)
library(readr)
library(DT)
library(leaflet)
library(dplyr)
library(sf)
library(tigris)

# Specify the name of your CSV file here
csv_filename <- "final_mapped_data_without_duplicates.csv"

# Read the CSV file once at startup
initial_data <- read_csv(csv_filename)

# Load California counties
options(tigris_use_cache = TRUE)
ca_counties <- counties(state = "CA", cb = TRUE)

ui <- fluidPage(
  titlePanel("NASS Data Map - With Inflation Adjustment"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("crop", "Crop Category",
                  choices = sort(unique(initial_data$`Crop Name`))),
      
      selectInput("commodity_detail", "Commodity Detail",
                  choices = c("All Commodities" = "all"),
                  selected = "all"),
      
      selectInput("year", "Year",
                  choices = sort(unique(initial_data$Year[!is.na(initial_data$Year)]))),
      
      # Create a radio button for choosing between original and adjusted values
      radioButtons("adjust_type", "Data Type:",
                   choices = c("Original Values" = "original", 
                               "Inflation-Adjusted Values (2024 Base)" = "adjusted"),
                   selected = "original"),
      
      # Variable selection - will be updated based on adjust_type
      selectInput("variable", "Variable",
                  choices = c("Total Area", "Yield", "Production", "Price P/U", "Value")),
      
      # Information about the CPI adjustment
      conditionalPanel(
        condition = "input.adjust_type == 'adjusted'",
        helpText("Values are adjusted for inflation using Consumer Price Index (CPI) with 2024 as the base year.")
      ),
      
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
  
  # Update commodity detail choices based on selected crop
  observe({
    req(input$crop)
    
    # Get unique commodity codes for the selected crop with substantial data
    commodity_options <- initial_data %>%
      filter(`Crop Name` == input$crop) %>%
      group_by(`Commodity Code`) %>%
      summarise(
        total_acres = sum(as.numeric(`Harvested Acres`), na.rm = TRUE),
        total_value = sum(as.numeric(Value), na.rm = TRUE),
        county_count = n_distinct(County),
        year_count = n_distinct(Year),
        .groups = "drop"
      ) %>%
      # Only include commodities with meaningful data (>500 acres OR >$50k value) AND multiple data points
      filter((total_acres > 500 | total_value > 50000) & county_count >= 2 & year_count >= 2) %>%
      arrange(`Commodity Code`)
    
    # Create choices list
    if(nrow(commodity_options) > 1) {
      choices_list <- c("All Commodities" = "all")
      
      # Add specific commodity codes with descriptive names
      for(i in 1:nrow(commodity_options)) {
        code <- as.character(commodity_options$`Commodity Code`[i])
        code_numeric <- as.numeric(code)
        # Create descriptive names based on common patterns
        description <- case_when(
          code_numeric == 181999 ~ "Hay",
          code_numeric == 172119 ~ "Seed", 
          code_numeric == 151999 ~ "Lint",
          code_numeric == 171519 ~ "Seed",
          TRUE ~ paste("Code", code_numeric)
        )
        choices_list <- c(choices_list, setNames(code, paste(input$crop, "-", description)))
      }
    } else {
      choices_list <- c("All Commodities" = "all")
    }
    
    updateSelectInput(session, "commodity_detail",
                      choices = choices_list,
                      selected = "all")
  })
  
  # Update variable choices based on adjustment type
  observe({
    if (input$adjust_type == "original") {
      variable_choices <- c("Total Area", "Yield", "Production", "Price P/U", "Value")
    } else {
      variable_choices <- c("Total Area", "Yield", "Production", "Adjusted Price" = "Adjusted Price", 
                            "Adjusted Total Production Value" = "Adjusted Total Production Value",
                            "Adjusted Gross Revenue" = "Adjusted Gross Revenue")
    }
    
    # Update the selectInput with new choices
    updateSelectInput(session, "variable", choices = variable_choices)
  })
  
  # Filter data based on selections
  filtered_data <- reactive({
    data <- initial_data %>%
      filter(`Crop Name` == input$crop,
             Year == input$year)
    
    # Filter by commodity code if specific one is selected
    if(input$commodity_detail != "all") {
      data <- data %>%
        filter(as.character(`Commodity Code`) == as.character(input$commodity_detail))
    }
    
    # Rename column for display purposes while preserving original data
    if ("Harvested Acres" %in% colnames(data)) {
      data <- data %>% 
        rename("Total Area" = "Harvested Acres")
    }
    
    return(data)
  })
  
  # Summarize data by county
  summarized_data <- reactive({
    # Handle the variable name mapping
    var_to_use <- input$variable
    
    # If we're looking for Total Area but the data has Harvested Acres
    if (var_to_use == "Total Area" && !"Total Area" %in% colnames(filtered_data()) && "Harvested Acres" %in% colnames(initial_data)) {
      var_to_use <- "Harvested Acres"
    }
    
    # Summarize by county - aggregate if "all" commodities selected
    if(input$commodity_detail == "all") {
      # Aggregate multiple commodity codes for same crop/county/year
      filtered_data() %>%
        group_by(County) %>%
        summarise(Value = sum(as.numeric(!!sym(var_to_use)), na.rm = TRUE))
    } else {
      # Use data as-is for specific commodity selection
      filtered_data() %>%
        group_by(County) %>%
        summarise(Value = sum(as.numeric(!!sym(var_to_use)), na.rm = TRUE))
    }
  })
  
  # Function to get the appropriate unit for each variable
  get_variable_unit <- function(variable_name) {
    case_when(
      variable_name == "Total Area" ~ "Acres",
      variable_name == "Yield" ~ "Tons/Acre",
      variable_name == "Production" ~ "Tons",
      variable_name == "Price P/U" ~ "USD/Ton",
      variable_name == "Value" ~ "Thousand USD",
      variable_name == "Adjusted Price" ~ "USD (2024)/Ton",
      variable_name == "Adjusted Total Production Value" ~ "Thousand USD (2024)",
      variable_name == "Adjusted Gross Revenue" ~ "USD (2024)/Acre",
      TRUE ~ ""
    )
  }
  
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
    
    # Get the appropriate unit for the variable
    variable_unit <- get_variable_unit(input$variable)
    
    # Set the legend title to only show units
    legend_title <- variable_unit
    
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
                 formatC(Value, format = "f", big.mark = ",", digits = 2)),
          " ", variable_unit
        )
      ) %>%
      addLegend(
        pal = pal,
        values = valid_values,
        opacity = 0.7,
        title = legend_title,
        position = "bottomright",
        na.label = "No data"
      ) %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 6)
  })
  
  # Add debug output
  output$debug <- renderText({
    req(filtered_data())
    if (input$adjust_type == "adjusted") {
      paste("Records found:", nrow(filtered_data()), "| Showing inflation-adjusted values (2024 base year)")
    } else {
      paste("Records found:", nrow(filtered_data()), "| Showing original values")
    }
  })
  
  # Download handler for selected data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("nass_data_", input$crop, "_", input$year, "_", 
            ifelse(input$adjust_type == "adjusted", "adjusted", "original"), ".csv", sep = "")
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