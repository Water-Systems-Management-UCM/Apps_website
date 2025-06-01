library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(tidyr)

# Read the CSV file with adjusted values and explicitly convert numeric columns
initial_data <- read_csv("final_mapped_data_without_duplicates.csv", 
                         col_types = cols(.default = "?"))

# Ensure numeric columns are properly parsed
numeric_columns <- c("Year", "Harvested Acres", "Yield", "Price P/U", "Adjusted Price", 
                     "Value", "Adjusted Total Production Value", "Gross Revenue", 
                     "Adjusted Gross Revenue", "Production")

for(col in numeric_columns) {
  if(col %in% names(initial_data)) {
    initial_data[[col]] <- as.numeric(initial_data[[col]])
  }
}

ui <- fluidPage(
  titlePanel("Historical Crop Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("crop", "Choose Crop",
                  choices = sort(unique(initial_data$`Crop Name`))),
      
      selectInput("commodity_detail", "Commodity Detail",
                  choices = c("All Commodities" = "all"),
                  selected = "all"),
      
      selectInput("county", "Choose County",
                  choices = NULL,
                  multiple = TRUE),
      
      selectInput("variable", "Variable",
                  choices = c("Total Area", "Yield", 
                              "Price P/U", "Adjusted Price", 
                              "Value", "Adjusted Total Production Value", 
                              "Gross Revenue", "Adjusted Gross Revenue")),
      
      checkboxInput("use_adjusted", "Use CPI-Adjusted Values", value = TRUE),
      
      sliderInput("year_range", "Year Range",
                  min = min(initial_data$Year, na.rm = TRUE),
                  max = max(initial_data$Year, na.rm = TRUE),
                  value = c(min(initial_data$Year, na.rm = TRUE),
                            max(initial_data$Year, na.rm = TRUE)),
                  step = 1,
                  sep = ""),
      
      # Download buttons in a horizontal layout
      fluidRow(
        column(5, downloadButton("download", "Download")),
        column(5, downloadButton("downloadAll", "Download All"))
      )
    ),
    
    mainPanel(
      plotlyOutput("timePlot", height = "600px"),
      
      # Footer text
      tags$div(
        style = "font-size: 12px; margin-top: 20px;",
        tags$p("Data source: NASS/Ag-commissioner's data using main crop categories"),
        tags$p("Monetary values were adjusted to 2022 using consumer price index"),
        tags$p("Reported price, yield and gross revenue are the weighted mean by acreage"),
        tags$p("Developed by Water Systems Management Lab UC Merced")
      )
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
  
  # Update county choices based on selected crop
  observe({
    crop_counties <- initial_data %>%
      filter(`Crop Name` == input$crop) %>%
      pull(County) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "county",
                      choices = crop_counties,
                      selected = crop_counties[1])  # Select first county by default
  })
  
  # Update variable choices based on whether adjusted values are selected
  observe({
    if (input$use_adjusted) {
      var_choices <- c("Total Area", "Yield", 
                       "Adjusted Price", "Adjusted Total Production Value", 
                       "Adjusted Gross Revenue")
    } else {
      var_choices <- c("Total Area", "Yield", 
                       "Price P/U", "Value", "Gross Revenue")
    }
    
    # Update without changing selection if possible
    current_var <- input$variable
    if (!current_var %in% var_choices) {
      # If current selection is Price P/U, switch to Adjusted Price (and similar for other variables)
      if (current_var == "Price P/U") {
        current_var <- "Adjusted Price"
      } else if (current_var == "Value") {
        current_var <- "Adjusted Total Production Value"
      } else if (current_var == "Gross Revenue") {
        current_var <- "Adjusted Gross Revenue"
      } else if (current_var == "Adjusted Price") {
        current_var <- "Price P/U"
      } else if (current_var == "Adjusted Total Production Value") {
        current_var <- "Value"
      } else if (current_var == "Adjusted Gross Revenue") {
        current_var <- "Gross Revenue"
      } else {
        current_var <- var_choices[1]
      }
    }
    
    updateSelectInput(session, "variable",
                      choices = var_choices,
                      selected = current_var)
  })
  
  # Filter data based on selections
  filtered_data <- reactive({
    req(input$county)
    
    # First get the raw data
    raw_data <- initial_data %>%
      filter(
        `Crop Name` == input$crop,
        County %in% input$county,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
    
    # Filter by commodity code if specific one is selected
    if(input$commodity_detail != "all") {
      raw_data <- raw_data %>%
        filter(as.character(`Commodity Code`) == as.character(input$commodity_detail))
    }
    
    # Aggregate multiple commodity codes for the same crop/county/year (only if "all" is selected)
    if(input$commodity_detail == "all") {
      # Sum extensive variables (acres, production, value) and calculate weighted averages for intensive variables (yield, price)
      aggregated_data <- raw_data %>%
        group_by(`Crop Name`, County, Year) %>%
        summarise(
          `Harvested Acres` = sum(as.numeric(`Harvested Acres`), na.rm = TRUE),
          Production = sum(as.numeric(Production), na.rm = TRUE),
          Value = sum(as.numeric(Value), na.rm = TRUE),
          `Adjusted Total Production Value` = sum(as.numeric(`Adjusted Total Production Value`), na.rm = TRUE),
          # Weighted averages for intensive variables
          Yield = ifelse(sum(as.numeric(`Harvested Acres`), na.rm = TRUE) > 0,
                        sum(as.numeric(Yield) * as.numeric(`Harvested Acres`), na.rm = TRUE) / sum(as.numeric(`Harvested Acres`), na.rm = TRUE),
                        0),
          `Price P/U` = ifelse(sum(as.numeric(Production), na.rm = TRUE) > 0,
                              sum(as.numeric(`Price P/U`) * as.numeric(Production), na.rm = TRUE) / sum(as.numeric(Production), na.rm = TRUE),
                              0),
          `Adjusted Price` = ifelse(sum(as.numeric(Production), na.rm = TRUE) > 0,
                                   sum(as.numeric(`Adjusted Price`) * as.numeric(Production), na.rm = TRUE) / sum(as.numeric(Production), na.rm = TRUE),
                                   0),
          `Gross Revenue` = ifelse(sum(as.numeric(`Harvested Acres`), na.rm = TRUE) > 0,
                                  sum(as.numeric(`Gross Revenue`) * as.numeric(`Harvested Acres`), na.rm = TRUE) / sum(as.numeric(`Harvested Acres`), na.rm = TRUE),
                                  0),
          `Adjusted Gross Revenue` = ifelse(sum(as.numeric(`Harvested Acres`), na.rm = TRUE) > 0,
                                           sum(as.numeric(`Adjusted Gross Revenue`) * as.numeric(`Harvested Acres`), na.rm = TRUE) / sum(as.numeric(`Harvested Acres`), na.rm = TRUE),
                                           0),
          .groups = "drop"
        ) %>%
        arrange(Year, County)
    } else {
      # Use raw data for specific commodity code selection
      aggregated_data <- raw_data %>%
        arrange(Year, County)
    }
    
    # Replace field name for visualization purposes
    if(input$variable == "Total Area") {
      aggregated_data <- aggregated_data %>% 
        rename(`Total Area` = `Harvested Acres`)
    }
    
    # Make sure all variables are numeric
    if(input$variable != "Total Area") {
      aggregated_data <- aggregated_data %>%
        mutate(!!sym(input$variable) := as.numeric(!!sym(input$variable)))
    } else {
      aggregated_data <- aggregated_data %>%
        mutate(`Total Area` = as.numeric(`Total Area`))
    }
    
    aggregated_data %>%
      drop_na(!!sym(input$variable)) %>%  # Remove NA values for selected variable
      filter(!!sym(input$variable) > 0)   # Remove zero values that make graphs look weird
  })
  
  # Get the appropriate y-axis label based on selected variable
  y_label <- reactive({
    variable <- input$variable
    
    if (variable == "Yield") {
      return("Yield (Tonnes per acre)")
    } else if (variable %in% c("Price P/U", "Adjusted Price")) {
      return(paste0(variable, " (USD)"))
    } else if (variable %in% c("Value", "Adjusted Total Production Value")) {
      if (variable == "Adjusted Total Production Value") {
        return("Total Production Value (Million USD, 2022)")
      } else {
        return("Value (USD)")
      }
    } else if (variable %in% c("Gross Revenue", "Adjusted Gross Revenue")) {
      if (variable == "Adjusted Gross Revenue") {
        return("Gross Revenue (USD, 2022)")
      } else {
        return("Gross Revenue (USD)")
      }
    } else if (variable == "Total Area") {
      return("Total Area")
    } else {
      return(variable)
    }
  })
  
  # Create the interactive time series plot
  output$timePlot <- renderPlotly({
    req(filtered_data())
    
    # Debugging info
    print(paste("Crop:", input$crop))
    print(paste("Commodity:", input$commodity_detail))
    print(paste("Counties:", paste(input$county, collapse=", ")))
    print(paste("Year range:", paste(input$year_range, collapse="-")))
    print(paste("Variable:", input$variable))
    print(paste("Filtered data rows:", nrow(filtered_data())))
    
    if(nrow(filtered_data()) == 0) {
      # Return empty plot with message
      return(ggplotly(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters", size = 6) +
        theme_void() + 
        xlim(0, 1) + ylim(0, 1)))
    }
    
    # Create the base ggplot
    p <- ggplot(filtered_data(), 
                aes(x = Year, 
                    y = !!sym(input$variable), 
                    color = County, 
                    group = County,
                    text = paste("County:", County,
                                 "\nYear:", Year,
                                 "\n", input$variable, ":", 
                                 format(round(as.numeric(!!sym(input$variable)), 2), 
                                        big.mark = ",")))) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5, na.rm = TRUE) +
      theme_minimal() +
      labs(
        title = paste("Historical Data\nCrop Category:", input$crop, 
                     if(input$commodity_detail != "all") paste("- Commodity Code:", input$commodity_detail) else ""),
        subtitle = if(input$use_adjusted) "Values adjusted to 2022 dollars using CPI" else "Original values (not adjusted for inflation)",
        x = "Year",
        y = y_label(),
        color = "County"
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      ) +
      scale_x_continuous(breaks = pretty_breaks(n = 10)) +
      scale_y_continuous(labels = comma_format())  # Use comma_format() instead of comma
    
    # Convert to plotly and customize tooltip
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        showlegend = TRUE
      )
  })
  
  # Download handler for filtered data
  output$download <- downloadHandler(
    filename = function() {
      paste0("crop_data_", input$crop, "_", 
             paste(input$county, collapse = "_"), "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for all data
  output$downloadAll <- downloadHandler(
    filename = function() {
      paste0("all_crop_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(initial_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)