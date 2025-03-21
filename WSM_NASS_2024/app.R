library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(tidyr)

# Read the CSV file with adjusted values
initial_data <- read_csv("../NASS_DATA_AND_SCRIPTS/data/cpi_adjusted_data/final_mapped_data_with_cpi.csv")

ui <- fluidPage(
  titlePanel("Historical Crop Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("crop", "Choose Crop",
                  choices = sort(unique(initial_data$`Crop Name`))),
      
      selectInput("county", "Choose County",
                  choices = NULL,
                  multiple = TRUE),
      
      selectInput("variable", "Variable",
                  choices = c("Harvested Acres", "Yield", 
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
      var_choices <- c("Harvested Acres", "Yield", 
                       "Adjusted Price", "Adjusted Total Production Value", 
                       "Adjusted Gross Revenue")
    } else {
      var_choices <- c("Harvested Acres", "Yield", 
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
    
    initial_data %>%
      filter(
        `Crop Name` == input$crop,
        County %in% input$county,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      ) %>%
      arrange(Year, County) %>%  # Ensure data is properly ordered
      drop_na(!!sym(input$variable))  # Remove NA values for selected variable
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
    } else if (variable == "Harvested Acres") {
      return("Harvested Acres")
    } else {
      return(variable)
    }
  })
  
  # Create the interactive time series plot
  output$timePlot <- renderPlotly({
    req(filtered_data())
    req(nrow(filtered_data()) > 0)
    
    # Create the base ggplot
    p <- ggplot(filtered_data(), 
                aes(x = Year, 
                    y = !!sym(input$variable), 
                    color = County, 
                    group = County,
                    text = paste("County:", County,
                                 "\nYear:", Year,
                                 "\n", input$variable, ":", round(as.numeric(!!sym(input$variable)), 2)))) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5, na.rm = TRUE) +
      theme_minimal() +
      labs(
        title = paste("Historical Data\nCrop Category:", input$crop),
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
      scale_y_continuous(labels = comma)
    
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