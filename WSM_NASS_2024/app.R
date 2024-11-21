library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Read the CSV file
initial_data <- read_csv("../NASS_DATA_AND_SCRIPTS/data/final_mapped_data/mapped_combineddata_2024_4.csv")

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
                  choices = c("Harvested Acres", "Yield", "Production", 
                              "Price P/U", "Value")),
      
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
      plotOutput("timePlot", height = "600px"),
      
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
                      selected = NULL)
  })
  
  # Filter data based on selections
  filtered_data <- reactive({
    initial_data %>%
      filter(
        `Crop Name` == input$crop,
        County %in% input$county,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
  })
  
  # Create the time series plot
  output$timePlot <- renderPlot({
    req(filtered_data())
    req(nrow(filtered_data()) > 0)
    
    # Create the plot
    ggplot(filtered_data(), aes(x = Year, y = !!sym(input$variable), 
                                color = County, group = County)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(
        title = paste("Historical Data\nCrop Category:", input$crop),
        x = "Year",
        y = paste0(input$variable, 
                   ifelse(input$variable == "Yield", " (Tonnes per acre)", "")),
        color = "County"
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      ) +
      scale_x_continuous(breaks = pretty_breaks(n = 10)) +
      scale_y_continuous(labels = comma)
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