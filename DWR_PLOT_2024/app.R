library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(janitor)
library(tidyr)

# Read and process data
csv_filename <- "mapped_combineddata_2024.csv"

initial_data <- tryCatch({
  # Read the data
  data <- read_csv(csv_filename, 
                   col_types = cols(
                     Year = col_double(),
                     `Commodity Code` = col_character(),
                     `Crop Name` = col_character(),
                     `County Code` = col_character(),
                     County = col_character(),
                     `Harvested Acres` = col_double(),
                     Yield = col_double(),
                     Production = col_double(),
                     `Price P/U` = col_double(),
                     Unit = col_character(),
                     Value = col_double()
                   ),
                   locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE)
  
  # Fix missing crop names and clean data
  data_fixed <- data %>%
    group_by(`Commodity Code`) %>%
    fill(`Crop Name`, .direction = "downup") %>%
    ungroup() %>%
    janitor::clean_names() %>%
    filter(!grepl("^[0-9]+$", crop_name),
           !grepl("^[0-9]+$", county)) %>%
    mutate(
      crop_name = trimws(crop_name),
      county = trimws(county),
      year = as.numeric(year)
    ) %>%
    filter(!is.na(crop_name), 
           crop_name != "",
           !is.na(county),
           county != "",
           !is.na(year))
  
  data_fixed
  
}, error = function(e) {
  stop(paste("Error processing CSV file:", e$message))
})

# Get year range and lists
year_min <- min(initial_data$year, na.rm = TRUE)
year_max <- max(initial_data$year, na.rm = TRUE)
crop_list <- sort(unique(initial_data$crop_name))
county_list <- sort(unique(initial_data$county))

ui <- fluidPage(
  titlePanel("Agricultural Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("crop_category", "Crop Category",
                  choices = c("All", crop_list),
                  selected = "All"),
      
      selectizeInput("county", "County",
                     choices = county_list,
                     selected = c("Fresno", "Kern"),
                     multiple = TRUE),
      
      selectInput("variable", "Variable",
                  choices = c("Land use" = "harvested_acres",
                              "Production" = "production",
                              "Value" = "value"),
                  selected = "harvested_acres"),
      
      sliderInput("year_range", "Year Range",
                  min = year_min,
                  max = year_max,
                  value = c(year_min, year_max),
                  step = 1,
                  sep = ""),
      
      hr(),
      downloadButton("downloadData", "Download Filtered Data"),
      downloadButton("downloadAllData", "Download All Data")
    ),
    
    mainPanel(
      plotOutput("timeseries_plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$crop_category)
    req(input$year_range)
    req(input$county)
    
    data <- initial_data
    
    if (input$crop_category != "All") {
      data <- data %>%
        filter(crop_name == input$crop_category)
    }
    
    data %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2],
             county %in% input$county) %>%
      arrange(year)
  })
  
  output$timeseries_plot <- renderPlot({
    req(filtered_data())
    
    y_var <- sym(input$variable)
    
    plot_data <- if(input$crop_category == "All") {
      filtered_data() %>%
        group_by(year, county) %>%
        summarize(!!input$variable := sum(!!y_var, na.rm = TRUE)) %>%
        ungroup()
    } else {
      filtered_data()
    }
    
    ggplot(plot_data, 
           aes(x = year, 
               y = !!y_var/1000,
               group = county,
               color = county)) +
      theme_minimal() +
      geom_line(size = 0.8, alpha = 0.7) +
      geom_point(size = 2.5) +
      scale_color_manual(values = c(
        "Fresno" = "#90EE90",
        "Kern" = "#B19CD9"
      )) +
      scale_x_continuous(
        name = "Year",
        breaks = seq(min(plot_data$year), max(plot_data$year), by = 5),
        expand = c(0.02, 0.02)
      ) +
      scale_y_continuous(
        name = case_when(
          input$variable == "harvested_acres" ~ "Thousand acres",
          input$variable == "production" ~ "Thousand units",
          input$variable == "value" ~ "Thousand dollars",
          TRUE ~ "Value (thousands)"
        ),
        labels = comma,
        expand = c(0.02, 0.02)
      ) +
      labs(
        title = case_when(
          input$variable == "harvested_acres" ~ "Land Use",
          input$variable == "production" ~ "Production",
          input$variable == "value" ~ "Value",
          TRUE ~ input$variable
        ),
        subtitle = if(input$crop_category == "All") "All Crops" 
        else paste("Crop Category:", input$crop_category),
        color = "County"
      ) +
      theme(
        panel.grid.minor = element_line(color = "gray95"),
        panel.grid.major = element_line(color = "gray90"),
        plot.title = element_text(size = 16, face = "plain", margin = margin(b = 5)),
        plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.box.margin = margin(l = 10),
        plot.margin = margin(20, 20, 20, 20)
      )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("agricultural-data-filtered-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$downloadAllData <- downloadHandler(
    filename = function() {
      paste("agricultural-data-all-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(initial_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)