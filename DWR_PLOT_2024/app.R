library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(reshape2)
library(RColorBrewer)
library(ggiraph)
library(glue)

# Read the CPI-adjusted NASS data (this has 2022 data)
current_dir <- getwd()
parent_dir <- dirname(current_dir)
base_dir <- file.path(parent_dir, "DWR_DATA_AND_SCRIPTS")
data_dir <- file.path(base_dir, "data")
cpi_dir <- file.path(data_dir, "cpi_adjusted_data_from_NASS")
csv_filename <- file.path(cpi_dir, "final_mapped_data_with_cpi.csv")

# Read and process data following original DWR_PLOT logic exactly
initial_data <- tryCatch({
  # Read the data
  NASS_data <- read_csv(csv_filename, 
                        col_types = NULL,
                        locale = locale(encoding = "UTF-8"),
                        show_col_types = FALSE)
  
  # Process following original DWR_PLOT pattern - simple and clean
  group_data <- NASS_data %>%
    mutate(
      Acres = as.numeric(`Harvested Acres`),
      Price = as.numeric(`Adjusted Price`),
      Value = as.numeric(`Adjusted Total Production Value`),
      Adj_Gross_Revenue = as.numeric(`Adjusted Gross Revenue`)
    ) %>%
    filter(
      !is.na(County),
      County != "",
      !County %in% c("Sum of Others", "State Totals", "State Total"),
      !is.na(`Crop Name`),
      `Crop Name` != ""
    )
  
  # Follow original "Find proxy" logic - simplified
  dgroup_data <- group_data %>%
    group_by(`Crop Name`, Year, County) %>%
    mutate(
      Total_Area = sum(Acres, na.rm = TRUE) / 1000
    ) %>%
    ungroup()
  
  dgroup_data2 <- dgroup_data %>%
    group_by(`Crop Name`, County, Year) %>%
    arrange(desc(Acres)) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(County, Year) %>%
    mutate(
      Total_Area1 = sum(Acres, na.rm = TRUE) / 1000
    ) %>%
    ungroup()
  
  # Create simplified CSV_data structure
  CSV_data <- dgroup_data2 %>%
    mutate(
      NAME = County,
      Crop = `Crop Name`,
      Adj_Total_Prod_Value = (Adj_Gross_Revenue * Total_Area) / 1000
    ) %>%
    group_by(NAME, Year) %>%
    mutate(
      Adj_Total_Prod_Value1 = sum(Adj_Total_Prod_Value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(NAME, Crop, Year, Total_Area, Total_Area1, 
           Adj_Gross_Revenue, Adj_Total_Prod_Value, Adj_Total_Prod_Value1)
  
  # Transform to long format following original exactly
  Data <- melt(as.data.frame(CSV_data), id = c("NAME", "Crop", "Year"))
  
  # Add "Sum All" category for totals (following original pattern)
  Data[Data$variable %in% c("Total_Area1", "Adj_Total_Prod_Value1"), ]$Crop <- "Sum All"
  
  # Clean up variable names to match original
  Data$variable <- as.character(Data$variable)
  Data[Data$variable == "Total_Area", ]$variable <- "Land use"
  Data[Data$variable == "Total_Area1", ]$variable <- "Land use"
  Data[Data$variable == "Adj_Gross_Revenue", ]$variable <- "Adjusted Gross Revenue"
  Data[Data$variable == "Adj_Total_Prod_Value", ]$variable <- "Adjusted Total Production Value"
  Data[Data$variable == "Adj_Total_Prod_Value1", ]$variable <- "Adjusted Total Production Value"
  
  # Final processing - minimal filtering
  Data$Year <- as.numeric(Data$Year)
  Data <- Data %>% 
    rename(County = NAME) %>%
    filter(!is.na(value), !is.infinite(value))
  
  Data
  
}, error = function(e) {
  stop(paste("Error processing CSV file:", e$message))
})

# Get year range and lists
year_min <- min(initial_data$Year, na.rm = TRUE)
year_max <- max(initial_data$Year, na.rm = TRUE)
crop_list <- sort(unique(initial_data$Crop))
county_list <- sort(unique(initial_data$County))
variable_list <- unique(initial_data$variable)

# Define y-axis labels
y_labels <- c(
  "Land use" = "Thousand acres",
  "Adjusted Gross Revenue" = "USD/Acre",
  "Adjusted Total Production Value" = "Thousand USD"
)

# Colors for counties
n_counties <- length(county_list)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
if(n_counties > length(col_vector)) {
  col_vector <- colorRampPalette(col_vector)(n_counties)
}

ui <- fluidPage(
  titlePanel("Water and Land Use Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("crop_category", "Crop Category",
                  choices = c("All", crop_list),
                  selected = if("Alfalfa" %in% crop_list) "Alfalfa" else crop_list[1]),
      
      selectInput("crop_detail", "Crop Detail",
                  choices = c("All Varieties" = "all"),
                  selected = "all"),
      
      selectizeInput("county", "County",
                     choices = county_list,
                     selected = if("Alameda" %in% county_list) "Alameda" else county_list[1],
                     multiple = TRUE),
      
      selectInput("variable", "Variable",
                  choices = variable_list,
                  selected = "Land use"),
      
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
      girafeOutput("timeseries_plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "crop_detail",
                      choices = c("All Varieties" = "all"),
                      selected = "all")
  })
  
  filtered_data <- reactive({
    req(input$crop_category)
    req(input$year_range)
    req(input$county)
    req(input$variable)
    
    data <- initial_data
    
    # Filter by crop category
    if (input$crop_category != "All") {
      data <- data %>%
        filter(Crop == input$crop_category)
    } else {
      # For "All" selection, use the "Sum All" aggregated data
      data <- data %>%
        filter(Crop == "Sum All")
    }
    
    # Filter by other criteria
    data %>%
      filter(Year >= input$year_range[1],
             Year <= input$year_range[2],
             County %in% input$county,
             variable == input$variable) %>%
      filter(!is.na(value), !is.infinite(value)) %>%
      arrange(Year)
  })
  
  output$timeseries_plot <- renderGirafe({
    req(filtered_data())
    
    plot_data <- filtered_data()
    
    if(nrow(plot_data) == 0) {
      return(NULL)
    }
    
    plot <- ggplot(plot_data, 
                   aes(x = Year, 
                       y = value,
                       group = County,
                       color = County)) +
      theme_minimal() +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point_interactive(
        aes(
          tooltip = paste(
            glue("County: {County}"),
            glue("Year: {Year}"),
            glue("Value: {format(value, big.mark=',', scientific=FALSE)}"),
            sep = "\n"
          ),
          data_id = paste(County, Year, sep = "-")
        ),
        size = 3.5
      ) +
      scale_color_manual(values = col_vector) +
      scale_x_continuous(
        name = "Year",
        breaks = scales::pretty_breaks(n = 8),
        expand = c(0.02, 0.02)
      ) +
      scale_y_continuous(
        name = ifelse(input$variable %in% names(y_labels), 
                      y_labels[input$variable], 
                      input$variable),
        labels = comma,
        expand = c(0.02, 0.02)
      ) +
      labs(
        title = input$variable,
        subtitle = if(input$crop_category == "All") "All Crops" 
        else paste("Crop Category:", input$crop_category),
        color = "County"
      ) +
      theme(
        panel.grid.minor = element_line(color = "#F0F0F0"),
        panel.grid.major = element_line(color = "#E0E0E0"),
        plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5), hjust = 0.5),
        plot.subtitle = element_text(size = 16, margin = margin(b = 10), hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text = element_text(size = 12),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.margin = margin(20, 20, 20, 20),
        panel.border = element_blank(),
        axis.line = element_line(color = "#888888", size = 0.5)
      )
    
    girafe(
      ggobj = plot,
      options = list(
        opts_tooltip(
          opacity = 0.9,
          use_fill = TRUE,
          use_stroke = FALSE,
          css = "padding:8pt;font-family:sans-serif;font-size:12pt;color:black;background-color:white;border-radius:5px;"
        ),
        opts_hover_inv(css = "opacity:0.4;"),
        opts_hover(css = "stroke-width:3;")
      )
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("agricultural-data-filtered-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data_to_download <- filtered_data() %>%
        select(County, Crop, Year, variable, value)
      write.csv(data_to_download, file, row.names = FALSE)
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