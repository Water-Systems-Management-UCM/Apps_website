library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(janitor)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(ggiraph)
library(glue)

# File paths based on directory structure
current_dir <- getwd()
parent_dir <- dirname(current_dir)
base_dir <- file.path(parent_dir, "DWR_DATA_AND_SCRIPTS")
data_dir <- file.path(base_dir, "data")
transformed_dir <- file.path(data_dir, "transformed_ag_data")
csv_filename <- file.path(transformed_dir, "consolidated_agricultural_data_with_cpi.csv")

# Read and process data
initial_data <- tryCatch({
  # Read the data with appropriate column types
  data <- read_csv(csv_filename, 
                   col_types = NULL,
                   locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE)
  
  # Clean and process data
  data_processed <- data %>%
    # Ensure proper column names (keeping original case for now)
    rename(
      county = NAME,
      applied_water = AW,
      total_land_acres = Total_Land,
      total_water = TW,
      adjusted_price = `Adjusted Price`,
      adjusted_total_production_value = `Adjusted Total Production Value`,
      adjusted_gross_revenue = `Adjusted Gross Revenue`
    ) %>%
    mutate(
      # Ensure proper trimming of text fields
      county = trimws(county),
      crop = trimws(crop),
      year = as.numeric(year),
      # Convert to thousands for display
      total_land = total_land_acres / 1000,
      tw = total_water / 1000,
      # Keep original water per acre
      applied_water_per_acre = applied_water
    ) %>%
    # Calculate gross revenue per acre-foot
    mutate(
      gross_revenue_per_acre_foot = adjusted_gross_revenue / applied_water
    ) %>%
    filter(!is.na(crop), 
           crop != "",
           !is.na(county),
           county != "",
           !is.na(year))
  
  # Calculate totals by county and year
  county_year_totals <- data_processed %>%
    group_by(county, year) %>%
    summarize(
      total_land_use = sum(total_land, na.rm = TRUE),
      total_water_use = sum(tw, na.rm = TRUE),
      total_adjusted_value = sum(adjusted_total_production_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Join totals back to main data
  data_final <- data_processed %>%
    left_join(county_year_totals, by = c("county", "year"))
  
  # Add "Sum All" category for aggregated metrics
  data_sum_all <- data_final %>%
    select(county, year, total_land_use, total_water_use, total_adjusted_value) %>%
    distinct() %>%
    mutate(crop = "Sum All") %>%
    rename(
      total_land = total_land_use,
      tw = total_water_use,
      adjusted_total_production_value = total_adjusted_value
    )
  
  # Transform to long format for easier plotting
  data_long <- data_final %>%
    select(county, crop, year, 
           applied_water_per_acre, total_land, tw, 
           adjusted_gross_revenue, gross_revenue_per_acre_foot, 
           adjusted_total_production_value) %>%
    reshape2::melt(id.vars = c("county", "crop", "year"), 
                   variable.name = "variable", value.name = "value")
  
  # Add "Sum All" data in long format
  sum_all_long <- data_sum_all %>%
    select(county, crop, year, total_land, tw, adjusted_total_production_value) %>%
    reshape2::melt(id.vars = c("county", "crop", "year"),
                   variable.name = "variable", value.name = "value")
  
  # Combine regular and sum all data
  combined_long <- bind_rows(data_long, sum_all_long) %>%
    # Clean up variable names for display
    mutate(
      variable = case_when(
        variable == "total_land" ~ "Land use",
        variable == "tw" ~ "Water use",
        variable == "applied_water_per_acre" ~ "Applied water per acre",
        variable == "adjusted_gross_revenue" ~ "Adjusted Gross Revenue",
        variable == "gross_revenue_per_acre_foot" ~ "Gross revenue per acre-foot",
        variable == "adjusted_total_production_value" ~ "Adjusted Total Production Value",
        TRUE ~ as.character(variable)
      )
    )
  
  combined_long
  
}, error = function(e) {
  stop(paste("Error processing CSV file:", e$message))
})

# Get year range and lists
year_min <- min(initial_data$year, na.rm = TRUE)
year_max <- max(initial_data$year, na.rm = TRUE)
crop_list <- sort(unique(initial_data$crop))
county_list <- sort(unique(initial_data$county))
variable_list <- unique(initial_data$variable)

# Define y-axis labels for different variables
y_labels <- c(
  "Applied water per acre" = "Acre-feet/acre",
  "Land use" = "Thousand acres",
  "Water use" = "Thousand acre-feet",
  "Adjusted Gross Revenue" = "USD/Acre",
  "Gross revenue per acre-foot" = "USD/Acre-feet",
  "Adjusted Total Production Value" = "Thousand USD"
)

# Colors for counties - ensure we have enough colors
n_counties <- length(county_list)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# Ensure we have enough colors
if(n_counties > length(col_vector)) {
  col_vector <- colorRampPalette(col_vector)(n_counties)
}

ui <- fluidPage(
  titlePanel("Water and Land Use Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("crop_category", "Crop Category",
                  choices = c("All", crop_list),
                  selected = "Alfalfa"),
      
      selectizeInput("county", "County",
                     choices = county_list,
                     selected = "Alameda",
                     multiple = TRUE),
      
      selectInput("variable", "Variable",
                  choices = c(
                    "Land use",
                    "Water use",
                    "Applied water per acre",
                    "Adjusted Gross Revenue",
                    "Gross revenue per acre-foot",
                    "Adjusted Total Production Value"
                  ),
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
  filtered_data <- reactive({
    req(input$crop_category)
    req(input$year_range)
    req(input$county)
    req(input$variable)
    
    data <- initial_data
    
    # Filter by crop category
    if (input$crop_category != "All") {
      data <- data %>%
        filter(crop == input$crop_category)
    } else {
      # For "All" selection, use the "Sum All" aggregated data
      data <- data %>%
        filter(crop == "Sum All")
    }
    
    # Filter by other criteria
    data %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2],
             county %in% input$county,
             variable == input$variable) %>%
      # Filter out points with value of 0.0
      filter(value != 0.0) %>%
      arrange(year)
  })
  
  output$timeseries_plot <- renderGirafe({
    req(filtered_data())
    
    plot_data <- filtered_data()
    
    plot <- ggplot(plot_data, 
                   aes(x = year, 
                       y = value,
                       group = county,
                       color = county)) +
      theme_minimal() +
      geom_line(size = 0.8, alpha = 0.7) +
      geom_point_interactive(
        aes(
          tooltip = paste(
            glue("County: {county}"),
            glue("Year: {year}"),
            glue("Value: {format(value, big.mark=',', scientific=FALSE)}"),
            sep = "\n"
          ),
          data_id = paste(county, year, sep = "-")
        ),
        size = 2.5
      ) +
      scale_color_manual(values = col_vector) +
      scale_x_continuous(
        name = "Year",
        breaks = seq(min(plot_data$year), max(plot_data$year), by = 5),
        expand = c(0.02, 0.02)
      ) +
      scale_y_continuous(
        name = y_labels[input$variable],
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
        panel.grid.minor = element_line(color = "gray95"),
        panel.grid.major = element_line(color = "gray90"),
        plot.title = element_text(size = 16, face = "plain", margin = margin(b = 5), hjust = 0.5),
        plot.subtitle = element_text(size = 14, margin = margin(b = 10), hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.box.margin = margin(l = 10),
        plot.margin = margin(20, 20, 20, 20),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
      )
    
    girafe(
      ggobj = plot,
      options = list(
        opts_tooltip(
          opacity = 0.8,
          use_fill = TRUE,
          use_stroke = FALSE,
          css = "padding:5pt;font-family:sans-serif;color:black;background-color:white;border-radius:3px;"
        ),
        opts_hover_inv(css = "opacity:0.5;"),
        opts_hover(css = "stroke-width:2;")
      )
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("agricultural-data-filtered-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Get the filtered data
      data_to_download <- filtered_data() %>%
        select(county, crop, year, variable, value)
      
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