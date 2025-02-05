library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)

# Load the dataset
filepath <- "C:\\Users\\bida22-020\\Downloads\\sampledata.xlsx"
customers <- read_excel(filepath)

# Clean up data: Remove "liters" from numeric columns and convert to numeric type
customers$`24 Hour Water Usage` <- as.numeric(gsub(" liters", "", customers$`24 Hour Water Usage`))
customers$`Water Usage Total` <- as.numeric(gsub(" liters", "", customers$`Water Usage Total`))
customers$`Water Forecast Usage Total` <- as.numeric(gsub(" liters", "", customers$`Water Forecast Usage Total`))
customers$`Meter Reading` <- as.numeric(gsub(" liters", "", customers$`Meter Reading`))

# Convert "Last 5 Days Water Usage" to list of numeric values
customers$`Last 5 Days Water Usage` <- strsplit(as.character(customers$`Last 5 Days Water Usage`), ",\\s*") %>%
  lapply(as.numeric)

# Flatten "Last 5 Days Water Usage" for table display
customers$`Last 5 Days Water Usage` <- sapply(customers$`Last 5 Days Water Usage`, toString)

# Add a Date column for each entry
customers$Date <- Sys.Date() - seq(0, by = 1, length.out = nrow(customers))

# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("Customer Water Usage Dataset"),
  
  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      # Slider to filter by 24 Hour Water Usage
      sliderInput("usageFilter",
                  "Filter by 24 Hour Water Usage (liters):",
                  min = min(customers$`24 Hour Water Usage`, na.rm = TRUE),
                  max = max(customers$`24 Hour Water Usage`, na.rm = TRUE),
                  value = c(min(customers$`24 Hour Water Usage`, na.rm = TRUE), 
                            max(customers$`24 Hour Water Usage`, na.rm = TRUE))),
      
      # Checkbox to filter by leaks
      checkboxInput("leakFilter",
                    "Show Only Customers with Leaks",
                    value = FALSE)
    ),
    
    # Main panel to display filtered table and plots
    mainPanel(
      tableOutput("filteredTable"),
      plotlyOutput("usagePlot"),
      plotlyOutput("creditPlot"),
      plotlyOutput("totalUsagePlot"),
      plotlyOutput("forecastUsagePlot"),
      plotlyOutput("meterReadingPlot"),
      plotlyOutput("leaksPlot"),
      plotlyOutput("last5DaysPlot")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Reactive expression to filter data based on user input
  filteredData <- reactive({
    req(input$usageFilter) # Ensure input is valid
    
    data <- customers
    
    # Filter by water usage range
    data <- data[data$`24 Hour Water Usage` >= input$usageFilter[1] & 
                   data$`24 Hour Water Usage` <= input$usageFilter[2], ]
    
    # Filter by leaks if checkbox is checked
    if (input$leakFilter) {
      data <- data[data$Leaks == "Yes", ]
    }
    
    return(data)
  })
  
  # Render the filtered table
  output$filteredTable <- renderTable({
    filteredData() %>%
      select(Date, `24 Hour Water Usage`, `Credit Remaining`, `Last 5 Days Water Usage`, `Water Usage Total`, `Water Forecast Usage Total`, `Meter Reading`, Leaks)
  })
  
  # Render bar plot for 24 Hour Water Usage
  output$usagePlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Date, y = `24 Hour Water Usage`, fill = factor(Date))) +
      geom_bar(stat = "identity") + 
      labs(title = "24 Hour Water Usage over Time", x = "Date", y = "24 Hour Water Usage (liters)") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # Render bar plot for Credit Remaining
  output$creditPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Date, y = as.numeric(gsub("P", "", `Credit Remaining`)), fill = factor(Date))) +
      geom_bar(stat = "identity") +
      labs(title = "Credit Remaining over Time", x = "Date", y = "Credit Remaining (P)") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # Render dot plot for Water Usage Total
  output$totalUsagePlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Date, y = `Water Usage Total`, )) +
      geom_point(size = 3) +
      geom_line() +
      labs(title = "Water Usage Total over Time", x = "Date", y = "Water Usage Total (liters)") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # Render dot plot for Water Forecast Usage Total
  output$forecastUsagePlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Date, y = `Water Forecast Usage Total`, color = factor(Date))) +
      geom_point(size = 3) +
      geom_line() +
      labs(title = "Water Forecast Usage Total over Time", x = "Date", y = "Water Forecast Usage Total (liters)") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # Render bar plot for Meter Reading
  output$meterReadingPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Date, y = `Meter Reading`)) +
      geom_bar(stat = "identity", fill = "steelblue") + # Set a single color for bars
      labs(title = "Meter Reading over Time", x = "Date", y = "Meter Reading (liters)") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # Render bar plot for Leaks
  output$leaksPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Date, y = as.numeric(Leaks == "Yes"), fill = factor(Date))) +
      geom_bar(stat = "identity") +
      labs(title = "Leaks over Time", x = "Date", y = "Leaks (1 = Yes, 0 = No)") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # Render plot for Last 5 Days Water Usage
  output$last5DaysPlot <- renderPlotly({
    data <- filteredData()
    
    # Create a data frame for plotting
    df <- data.frame(
      Day = rep(1:5, times = nrow(data)),
      Usage = unlist(data$`Last 5 Days Water Usage`),
      Date = rep(data$Date, each = 5)
    )
    
    p <- ggplot(df, aes(x = Day, y = Usage, color = factor(Date))) +
      geom_line() +
      geom_point(size = 3) +
      facet_wrap(~ Date) +
      labs(title = "Last 5 Days Water Usage", x = "Day", y = "Usage (liters)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
