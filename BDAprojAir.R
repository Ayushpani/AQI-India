# Step 1: Load Necessary Packages
# Uncomment the line below to install the packages if you haven't already
install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "DT", "reshape2"))

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(reshape2)  # for melt function

# Step 2: Load and Prepare the Dataset
air_data <- read.csv("final.csv", stringsAsFactors = FALSE)

# Convert 'Date' column to Date type
air_data$Date <- as.Date(air_data$Date, format="%Y-%m-%d")

# Step 3: Define the UI for the Shiny Dashboard
ui <- fluidPage(
  titlePanel("Air Pollution in India Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting a city
      selectInput("city", "Select City:", 
                  choices = unique(air_data$City),
                  selected = "Delhi"),
      
      # Dropdown for selecting a pollutant
      selectInput("pollutant", "Select Pollutant:", 
                  choices = c("PM2.5", "PM10", "NO", "NO2", "NOx", "NH3", "CO", "SO2", "O3", "Benzene", "Toluene", "Xylene"),
                  selected = "PM2.5"),
      
      # Date range filter
      dateRangeInput("date_range", "Select Date Range:", 
                     start = min(air_data$Date),
                     end = max(air_data$Date))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summary_table")),
        tabPanel("Time Series", plotlyOutput("time_series_plot")),
        tabPanel("Pollutant Distribution", plotlyOutput("pollutant_distribution")),
        tabPanel("Correlation Matrix", plotlyOutput("correlation_plot")),
        tabPanel("AQI Over Time", plotlyOutput("aqi_plot")),
        
        # Only AQI Pie Chart here
        tabPanel("AQI Distribution", 
                 plotlyOutput("aqi_bucket_pie")
        ),
        
        tabPanel("Top and Bottom 5 Cities", 
                 fluidRow(
                   column(6, plotlyOutput("top5_cities")),
                   column(6, plotlyOutput("bottom5_cities"))
                 )
        )
      )
    )
  )
)

# Step 4: Define the Server Logic for the Shiny Dashboard
server <- function(input, output) {
  
  # Filter data based on selected city and date range
  filtered_data <- reactive({
    air_data %>%
      filter(City == input$city,
             Date >= input$date_range[1] & Date <= input$date_range[2])
  })
  
  # 1. Summary Table of Pollutants
  output$summary_table <- renderDT({
    data <- filtered_data() %>%
      select(City, Date, PM2.5, PM10, NO, NO2, NOx, NH3, CO, SO2, O3, Benzene, Toluene, Xylene, AQI, AQI_Bucket)
    datatable(data)
  })
  
  # 2. Time Series Plot for Selected Pollutant
  output$time_series_plot <- renderPlotly({
    data <- filtered_data()
    p <- ggplot(data, aes(x = Date, y = get(input$pollutant))) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = paste(input$pollutant, "Over Time in", input$city),
           x = "Date", y = input$pollutant) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 3. Pollutant Distribution Plot
  output$pollutant_distribution <- renderPlotly({
    data <- filtered_data()
    p <- ggplot(data, aes(x = get(input$pollutant))) +
      geom_histogram(fill = "lightblue", bins = 30, color = "darkblue") +
      labs(title = paste("Distribution of", input$pollutant, "in", input$city),
           x = input$pollutant, y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 4. Correlation Matrix Plot
  output$correlation_plot <- renderPlotly({
    data <- filtered_data() %>%
      select(PM2.5, PM10, NO, NO2, NOx, NH3, CO, SO2, O3, Benzene, Toluene, Xylene) %>%
      cor(use = "complete.obs")
    
    p <- ggplot(melt(data), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Correlation Matrix of Pollutants") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 5. AQI Over Time Plot
  output$aqi_plot <- renderPlotly({
    data <- filtered_data()
    p <- ggplot(data, aes(x = Date, y = AQI)) +
      geom_line(color = "green") +
      geom_point(color = "orange") +
      labs(title = paste("AQI Over Time in", input$city),
           x = "Date", y = "AQI") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 6. Pie Chart for AQI Bucket Distribution
  output$aqi_bucket_pie <- renderPlotly({
    data <- filtered_data() %>%
      group_by(AQI_Bucket) %>%
      summarize(count = n())
    
    p <- plot_ly(data, labels = ~AQI_Bucket, values = ~count, type = 'pie',
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial') %>%
      layout(title = "AQI Bucket Distribution")
    
    p
  })
  
  # 7. Top 5 Most Polluted Cities
  output$top5_cities <- renderPlotly({
    top_cities <- air_data %>%
      group_by(City) %>%
      summarize(mean_AQI = mean(AQI, na.rm = TRUE)) %>%
      arrange(desc(mean_AQI)) %>%
      top_n(5, wt = mean_AQI)
    
    p <- ggplot(top_cities, aes(x = reorder(City, -mean_AQI), y = mean_AQI, fill = City)) +
      geom_col() +
      labs(title = "Top 5 Most Polluted Cities (by AQI)",
           x = "City", y = "Mean AQI") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 8. Bottom 5 Least Polluted Cities
  output$bottom5_cities <- renderPlotly({
    bottom_cities <- air_data %>%
      group_by(City) %>%
      summarize(mean_AQI = mean(AQI, na.rm = TRUE)) %>%
      arrange(mean_AQI) %>%
      top_n(-5, wt = mean_AQI)
    
    p <- ggplot(bottom_cities, aes(x = reorder(City, mean_AQI), y = mean_AQI, fill = City)) +
      geom_col() +
      labs(title = "Bottom 5 Least Polluted Cities (by AQI)",
           x = "City", y = "Mean AQI") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Step 5: Run the Shiny App
shinyApp(ui = ui, server = server)
