install.packages("shiny")
install.packages("ggplot2")
install.packages("plotly")
install.packages("corrplot")


library(shiny)
library(ggplot2)
library(plotly)
library(corrplot)

# Load your dataset
data <- read.csv("cleaned_target.csv")  # Adjust the path

# Clean column names
names(data) <- gsub(" ", "_", names(data))  # Replace spaces with underscores

ui <- fluidPage(
  titlePanel("Life Expectancy Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(data$Country)),
      selectInput("status", "Select Status:", choices = unique(data$Status)),
      selectInput("plotType", "Select Plot Type:", choices = c("Time Series", "Max/Min Life Expectancy", "Average Life Expectancy", "Life Expectancy Distribution", "Life Expectancy by Status"))
    ),
    
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive data based on input
  filteredData <- reactive({
    subset(data, Country == input$country & Status == input$status)
  })
  
  # Render plot based on user input
  output$plot <- renderPlotly({
    req(input$plotType)  # Ensure input is available
    
    plotData <- filteredData()
    
    # Check if the data is empty
    if (nrow(plotData) == 0) {
      return(NULL)  # No plot for empty data
    }
    
    if (input$plotType == "Time Series") {
      p <- ggplot(plotData, aes(x = Year, y = Life.expectancy)) +
        geom_line() +
        geom_point() +
        labs(title = paste("Life Expectancy Over Time -", input$country, input$status),
             x = "Year", y = "Life Expectancy") +
        theme_minimal()
      ggplotly(p)
      
    } else if (input$plotType == "Max/Min Life Expectancy") {
      max_life <- data[which.max(data$Life.expectancy), ]
      min_life <- data[which.min(data$Life.expectancy), ]
      
      p <- ggplot() +
        geom_bar(aes(x = max_life$Country, y = max_life$Life.expectancy), stat = "identity", fill = "green") +
        geom_bar(aes(x = min_life$Country, y = min_life$Life.expectancy), stat = "identity", fill = "red") +
        labs(title = "Countries with Maximum and Minimum Life Expectancy",
             x = "Country", y = "Life Expectancy") +
        theme_minimal()
      ggplotly(p)
      
    } else if (input$plotType == "Average Life Expectancy") {
      avg_life <- aggregate(Life.expectancy ~ Year, data = data, FUN = mean)
      
      p <- ggplot(avg_life, aes(x = Year, y = Life.expectancy)) +
        geom_line() +
        geom_point() +
        labs(title = "Average Life Expectancy Over the Years",
             x = "Year", y = "Average Life Expectancy") +
        theme_minimal()
      ggplotly(p)
      
    } else if (input$plotType == "Life Expectancy Distribution") {
      p <- ggplot(data, aes(x = Life.expectancy)) +
        geom_histogram(fill = "blue", bins = 30, alpha = 0.7) +
        labs(title = "Distribution of Life Expectancy",
             x = "Life Expectancy", y = "Frequency") +
        theme_minimal()
      ggplotly(p)
      
    } else if (input$plotType == "Life Expectancy by Status") {
      p <- ggplot(data, aes(x = Status, y = Life.expectancy, fill = Status)) +
        geom_boxplot() +
        labs(title = "Life Expectancy by Status (Developed/Developing)",
             x = "Status", y = "Life Expectancy") +
        theme_minimal()
      ggplotly(p)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
