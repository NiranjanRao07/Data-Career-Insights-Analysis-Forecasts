library(shiny)
library(ggplot2)
library(dplyr)

# loading the dataset
file_path <- "jobs_in_data.csv"
data <- read.csv(file_path)

# building the Shiny app
ui <- fluidPage(
  titlePanel("Salary Prediction for Job Roles"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("job_title", "Select Job Title:",
                  choices = unique(data$job_title)),
      selectInput("country", "Select Country:",
                  choices = c("Global Average", unique(data$company_location))),
      checkboxInput("use_global", "Fallback to Global Data if insufficient?", value = TRUE)
    ),
    
    mainPanel(
      plotOutput("salaryPlot")
    )
  )
)

server <- function(input, output) {
  
  output$salaryPlot <- renderPlot({
    # filter data based on input
    filtered_data <- data %>%
      filter(job_title == input$job_title)
    
    # check if using specific country or global average
    if (input$country != "Global Average") {
      country_data <- filtered_data %>%
        filter(company_location == input$country)
    } else {
      country_data <- filtered_data
    }
    
    # handle insufficient data
    if (nrow(country_data) < 3) {
      if (input$use_global && nrow(filtered_data) >= 3) {
        country_data <- filtered_data
        message <- "Insufficient data for the selected country. Using global averages."
      } else {
        ggplot() +
          geom_text(aes(0.5, 0.5), label = "Not enough data for meaningful prediction.") +
          theme_void()
        return()
      }
    } else {
      message <- NULL
    }
    
    # building regression model
    model <- lm(salary ~ work_year, data = country_data)
    
    # generate predictions for the next 5 years
    future_years <- data.frame(work_year = seq(max(country_data$work_year) + 1,
                                               max(country_data$work_year) + 5))
    predictions <- predict(model, newdata = future_years)
    prediction_data <- cbind(future_years, salary = predictions)
    
    # combine actual and predicted data
    combined_data <- rbind(
      country_data %>% select(work_year, salary) %>% mutate(Type = "Actual"),
      prediction_data %>% mutate(Type = "Predicted")
    )
    
    # plotting the scatter plot with regression line
    plot <- ggplot(combined_data, aes(x = work_year, y = salary, color = Type)) +
      geom_point(size = 3) +
      geom_line(data = combined_data %>% filter(Type == "Predicted"), linetype = "dashed", size = 1.2) +
      geom_smooth(data = country_data, method = "lm", se = FALSE, color = "blue", size = 1) +
      labs(title = paste("Salary Prediction for", input$job_title, "in", input$country),
           subtitle = if (!is.null(message)) message else NULL,
           x = "Year", y = "Salary (USD)",
           color = "Data Type") +
      theme_minimal() +
      scale_color_manual(values = c("Actual" = "orange", "Predicted" = "brown"))
    
    plot
  })
}

shinyApp(ui = ui, server = server)