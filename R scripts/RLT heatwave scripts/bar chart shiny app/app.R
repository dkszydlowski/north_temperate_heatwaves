#### shiny app for plotting bar chart of heatwave response under different conditions

if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)

if (!require(ggpubr)) install.packages('ggborderline')
library(ggborderline)


#-------------------------------------------------------------------------------#
###### MAKE BAR PLOT FIGURE ######



# Define UI
ui <- fluidPage(
  titlePanel("Phytoplankton response to heatwaves -- individual response"),
  sidebarLayout(
    sidebarPanel(
      selectInput("input.orientation", "Analysis orientation:",
                  choices = c("end")),
      sliderInput("days_after_slider", "daysAfter:", min = -16, max = 20, value = 3, step = 1),
      sliderInput("num_slopes_slider", "number of slopes:", min = 1, max = 10, value = 1, step = 1)
    ),
    mainPanel(
      plotOutput("plot", height = "500px", width = "800px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load the data based on file selection
  looped.results <- reactive({
    read.csv("start hw 2024_10_04.csv")
  })
  
  # Filter data based on slider input
  filtered_data <- reactive({
    req(looped.results())  # Ensure the data is loaded
    looped.results() %>% filter(slopeLength == input$slope_length_slider) %>% 
      filter(orientation == input$input.orientation) %>% 
      filter(numSlopes == input$num_slopes_slider) %>%
      select(slopeLength, daysAfter, numSlopes, orientation, contains("after.heatwave")) %>%  # Select only necessary columns
      pivot_longer(
        cols = ends_with("after.heatwave"),              # Pivot only after.heatwave columns
        names_to = "lake",                               # New column for lake names
        names_pattern = "(.*)\\.after\\.heatwave",       # Extract lake name from column names
        values_to = "mean.response"                      # New column for the values
      ) %>% 
      filter(lake != "all")
    
  })
  
  
  output$plot <- renderPlot({
    req(filtered_data())  # Ensure the filtered data is available
    
    if(input$input.orientation == "start"){
      
      filtered_data() %>%  
        ggplot(aes( x= daysAfter, y = mean.response, color = lake))+
        
      
    }
    
  })
}



shinyApp(ui, server)