# Sensitivity test shiny app
# allows us to manually look at different outcomes if we select different variables

if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)

#### Shiny app ####

looped.results = read.csv("./results/sensitivity results/looped results.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Vary slope length"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slope_length_slider", "Select slopeLength:", min = 3, max = 14, value = 7)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)




# Define server logic
server <- function(input, output) {
  # Filter data based on slider input
  filtered_data <- reactive({
    looped.results %>% filter(slopeLength == input$slope_length_slider)
  })
  
  # Create ggplot
  output$plot <- renderPlot({
    
    a =   ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = R.after.heatwave)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Peter")
    
    b =   ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = L.after.heatwave)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Paul")
    
    c =   ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = T.after.heatwave)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Tuesday")
    
    
    d =   ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = R.all.other.days)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Peter")
    
    e =   ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = L.all.other.days)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Paul")
    
    f =   ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = T.all.other.days)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Tuesday")
    
    
    ggarrange(a, b, c, d, e, f, nrow = 2, ncol = 3)
    
  })
}

# Run the application
shinyApp(ui, server)






