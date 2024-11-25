# Sensitivity test shiny app
# allows us to manually look at different outcomes if we select different variables

if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)


# #### Shiny app ####
# 
# # Define UI
# ui <- fluidPage(
#   titlePanel("Phytoplankton response to heatwaves -- sensitivity test"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("file_select", "Select File:",
#                   choices = c("looped results.csv", "looped results START OF HW.csv")),
#       sliderInput("slope_length_slider", "slopeLength:", min = 3, max = 10, value = 7, step = 1)
#     ),
#     mainPanel(
#       plotOutput("plot", height = "500px", width = "800px")
#     )
#   )
# )
# 
# # Define server logic
# server <- function(input, output, session) {
#   # Load the data based on file selection
#   looped.results <- reactive({
#     req(input$file_select)  # Ensure the file selection is available
#     read.csv(input$file_select)
#   })
#   
#   # Filter data based on slider input
#   filtered_data <- reactive({
#     req(looped.results())  # Ensure the data is loaded
#     looped.results() %>% filter(slopeLength == input$slope_length_slider)
#   })
#   
#   # Create ggplot
#   output$plot <- renderPlot({
#     req(filtered_data())  # Ensure the filtered data is available
#     
#     x_var <- if (input$file_select == "looped results START OF HW.csv") "daysAfterStart" else "daysAfter"
#     
#     
#     a <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "R.after.heatwave")) +
#       geom_tile(color = "black") +
#       scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
#       labs(title = "Peter during and after heatwave") +
#       theme_classic()
#     
#     b <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "L.after.heatwave")) +
#       geom_tile(color = "black") +
#       scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
#       labs(title = "Paul during and after heatwave") +
#       theme_classic()
#     
#     c <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "T.after.heatwave")) +
#       geom_tile(color = "black") +
#       scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
#       labs(title = "Tuesday during and after heatwave") +
#       theme_classic()
#     
#     d <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "R.all.other.days")) +
#       geom_tile(color = "black") +
#       scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
#       labs(title = "Peter all other days") +
#       theme_classic()
#     
#     e <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "L.all.other.days")) +
#       geom_tile(color = "black") +
#       scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
#       labs(title = "Paul all other days") +
#       theme_classic()
#     
#     f <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "T.all.other.days")) +
#       geom_tile(color = "black") +
#       scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
#       labs(title = "Tuesday all other days") +
#       theme_classic()
#     
#     ggarrange(a, d, b, e, c, f, nrow = 3, ncol = 2)
#   })
# }
# 
# # Run the application
# shinyApp(ui, server)
# 





##### new App #####



if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)


#### Shiny app ####

# Define UI
ui <- fluidPage(
  titlePanel("Phytoplankton response to heatwaves -- sensitivity test"),
  sidebarLayout(
    sidebarPanel(
      selectInput("input.orientation", "Analysis orientation:",
                  choices = c("end", "start")),
      sliderInput("slope_length_slider", "slopeLength:", min = 3, max = 8, value = 8, step = 1)
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
    #req(input$file_select)  # Ensure the file selection is available
    read.csv("start hw 2024_10_04.csv")
  })
  
  # Filter data based on slider input
  filtered_data <- reactive({
    req(looped.results())  # Ensure the data is loaded
    looped.results() %>% filter(slopeLength == input$slope_length_slider) %>% filter(orientation == input$input.orientation)
  })
  
  # Create ggplot
  output$plot <- renderPlot({
    req(filtered_data())  # Ensure the filtered data is available
    
    x_var <- "daysAfter"
    
    # set minimum and maximum values for plotting of each lake
    L.min = min(looped.results() %>% filter(orientation == input$input.orientation) %>% pull(L.after.heatwave),
                looped.results() %>% filter(orientation == input$input.orientation) %>% pull(L.all.other.days), na.rm = TRUE)
    
    
    L.max = max(looped.results() %>% 
                filter(orientation == input$input.orientation)
                %>% pull(L.after.heatwave), looped.results() %>% filter(orientation == input$input.orientation) %>%
                  pull(L.all.other.days), na.rm = TRUE)
    
    
    R.min = min(looped.results() %>% filter(orientation == input$input.orientation) %>% pull(R.after.heatwave),
                looped.results() %>% filter(orientation == input$input.orientation) %>% pull(R.all.other.days), na.rm = TRUE)
    
    
    R.max = max(looped.results() %>% 
                  filter(orientation == input$input.orientation)
                %>% pull(R.after.heatwave), looped.results() %>% filter(orientation == input$input.orientation) %>%
                  pull(R.all.other.days), na.rm = TRUE)
    
    T.min = min(looped.results() %>% filter(orientation == input$input.orientation) %>% pull(T.after.heatwave),
                looped.results() %>% filter(orientation == input$input.orientation) %>% pull(T.all.other.days), na.rm = TRUE)
    
    
    T.max = max(looped.results() %>% 
                  filter(orientation == input$input.orientation)
                %>% pull(T.after.heatwave), looped.results() %>% filter(orientation == input$input.orientation) %>%
                  pull(T.all.other.days), na.rm = TRUE)
    
    
    a <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "R.after.heatwave")) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse", limits = c(R.max, R.min)) +
      labs(title = "Peter heatwave") +
      theme_classic()
    
    b <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "L.after.heatwave")) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse",
                           limits = c(L.max, L.min)) +
      labs(title = "Paul heatwave") +
      theme_classic()
    
    c <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "T.after.heatwave")) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse", limits = c(T.max, T.min)) +
      labs(title = "Tuesday heatwave") +
      theme_classic()
    
    d <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "R.all.other.days")) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse", limits = c(R.max, R.min)) +
      labs(title = "Peter non-heatwave days") +
      theme_classic()
    
    e <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "L.all.other.days")) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse", limits = c(L.max, L.min)) +
      labs(title = "Paul non-heatwave days") +
      theme_classic()
    
    f <- ggplot(data = filtered_data(), aes_string(x = x_var, y = "numSlopes", fill = "T.all.other.days")) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse", limits = c(T.max, T.min)) +
      labs(title = "Tuesday non-heatwave days") +
      theme_classic()
    
    ggarrange(b, e, a, d, c, f, nrow = 3, ncol = 2)
  })
}

# Run the application
shinyApp(ui, server)
