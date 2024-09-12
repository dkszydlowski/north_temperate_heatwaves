#### shiny app for plotting timing of heatwave response under different conditions

if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)

if (!require(ggpubr)) install.packages('ggborderline')
library(ggborderline)


#-------------------------------------------------------------------------------#
###### MAKE TIMESCALES FIGURE ######



# Define UI
ui <- fluidPage(
  titlePanel("Phytoplankton response to heatwaves -- timing of response"),
  sidebarLayout(
    sidebarPanel(
      selectInput("input.orientation", "Analysis orientation:",
                  choices = c("end", "start")),
      sliderInput("slope_length_slider", "slopeLength:", min = 3, max = 8, value = 8, step = 1),
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
  annotate("rect", xmin=0, xmax=5, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=5, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=5, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=5, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
  annotate("rect", xmin=0, xmax=5, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
  annotate("rect", xmin=0, xmax=5, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
  annotate("rect", xmin=0, xmax=5, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=5, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=5, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=5, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=5, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=6, ymin=-75, ymax=Inf, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=6, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=7, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=7, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=7, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=7, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=7, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=7, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=7, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=8, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=8, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
  annotate("rect", xmin=0, xmax=8, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=8, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=9, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
  annotate("rect", xmin=0, xmax=9, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
  annotate("rect", xmin=0, xmax=10, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=10, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=10, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=10, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=13, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  annotate("rect", xmin=0, xmax=13, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=15, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
  annotate("rect", xmin=0, xmax=16, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
  geom_borderline(size = 1, bordercolour = "black")+
  scale_color_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D", "all" = "grey"), labels = c("L" = "Paul", "R" = "Peter", "T" = "Tuesday"))+
  geom_point(size = 2.5, pch = 21, aes(fill = lake),  color = "black", stroke = 0.7)+
  labs(title = "Response of chlorophyll to heatwaves over time")+
  #ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+1)+
  labs(x = "days relative to start of heatwave", y = "mean % change in chlorophyll \nacross heatwaves")+
  theme_classic()+
  scale_fill_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D", "all" = "grey"), labels = c("L" = "Paul", "R" = "Peter", "T" = "Tuesday"))+
  xlim(-15, 20)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  geom_vline(xintercept = 0, linetype = "dashed")
  # geom_text(aes(x = 20,
  #               y = 120,  
  #               label = "end of heatwaves",
  #               color = "black",
  #               vjust = 1))

      
    } else if(input$input.orientation == "end"){
      
      filtered_data() %>%  
        ggplot(aes( x= daysAfter, y = mean.response, color = lake))+
        annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-5, xmax=0, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
        annotate("rect", xmin=-5, xmax=0, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
        annotate("rect", xmin=-5, xmax=0, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
        annotate("rect", xmin=-5, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-5, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-5, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-5, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-5, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-6, xmax=0, ymin=-75, ymax=Inf, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-6, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-7, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-7, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-7, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-8, xmax=0, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
        annotate("rect", xmin=-8, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-8, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-9, xmax=0, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
        annotate("rect", xmin=-9, xmax=0, ymin=70, ymax=Inf, alpha = 0.07, fill="#BAAD8D", color = "grey") +
        annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-10, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-10, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-13, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey") +
        annotate("rect", xmin=-13, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-15, xmax=0, ymin=20, ymax=70, alpha = 0.07, fill="#4AB5C4", color = "grey") +
        annotate("rect", xmin=-16, xmax=0, ymin=-Inf, ymax=20, alpha = 0.07, fill="#ADDAE3", color = "grey")+
        geom_borderline(size = 1, bordercolour = "black")+
        scale_color_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D", "all" = "grey"), labels = c("L" = "Paul", "R" = "Peter", "T" = "Tuesday"))+
        geom_point(size = 2.5, pch = 21, aes(fill = lake),  color = "black", stroke = 0.7)+
        labs(title = "Response of chlorophyll to heatwaves over time")+
        #ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+1)+
        labs(x = "days relative to end of heatwave", y = "mean % change in chlorophyll \nacross heatwaves")+
        theme_classic()+
        scale_fill_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D", "all" = "grey"), labels = c("L" = "Paul", "R" = "Peter", "T" = "Tuesday"))+
        xlim(-15, 20)+
        geom_hline(yintercept = 0, linetype = "dashed")+
        theme(plot.title = element_text(hjust = 0.5))+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=12))+
        geom_vline(xintercept = 0, linetype = "dashed")
        # geom_text(aes(x = 20,
        #               y = 120,  
        #               label = "end of heatwaves",
        #               color = "black",
        #               vjust = 1))
      
      
    }

})
}



shinyApp(ui, server)