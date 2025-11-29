# Load Libraries

library(shiny) # shiny to build the web app
library(tidyverse) # tidyverse for data manipulation and plotting

# Data
data(iris) #use the built-in 'iris' data set
summary(iris) #  150 flowers from 3 different species


# User Interface (UI) # defines what the user sees on the webpage.
ui <- fluidPage(  # creates page to adjusts the size of the browser

# Title of the App 
titlePanel("Abid Iris Explorer"),
  
# splits the page into two sections:A. 'sidebarPanel' on the left for inputs (dropdowns, checkboxes), B. 'mainPanel' on the right for outputs (graphs, tables)
sidebarLayout(
    
### Input ######
sidebarPanel(
     
# Dropdown menu to select the X-axis
 selectInput(inputId = "x_var",      # refer as input$x_var in the server
                  label = "Select X-axis Variable:",  # the user sees
                  choices = c("Sepal.Length", "Sepal.Width", 
                              "Petal.Length", "Petal.Width"),
                  selected = "Sepal.Length"),         # Default selection
      
# Dropdown menu to select the Y-axis
 selectInput(inputId = "y_var", 
                  label = "Select Y-axis Variable:",
                  choices = c("Sepal.Length", "Sepal.Width", 
                              "Petal.Length", "Petal.Width"),
                  selected = "Petal.Length"),
      
# Checkboxes to filter the data by Species
checkboxGroupInput(inputId = "species_filter", 
                         label = "Select Species to Show:",
                         choices = unique(iris$Species),  # all 3 species options
                         selected = unique(iris$Species)) #default
 ),
    

### Output : plot #######
 mainPanel( # Plot Placeholder
plotOutput("scatter_plot"), # created a "hole" here where the plot named scatter_plot will go.
  
h4("Summary Statistics for Selected Species"), #Summary Placeholder
      
verbatimTextOutput("summary_stats") # space for a text output named summary_stats
    )
  )
)

### Server : How the app WORKS #######
server <- function(input, output) { # Input (what the user changes) and updates output (what shows on screen).

filtered_data <- reactive({ #creates a live, updating version of data.
    iris %>%
      filter(Species %in% input$species_filter) # Filter rows based on user selection
  })
  
### Output: plot ###
output$scatter_plot <- renderPlot({ #  building a graph.
 
    ggplot(filtered_data(), aes_string(x = input$x_var, # reactive data
                                       y = input$y_var, # aes_string for x and y.
                                       color = "Species")) +
    geom_point(size = 3, alpha = 0.7) +   # points (dots)
    theme_minimal(base_size = 14) +       # theme
    labs(title = paste("Scatter Plot:", input$y_var, "vs", input$x_var))
  })
  
### Output : Summary ###
  output$summary_stats <- renderPrint({  # printing text or statistics.
  
  selected_data <- filtered_data()[, c(input$x_var, input$y_var)] ## Select only the X and Y columns 
  summary(selected_data) ## Run the summary
  })
}

# App
shinyApp(ui = ui, server = server) # UI and Server to launch the app
