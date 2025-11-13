# app/app.R
# Packages ----
library(shiny)
library(tidyverse)

# UI ----
ui <- fluidPage(
  titlePanel("MPG Explorer (mtcars)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X-axis:", choices = c("wt","hp","disp","qsec"), selected = "wt"),
      selectInput("color", "Color by:", choices = c("cyl","gear","carb"), selected = "cyl"),
      checkboxInput("show_lm", "Add linear trend", TRUE),
      sliderInput("nrows", "Sample N rows (for speed):", min = 10, max = nrow(mtcars), value = nrow(mtcars), step = 5)
    ),
    mainPanel(
      plotOutput("p"),
      tags$hr(),
      h4("Summary (second output)"),
      tableOutput("sumtab"),
      textOutput("corrtxt")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  dat <- reactive({
    mtcars %>%
      rownames_to_column("model") %>%
      slice_head(n = input$nrows)
  })
  
  output$p <- renderPlot({
    gg <- ggplot(dat(), aes_string(x = input$xvar, y = "mpg", color = input$color)) +
      geom_point(size = 2, alpha = 0.8) +
      theme_minimal(base_size = 12)
    if (isTRUE(input$show_lm)) gg <- gg + geom_smooth(method = "lm", se = TRUE)
    gg + labs(title = "Fuel efficiency vs chosen variable", y = "mpg", x = input$xvar)
  })
  
  output$sumtab <- renderTable({
    dat() %>%
      group_by(across(all_of(input$color))) %>%
      summarise(
        n = n(),
        mpg_mean = mean(mpg), mpg_sd = sd(mpg),
        .groups = "drop"
      )
  }, digits = 2)
  
  output$corrtxt <- renderText({
    r <- cor(dat()[[input$xvar]], dat()[["mpg"]], use = "complete.obs")
    paste0("Correlation between ", input$xvar, " and mpg: ", round(r, 3))
  })
}

# App ----
shinyApp(ui, server)


install.packages("rsconnect")
library(rsconnect)
# In shinyapps.io: Account -> Tokens -> "Show", then:
rsconnect::setAccountInfo(name="<your_name>", token="<TOKEN>", secret="<SECRET>")


rsconnect::deployApp("Week_11_Shiny/app")

## Week 11 — Shiny App
Live app: https://<your_name>.shinyapps.io/app/  
  Code: `Week_11_Shiny/app/app.R`
Features: 1) Dynamic ggplot with optional trend, 2) Summary table + correlation text.


