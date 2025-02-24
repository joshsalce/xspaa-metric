library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(DT)
library(rsconnect)
library(tidyr)
library(openxlsx)
library(stringdist)
library(stringi)
library(magick)

# URL: https://joshsalce.shinyapps.io/xspw-v1/

# Read in Data =========================================================================================================

event_predictions_data <- read.csv("event_predictions_app.csv")

# Build UI and Server ==================================================================================================
# UI
ui <- fluidPage(
  titlePanel("xSPW Demo Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_rallyid", "Select a Rally:", 
                  choices = c("", unique(points_data$rally_select_name)),
                  selected = NULL,
                  multiple = FALSE),
      width = 5
    ),
    mainPanel(
      imageOutput("gif_image"),
      verbatimTextOutput("selected_text")
    )
    

  )
)

# Server
server <- function(input, output) {

  # Get rally id
  rally_id <- reactive({
    req(input$selected_rallyid)

    filter(points_data, rally_select_name %in% input$selected_rallyid)$rallyid
  })

  rally_server <- reactive({
    req(input$selected_rallyid)

    filter(points_data, rally_select_name %in% input$selected_rallyid)$server
  })

  rally_returner <- reactive({
    req(input$selected_rallyid)

    filter(points_data, rally_select_name %in% input$selected_rallyid)$returner
  })

  rally_winner <- reactive({
    req(input$selected_rallyid)

    filter(points_data, rally_select_name %in% input$selected_rallyid)$winner
  })

  rally_reason <- reactive({
    req(input$selected_rallyid)

    filter(points_data, rally_select_name %in% input$selected_rallyid)$reason
  })

  
  observeEvent(rally_id(), {
    
    print("Displaying Rally Info...")
    # Paths to the GIF and text files based on selection
    gif_path <- paste0("www/animation_", rally_id(), ".gif")
    #text_path <- paste0("www/", selected_option, ".txt")
    
    # Read the GIF and text files
    #gif <- image_read(gif_path)
    #selected_text <- readLines(text_path, warn = FALSE)
    
    # Render the GIF and text in the UI
    output$gif_image <- renderImage({
      list(src = gif_path, contentType = "image/gif", height='400px', width='800px')
    }, deleteFile = FALSE)
    
    # output$selected_text <- renderText({
    #   paste(selected_text, collapse = "\n")
    # })
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
