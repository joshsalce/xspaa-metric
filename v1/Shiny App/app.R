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
                              choices = c("", unique(event_predictions_data$rally_select_name)),
                              selected = NULL,
                              multiple = FALSE),
      width = 5
    ),
    mainPanel(
      p("This app includes all rallies for the final four games of the 2019 Australian Open Final.", br(),
        "Select a rally to show a GIF of the rally and its accompanying summary info."),
      fluidRow(
        column(9, imageOutput("gif_image")),
        column(3, div(style = "padding-left: 100px;", tableOutput("output_table")))
      )
    )
  )
)

# Server
server <- function(input, output) {

  # Get rally id
  rally_id <- reactive({
    req(input$selected_rallyid)

    filter(event_predictions_data, rally_select_name %in% input$selected_rallyid)$rallyid
  })

  # Filter data for rally, pivot table with summary info
  rally_data <- reactive({
    req(rally_id())

    event_predictions_data %>%
      filter(rallyid == rally_id()) %>%
      select(-rallyid, -rally_select_name) %>%
      mutate(xSPW = as.character(round(xSPW, 2))) %>%
      rename(
        Score = start_score,
        Server = server,
        Returner = returner,
        Winner = winner,
        Reason = reason
      ) %>% 
      pivot_longer(
        everything(), 
        names_to = "Name", 
        values_to = "Value"
      )
  })

  observeEvent(rally_id(), {
    
    print("Displaying Rally Info...")
    
    # Create path to GIF file
    gif_path <- paste0("www/animation_", rally_id(), ".gif")
    
    # Render GIF and summary info in the UI
    output$gif_image <- renderImage({
      list(src = gif_path, contentType = "image/gif", height='400px', width='800px', align = "left")
    }, deleteFile = FALSE)
    
    output$output_table <- renderTable({
      req(rally_data())
      rally_data() 
      
    }, options = list(
      pageLength = 10,
      autoWidth = FALSE
    ))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
