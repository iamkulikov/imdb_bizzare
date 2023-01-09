library(shiny)

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("Watch movies based on genre combinations (IMDB)"),
  fluidRow(
    column(5, 
      plotlyOutput("heat")),
    column(7,
      textOutput("chosen_genres"),
      tableOutput("table"))
  ),
  fluidRow(
    verbatimTextOutput("clicks")
  )  
)

server <- function(input, output, session) {
  
  output$heat <- renderPlotly(ggplotly(gr, tooltip="text", source = "heat_plot"))
  
  output$chosen_genres <- renderText({
    
    clickData <- event_data("plotly_click", source = "heat_plot")
    
    if (is.null(clickData)) {
      return("Choose the genre composition by clicking somewhere on the heatplot")
    } else {
      return(glue::glue("{clickData[['x']]} + {clickData[['y']]}"))
    }
    
  })
  
  output$table <- renderTable({
    
    clickData <- event_data("plotly_click", source = "heat_plot")
    
    if (is.null(clickData)) {
      
    } else {
      
    }
    
  })
  
  output$clicks <- renderPrint({
    
    clickData <- event_data("plotly_click", source = "heat_plot")
    clickData
    
  })  
  
}

shinyApp(ui, server)