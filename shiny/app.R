library(shiny)

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("Watch movies based on genre combinations (IMDB)"),
  fluidRow(
    column(5, 
      plotlyOutput("heat")),
    column(7,
      textOutput("chosen_genres"),
      tags$head(tags$style("#chosen_genres{color: red;
                                 font-size: 30px;
                                 font-style: bold;
                                 }")),
      dataTableOutput("table"))
  ),
  fluidRow(
    verbatimTextOutput("clicks")
  )  
)

server <- function(input, output, session) {
  
  output$heat <- renderPlotly(ggplotly(gr, tooltip="text", source = "heat_plot"))
  clickData <- reactive(event_data("plotly_click", source = "heat_plot"))
  chosen_genre_x <- reactive(genres_ordered[clickData()[['x']]])
  chosen_genre_y <- reactive(genres_ordered[30 - as.numeric(clickData()[['y']])])
  movies_to_show <- reactive(findMoviesByGenreComb(df2, chosen_genre_x(), chosen_genre_y()))
  
  output$chosen_genres <- renderText({
    
    if (is.null(clickData())) {
      return("Choose the genre composition by clicking somewhere on the heatplot")
    } else {
      return(glue::glue("{chosen_genre_x()} + {chosen_genre_y()}"))
    }
    
  })
  
  output$table <- renderDataTable({
    
    if (is.null(clickData())) {
      return(NULL)
    } else {
      movies_to_show()
    }
    
  })
  
  output$clicks <- renderPrint({

    clickData()
    
  })  
  
}

shinyApp(ui, server)