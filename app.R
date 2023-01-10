source("do_beauty.R")
library(shiny)

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("Choose movies to watch based on genre combinations (IMDB)"),
  
  fluidRow(  
    column(
      6,
      plotlyOutput("heat", height = "100%")),
    column(
      6,
      textOutput("chosen_genres"),
      tags$head(tags$style("#chosen_genres{color: red;
                                          font-size: 25px;
                                          font-style: bold;
                                          }")),
      dataTableOutput("table"))
  )
)

server <- function(input, output, session) {
  
  output$heat <- renderPlotly(ggplotly(gr, tooltip="text", source = "heat_plot")) %>% 
        bindCache(gr, cache = "app")
 
  
  clickData <- reactive(event_data("plotly_click", source = "heat_plot"))
  chosen_genre_x <- reactive(genres_ordered[clickData()[['x']]])
  chosen_genre_y <- reactive(genres_ordered[30 - as.numeric(clickData()[['y']])])
  movies_to_show <- reactive(findMoviesByGenreComb(df2, chosen_genre_x(), chosen_genre_y()) %>%
                select(c(linkedTitle, startYear, averageRating, numVotes)) %>%
                rename('Title' = 'linkedTitle',
                       'Year' = 'startYear',
                       'Rating' = 'averageRating',
                        'Votes' = 'numVotes')
                       ) %>% bindCache(chosen_genre_x(), chosen_genre_y(), cache = "app")
  table_length <- reactive(dim(movies_to_show())[1])
  
  
  output$chosen_genres <- renderText({
    
    if (is.null(clickData())) {
      return("Choose the genre composition by clicking somewhere on the heatplot")
    } else {
      return(glue::glue("{chosen_genre_x()} + {chosen_genre_y()} ({table_length()} found)"))
    }
    
  })
  
  
  output$table <- renderDataTable({
    
    if (is.null(clickData())) {
      return(NULL)
    } else {
      movies_to_show()
    }
    
  }, escape = FALSE, options = list(pageLength = 7, autoWidth = TRUE))
  
  
  output$clicks <- renderPrint(clickData())  
  
}

shinyApp(ui, server)