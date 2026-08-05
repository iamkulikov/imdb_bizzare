source("do_beauty.R")
library(shiny)
library(DT)

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("Choose movies to watch based on genre combinations (IMDB)"),
  
  fluidRow(  
    column(6, plotlyOutput("heat", height = paste0(heatmap_height, "px"))),
    column(6, textOutput("chosen_genres"),
              tags$head(tags$style("#chosen_genres{color: red;
                                                  font-size: 25px;
                                                  font-style: bold;
                                                  }")),
              DTOutput("table"))
  )
)

server <- function(input, output, session) {
  
  movies_cache <- reactiveVal(NULL)

  ensure_movies_loaded <- function() {
    cached <- movies_cache()
    if (is.null(cached)) {
      cached <- load_movies()
      movies_cache(cached)
    }
    cached
  }

  # Placing the plot
  output$heat <- renderPlotly(gr) %>% bindCache(gr, cache = "app")
 
  # Defining all the reactive calculations click -> table
  clickData <- reactive(event_data("plotly_click", source = "heat_plot"))
  chosen_genre_x <- reactive({
    req(clickData())
    genre_from_plotly_click(clickData()[["x"]], genres_ordered)
  })
  chosen_genre_y <- reactive({
    req(clickData())
    genre_from_plotly_click(clickData()[["y"]], genres_ordered_y)
  })
  movies_to_show <- reactive({
    req(clickData(), chosen_genre_x(), chosen_genre_y())
    findMoviesByGenreComb(
      ensure_movies_loaded(),
      chosen_genre_x(),
      chosen_genre_y()
    ) %>%
      select(c(linkedTitle, startYear, averageRating, numVotes)) %>%
      rename(
        'Title' = 'linkedTitle',
        'Year' = 'startYear',
        'Rating' = 'averageRating',
        'Votes' = 'numVotes'
      )
  }) %>% bindCache(chosen_genre_x(), chosen_genre_y(), cache = "app")
  table_length <- reactive(dim(movies_to_show())[1])
  
  # Showing the names of a chosen genre pair 
  output$chosen_genres <- renderText({
    if (is.null(clickData())) {
      return("Choose the genre composition by clicking somewhere on the heatplot")
    }
    gx <- chosen_genre_x()
    gy <- chosen_genre_y()
    if (is.na(gx) || is.na(gy)) {
      return("Could not read the clicked genre pair. Try clicking another tile.")
    }
    glue::glue("{gx} + {gy} ({table_length()} found)")
  })
  
  # Printing a table of movies for a chosen genre pair
  output$table <- renderDT({
    
    if (is.null(clickData())) {
      return(NULL)
    } else {
      movies_to_show()
    }
    
  }, escape = FALSE, options = list(pageLength = 7, autoWidth = TRUE))  
  # Sleeping calculation for troubleshooting
  output$clicks <- renderPrint(clickData())  
  
}

shinyApp(ui, server)
