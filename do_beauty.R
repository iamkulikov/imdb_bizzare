library(dplyr)
library(ggplot2)
library(plotly)
setwd("C:/Projects/imdb_bizzare")
source("genre_script.R")

### Set options

options(timeout=400)  # for small bandwidth in seconds
update_data <- 0      # download full database from IMDB and recalculate (1) or take local (0)
basics_fname <- "title.basics.tsv.gz"
ratings_fname <- "title.ratings.tsv.gz"
use_adult <- 0
use_types <- c("movie")
export_fname <- "movies.xlsx"
export_sheetname <- c("data", "pairs_count")
import_fname <- "movies.xlsx"
import_sheetname <- c("data", "pairs_count")
OMDB_API_KEY <- "3c35f91c"   # move to external file?

### Import data

if (update_data == 1) {
  updateDataFromIMDB(basics_fname = basics_fname, use_adult = use_adult, use_types = use_types, export_fname = export_fname, export_sheetname = export_sheetname)
  gc()
  }
  
df2 <- readxl::read_excel(import_fname, sheet = import_sheetname[1], col_names = T)
genre_pairs <- readxl::read_excel(import_fname, sheet = import_sheetname[2], col_names = T)
genre_pairs_long <- expandPairList(genre_pairs = genre_pairs)
moviecount <- dim(df2)[1]


### Plot heatmap

genre_pairs_long_t <- genre_pairs_long %>% 
  mutate(text = glue::glue("{genre1} \n {genre2} \n Count = {pair_share}")) %>%
  mutate(genre1 = as.factor(genre1), genre2 = as.factor(genre2)) %>%
  mutate(genre1 = reorder(genre1,desc(pair_share)), genre2 = reorder(genre2,pair_share))
genres_ordered <- genre_pairs_long_t$genre1 %>% levels

gr <- ggplot(genre_pairs_long_t, 
            aes(x = genre1, y = genre2, fill = log(pair_share), text = text)) + 
            geom_tile(show.legend = FALSE) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
                  axis.title.x = element_blank(), axis.title.y = element_blank())

ggplotly(gr, tooltip="text")


### Generate a long list of movies based on the chosen genre pair
movie_list <- findMoviesByGenreComb(df2, "Sci-Fi", "Reality-TV")
short_movie_list <- movie_list %>% arrange(averageRating) %>% slice_head(n = 5)

### Augment the short list with information from OMDB
omdb_info <- getMovieDetails(imdb_codes = movie_list$tconst, OMDB_API_KEY = OMDB_API_KEY) %>% 
                  select(imdbID, Poster, Plot)
short_movie_list <- short_movie_list %>% left_join(omdb_info, by = c("tconst" = "imdbID")) %>% select(-c(tconst))
short_movie_list$Plot