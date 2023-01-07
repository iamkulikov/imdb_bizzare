library(dplyr)
library(ggplot2)
library(plotly)
library(tidyjson)
setwd("C:/Projects/imdb_bizzare")
source("genre_script.R")

### Set options

options(timeout=400)  # for small bandwidth in seconds
update_data <- 1      # download full database from IMDB and recalculate (1) or take local (0)
download_filename <- "title.basics.tsv.gz"
use_adult <- 0
use_types <- c("movie")
dataexport_fname <- "movies.xlsx"
dataexport_sheetname <- c("data", "pairs_count")
dataimport_fname <- "movies.xlsx"
dataimport_sheetname <- c("data", "pairs_count")
OMDB_API_KEY <- "3c35f91c"   # move to external file?

### Import data

if (update_data == 1) {
  updateDataFromIMDB(download_filename = download_filename, use_adult = use_adult, use_types = use_types, dataexport_fname = dataexport_fname, dataexport_sheetname = dataexport_sheetname)
}

gc()
df2 <- readxl::read_excel(dataimport_fname, sheet = dataimport_sheetname[1], col_names = T)
genre_pairs <- readxl::read_excel(dataimport_fname, sheet = dataimport_sheetname[2], col_names = T)
genre_pairs_long <- expandPairList(genre_pairs = genre_pairs)
moviecount <- dim(df2)[1]


### Plot heatmap

genre_pairs_long_t <- genre_pairs_long %>% 
  mutate(text = glue::glue("{genre1} \n {genre2} \n Count = {pair_share}")) %>%
  mutate(genre1 = as.factor(genre1), genre2 = as.factor(genre2))

gr <- ggplot(genre_pairs_long_t, 
             aes(x = reorder(genre1,desc(pair_share)), 
                 y = reorder(genre2,pair_share),
                 fill = log(pair_share), text = text)) + 
  geom_tile(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggplotly(gr, tooltip="text")


### Generate a long list of movies based on the chosen genre pair

findMoviesByGenreComb(df2, "News", "Fantasy")
