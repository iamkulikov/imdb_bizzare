library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(plotly)

# Set options
setwd("C:/Projects/imdb_bizzare")
options(timeout=400)  # for small bandwidth in seconds
new <- 0              # download full database from IMDB (1) or not
read_full <- 0        # read from the full file or from the already subsetted one
dataexport_fname <- "movies.xlsx" # for the subsetted file
dataimport_fname <- "movies.xlsx" # for the subsetted file
sheetname <- "data"   # in the subsetted file

# Download source file from IMDB
# Data description: https://www.imdb.com/interfaces/
filename <- "title.basics.tsv.gz"
if (new == 1) {
 url <- glue::glue("https://datasets.imdbws.com/{filename}")
 dest <- paste(getwd(), filename, sep="/")
 download.file(url, dest)
}

# Read data (takes around 6-7 min when the full file is used)
if (read_full == 0) { 
  
  df2 <- readxl::read_excel(dataimport_fname, sheet = "data", col_names = T)
  
  } else {
  
  df <- readr::read_tsv(filename)
  dim(df)
  
  ## Keep only widescreen non-adult movies
  df2 <- df %>% filter(isAdult == 0, titleType == "movie") %>% select(tconst, primaryTitle, genres)
  dim(df2)
  remove(df)
  
  ## Save truncated file
  data_export <- list(df2)
  names(data_export) <- c("data")
  writexl::write_xlsx(data_export, path = dataexport_fname, col_names = TRUE, format_headers = TRUE)
  rm(data_export)
  }

moviecount <- dim(df2)[1]

# Construct genre dictionary
genre_list <- df2$genres %>% stringr::str_split(., ",") %>% unlist %>% unique
genre_list <- genre_list[!genre_list %in% c("\\N")]
genre_list

# Count prevalence of all the genres
genre_prevalence <- data.frame(genre = genre_list, count = NA)

for (i in seq_along(genre_list)) {
  df3 <- df2 %>% mutate(isOurGenre = stringr::str_detect(genres, genre_list[i])) %>% filter(isOurGenre == T)
  genre_prevalence$count[i] <- dim(df3)[1]/moviecount
}

genre_prevalence <- genre_prevalence %>% arrange(desc(count))
genre_list <- genre_prevalence$genre

# Count all genre pairs
num_pairs <- 0.5*length(genre_list)*(length(genre_list)-1)
num_pairs

genre_pairs <- combn(genre_list, 2) %>% t %>% data.frame %>% tibble %>%
    rename("genre1" = 1, "genre2" = 2) %>% mutate(pair_share = NA)
dim(genre_pairs)

for (i in seq_along(genre_pairs$genre1)) {
  genre_comb <- as.character(genre_pairs[i,])
  df3 <- df2 %>% mutate(first = stringr::str_detect(genres, genre_comb[1]), 
                        second = stringr::str_detect(genres, genre_comb[2]),
                        both = first*second) %>% filter(both == T)
  genre_pairs$pair_share[i] <- dim(df3)[1]
}

#genre_pairs <- genre_pairs %>% arrange(pair_share)

# Make table longer to produce a heatmap
genre_pairs_swap <- genre_pairs %>% rename(genre1 = genre2, genre2 = genre1)
genre_pairs_long <- rbind(genre_pairs, genre_pairs_swap) %>% unique %>% 
            mutate(text = glue::glue("{genre1} \n {genre2} \n Count = {pair_share}"))

# In case the some heatmap function only eats matrices
#genre_pairs_mat <- tidyr::spread(genre_pairs_long, genre2, pair_share) %>% as.matrix()
#rownames(genre_pairs_mat) <- genre_pairs_mat[,1]
#genre_pairs_mat <- genre_pairs_mat[,-1]
#genre_pairs_mat <- genre_pairs_mat[genre_list,genre_list]           # reorder according to the prevalence

# Function to produce the heatmap graph

gr <- ggplot(genre_pairs_long, aes(genre1, genre2, fill = log(pair_share), text = text)) + 
  geom_tile()

ggplotly(gr, tooltip="text")

# Function to produce the list of movies with a particular genre pair

findMoviesByGenreComb <- function(data, genre1, genre2) {

out <- data %>% mutate(first = stringr::str_detect(genres, genre1), 
               second = stringr::str_detect(genres, genre2),
               both = first*second) %>% filter(both == T) %>% select(tconst, primaryTitle)

out

}

findMoviesByGenreComb(df2, "Mystery", "News")


