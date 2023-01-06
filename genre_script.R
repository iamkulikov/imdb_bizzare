library(dplyr)
library(ggplot2)
library(plotly)

# Set options
setwd("C:/Projects/imdb_bizzare")
options(timeout=400)  # for small bandwidth in seconds
new <- 0              # download full database from IMDB (1) or not
read_full <- 0        # read from the full file or from the already subsetted one
recalc_pairs <- 0     # count pairs (1) or read precounted (0)
dataexport_fname <- "movies.xlsx" # for the subsetted file
dataimport_fname <- "movies.xlsx" # for the subsetted file
sheetname <- "data"   # in the subsetted file
pairsheetname <- "pair_count"

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

genre_prevalence2 <- genre_prevalence %>% arrange(desc(count))
genre_list <- genre_prevalence2$genre

# Count all genre pairs
if (recalc_pairs == 1) {
  
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
  
  data_export <- list(df2, genre_pairs)
  names(data_export) <- c(sheetname, pairsheetname)
  writexl::write_xlsx(data_export, path = dataexport_fname, col_names = TRUE, format_headers = TRUE)
  rm(data_export)

} else {genre_pairs <- readxl::read_excel(dataimport_fname, sheet = pairsheetname, col_names = T)}

#genre_pairs <- genre_pairs %>% arrange(pair_share)

# Make table longer to produce a heatmap
genre_pairs_swap <- genre_pairs %>% rename(genre1 = genre2, genre2 = genre1)
genre_pairs_long <- rbind(genre_pairs, genre_pairs_swap) %>% unique 
genre_pairs_long_t <- genre_pairs_long %>% 
  mutate(text = glue::glue("{genre1} \n {genre2} \n Count = {pair_share}")) %>%
  mutate(genre1 = as.factor(genre1), genre2 = as.factor(genre2))

# In case the some heatmap function only eats matrices
genre_pairs_mat <- tidyr::spread(genre_pairs_long, genre2, pair_share) %>% 
            mutate_if(is.numeric, tidyr::replace_na, 0)
a <- unlist(genre_pairs_mat[,1])
genre_pairs_mat <- genre_pairs_mat[,-1] %>% data.matrix
rownames(genre_pairs_mat) <- a
genre_pairs_mat <- genre_pairs_mat[genre_list,genre_list]           # reorder according to the prevalence
#genre_pairs_mat

# Basic heatmap graph
# heatmap(genre_pairs_mat, Colv = NA, Rowv = NA, main="Most common genre pairs", col = heat.colors(256))

# Interactive heatmap graph
gr <- ggplot(genre_pairs_long_t, 
                aes(x = reorder(genre1,desc(pair_share)), 
                    y = reorder(genre2,pair_share),
                    fill = log(pair_share), text = text)) + 
                geom_tile(show.legend = FALSE) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank())
ggplotly(gr, tooltip="text")

# Function to produce the list of movies with a particular genre pair

findMoviesByGenreComb <- function(data, genre1, genre2) {

out <- data %>% mutate(first = stringr::str_detect(genres, genre1), 
               second = stringr::str_detect(genres, genre2),
               both = first*second) %>% filter(both == T) %>% select(tconst, primaryTitle)

out

}

findMoviesByGenreComb(df2, "News", "Fantasy")


