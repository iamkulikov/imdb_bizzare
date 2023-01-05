library("dplyr")
#library("reshape2")
#library("readr")
#library("tidyr")
#library("data.table")
#library("writexl")
#library("stringr")
#library("glue")

# Set options
setwd("C:/Projects/imdb_bizzare")
options(timeout=400)  # for small bandwidth in seconds
new <- 0              # download from IMDB (1) or not
read_full <- 1        # read from the full file or from already subsetted one
dataexport_fname <- "movies.xlsx"
dataimport_fname <- "movies.xlsx"
sheetname <- "data"

# Download source file from IMDB
# Data description: https://www.imdb.com/interfaces/
if (new == 1) {
 filename <- "title.basics.tsv.gz"
 url <- glue::glue("https://datasets.imdbws.com/{filename}")
 dest <- paste(getwd(), "title.basics.tsv.gz", sep="/")
 download.file(url, dest)
}

# Read (takes around 2 min when the full file is used)
if (read_full == 0) { 
  
  df2 <- readxl::read_excel(dataimport_fname, sheet = "data", col_names = T, skip=1)
  
  } else {
  
  df <- readr::read_tsv(filename)
  dim(df)
  
  ## Keep only widescreen non-adult movies
  df2 <- df %>% filter(isAdult == 0, titleType == "movie") %>% select(tconst, primaryTitle, genres)
  dim(df2)
  remove(df)
  
  data_export <- list(df2)
  names(data_export) <- c("data")
  writexl::write_xlsx(data_export, path = dataexport_fname, col_names = TRUE, format_headers = TRUE)
  
  }

moviecount <- dim(df2)[1]

# Construct genre dictionary
genre_list <- df2$genres %>% str_split(., ",") %>% unlist %>% unique
genre_list <- genre_list[!genre_list %in% c("\\N")]

# Count all genre pairs
num_pairs <- 0.5*length(genre_list)*(length(genre_list)-1)
num_pairs

genre_pairs <- combn(genre_list, 2) %>% t %>% data.frame %>% tibble %>%
    rename("genre1" = 1, "genre2" = 2) %>% mutate(pair_share = NA)
dim(genre_pairs)

for (i in seq_along(genre_pairs$genre1)) {
  genre_comb <- as.character(genre_pairs[i,])
  df3 <- df2 %>% mutate(first = str_detect(genres, genre_comb[1]), 
                        second = str_detect(genres, genre_comb[2]),
                        both = first*second) %>% filter(both == T) %>% select(tconst, primaryTitle)
  genre_pairs$pair_share[i] <- dim(df3)[1]/moviecount
}

genre_pairs <- genre_pairs %>% arrange(pair_share)
head(genre_pairs)

# Filter particular pair
genre_comb <- c('Sport', 'Western')

df3 <- df2 %>% mutate(first = str_detect(genres, genre_comb[1]), 
               second = str_detect(genres, genre_comb[2]),
               both = first*second) %>% filter(both == T) %>% select(tconst, primaryTitle)
head(df3)
dim(df3)
df3

