## Functions needed to choose movies based on genres

### Download source file from IMDB and save subset locally. Data description: https://www.imdb.com/interfaces/

updateDataFromIMDB <- function(download_filename, use_adult, use_types, dataexport_fname, dataexport_sheetname) {
  
  # download
  url <- glue::glue("https://datasets.imdbws.com/{download_filename}")
  dest <- paste(getwd(), download_filename, sep="/")
  #download.file(url, dest)
  #gc()
  
  # read
  df <- readr::read_tsv(download_filename)
  
  # filter
  if (use_adult == 0) {df2 <- df %>% filter(isAdult == 0) } else {df2 <- df}
  remove(df)
  df2 <- df2 %>% filter(titleType %in% use_types) %>% select(tconst, primaryTitle, genres)
  
  # count pair prevalence
  genre_list <- df2$genres %>% stringr::str_split(., ",") %>% unlist %>% unique
  genre_list <- genre_list[!genre_list %in% c("\\N")]
  
  genre_pairs <- combn(genre_list, 2) %>% t %>% data.frame %>% tibble %>%
    rename("genre1" = 1, "genre2" = 2) %>% mutate(pair_share = NA)
  
  for (i in seq_along(genre_pairs$genre1)) {
    genre_comb <- as.character(genre_pairs[i,])
    df3 <- df2 %>% mutate(first = stringr::str_detect(genres, genre_comb[1]), 
                          second = stringr::str_detect(genres, genre_comb[2]),
                          both = first*second) %>% filter(both == T)
    genre_pairs$pair_share[i] <- dim(df3)[1]
  }
  
  # export
  data_export <- list(df2, genre_pairs)
  names(data_export) <- dataexport_sheetname
  writexl::write_xlsx(data_export, path = dataexport_fname, col_names = TRUE, format_headers = TRUE)
  rm(data_export)
  
  }


### Count prevalence of individual genres

countGenrePrevalence <- function(data, genre_list) {
  
  genre_prevalence <- data.frame(genre = genre_list, count = NA)
  
  for (i in seq_along(genre_list)) {
    df3 <- data %>% mutate(isOurGenre = stringr::str_detect(genres, genre_list[i])) %>% filter(isOurGenre == T)
    genre_prevalence$count[i] <- dim(df3)[1]
  }
  
  genre_prevalence_ordered <- genre_prevalence %>% arrange(desc(count))
  return(genre_prevalence_ordered)
  
}

### Generate a full long pair list from the minimal one

expandPairList <- function(genre_pairs) {
  
  genre_pairs_swap <- genre_pairs %>% rename(genre1 = genre2, genre2 = genre1)
  genre_pairs_long <- rbind(genre_pairs, genre_pairs_swap) %>% unique 
  return(genre_pairs_long)
  
}

### Generate a genre pair matrix from long minimal format

makeHeatmapMatrix <- function(genre_pairs_long, order) {   #order has to contain vector of genre names like genre_prevalence_ordered$genre
  
  genre_pairs_mat <- tidyr::spread(genre_pairs_long, genre2, pair_share) %>% 
              mutate_if(is.numeric, tidyr::replace_na, 0)
  a <- unlist(genre_pairs_mat[,1])
  genre_pairs_mat <- genre_pairs_mat[,-1] %>% data.matrix
  rownames(genre_pairs_mat) <- a
  genre_pairs_mat <- genre_pairs_mat[order, order]           # reorder according to the prevalence
  return(genre_pairs_mat)

}

### Plot a basic heatmap graph
# heatmap(genre_pairs_mat, Colv = NA, Rowv = NA, main="Most common genre pairs", col = heat.colors(256))


### Function to produce the list of movies with a particular genre pair

findMoviesByGenreComb <- function(data, genre1, genre2) {

out <- data %>% mutate(first = stringr::str_detect(genres, genre1), 
               second = stringr::str_detect(genres, genre2),
               both = first*second,
               imdbLink = glue::glue("https://www.imdb.com/title/{tconst}")) %>% 
      filter(both == T) %>% select(tconst, primaryTitle, imdbLink)

return(out)

}


### Function to augment movie list with ratings, posters and descriptions

#adres <- "https://www.omdbapi.com/?i=tt1538901&apikey=3c35f91c"
#adres <- glue("https://www.omdbapi.com/?i={tconst}&apikey={OMDB_API_KEY}")
#print(adres)
#downloaded <- jsonlite::fromJSON(adres)
#downloaded %>% tidyjson::spread_all
#tt10245828

