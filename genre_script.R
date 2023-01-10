## Functions needed to choose movies based on genres

### Download source file from IMDB and save subset locally. Data description: https://www.imdb.com/interfaces/

updateDataFromIMDB <- function(basics_fname, use_adult, use_types, exportcsvdata_fname, exportcsvpairs_fname) {
  
  # download
  url <- glue::glue("https://datasets.imdbws.com/{basics_fname}")
  dest <- paste(getwd(), basics_fname, sep="/")
  #download.file(url, dest)
  url <- glue::glue("https://datasets.imdbws.com/{ratings_fname}")
  dest <- paste(getwd(), ratings_fname, sep="/")
  #download.file(url, dest)
  gc()
  
  # read
  df <- readr::read_tsv(basics_fname)
  
  # filter
  if (use_adult == 0) {df2 <- df %>% filter(isAdult == 0) } else {df2 <- df}
  remove(df)
  df2 <- df2 %>% filter(titleType %in% use_types) %>% select(tconst, primaryTitle, startYear, genres)
  
  # augment with ratings
  ratings_df <- readr::read_tsv(ratings_fname)
  df2 <- df2 %>% left_join(ratings_df, by = c("tconst"="tconst"))
  
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
  write.csv(df2, exportcsvdata_fname, row.names=FALSE)
  write.csv(genre_pairs, exportcsvpairs_fname, row.names=FALSE)
  
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
               imdbLink = glue::glue("https://www.imdb.com/title/{tconst}"),
               linkedTitle = glue::glue("<a href = '{imdbLink}', target='_blank'>{primaryTitle}</a>")) %>% 
      filter(both == T) %>% select(-c(first, second, both)) %>% arrange(desc(averageRating))

return(out)

}

### Function to get movie details from OMDB
#a <- findMoviesByGenreComb(df2, "News", "Fantasy")

getMovieDetails <- function (imdb_codes, OMDB_API_KEY = OMDB_API_KEY) {
  
  #imdb_codes <- a$tconst
  for (i in seq_along(imdb_codes)) {
    
    adres <- glue::glue("https://www.omdbapi.com/?i={imdb_codes[i]}&apikey={OMDB_API_KEY}")
    #https://www.omdbapi.com/?i=tt10558850&apikey=3c35f91c
    downloaded <- jsonlite::fromJSON(adres)
    downloaded <- downloaded[names(downloaded) %in% c("imdbID","Title","Year","Plot","Poster","imdbRating","imdbVotes")] 
    downloaded <- as.data.frame(downloaded)
    downloaded
    if (i == 1) {output <- downloaded} else {output <- rbind(output, downloaded)}
    
  }
  #output
  
  return(tibble(output))
  
}


### Function to parse movie details from IMDB directly
