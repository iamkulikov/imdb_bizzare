library(dplyr)
library(ggplot2)
library(plotly)
#library(rsconnect)

#### !!!! Do not forget to comment the working directory for deployment
#setwd("C:/Projects/imdb_bizzare")
source("genre_script.R")

runtime_prepare_cmd <- "Rscript scripts/prepare_runtime_data.R"

load_runtime_artifact <- function(path, label) {
  if (!file.exists(path)) {
    stop(
      label, " not found (", path, ").\n",
      "Run: ", runtime_prepare_cmd,
      call. = FALSE
    )
  }
  readRDS(path)
}

### Set options

options(timeout = 400)  # for small bandwidth in seconds
update_data <- 0      # download full database from IMDB and recalculate (1) or take local (0)
basics_fname <- "title.basics.tsv.gz"
ratings_fname <- "title.ratings.tsv.gz"
use_adult <- 0
use_types <- c("movie")
importcsvdata_fname <- "movies.csv"
importcsvpairs_fname <- "pairs_count.csv"
OMDB_API_KEY <- "3c35f91c"   # move to external file?

if (update_data == 1) {
  updateDataFromIMDB(
    basics_fname = basics_fname,
    use_adult = use_adult,
    use_types = use_types,
    exportcsvdata_fname = importcsvdata_fname,
    exportcsvpairs_fname = importcsvpairs_fname
  )
  gc()
  message("Source CSV updated. Rebuild runtime artifacts with: ", runtime_prepare_cmd)
}

heatmap_data <- load_runtime_artifact("heatmap.rds", "Heatmap cache")
gr <- heatmap_data$gr
genres_ordered <- heatmap_data$genres_ordered
genres_ordered_y <- heatmap_data$genres_ordered_y
if (is.null(genres_ordered_y)) {
  genres_ordered_y <- genres_ordered
}
n_genres <- heatmap_data$n_genres
heatmap_height <- max(520, n_genres * 18)

if (is.null(gr$height) || identical(gr$height, "")) {
  gr$height <- heatmap_height
}

load_movies <- function() {
  load_runtime_artifact("movies.rds", "Movie catalog")
}
