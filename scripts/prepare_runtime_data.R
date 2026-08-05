#!/usr/bin/env Rscript
# Build optimized runtime artifacts from source CSV files.
# Run from the project root: Rscript scripts/prepare_runtime_data.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(plotly)
})

source("genre_script.R")

movies_csv <- "movies.csv"
pairs_csv <- "pairs_count.csv"
movies_rds <- "movies.rds"
heatmap_rds <- "heatmap.rds"

missing <- c(movies_csv, pairs_csv)[!file.exists(c(movies_csv, pairs_csv))]
if (length(missing) > 0) {
  stop(
    "Missing source file(s): ", paste(missing, collapse = ", "), "\n",
    "Place movies.csv and pairs_count.csv in the project root, ",
    "or set update_data <- 1 in do_beauty.R to rebuild from IMDb TSV exports.",
    call. = FALSE
  )
}

message("Reading ", movies_csv, "...")
movies <- readr::read_csv(movies_csv, col_names = TRUE, show_col_types = FALSE)
saveRDS(movies, movies_rds, compress = "xz")
message("Wrote ", movies_rds, " (", nrow(movies), " rows)")

message("Reading ", pairs_csv, "...")
genre_pairs <- readr::read_csv(pairs_csv, col_names = TRUE, show_col_types = FALSE)
genre_pairs_long <- expandPairList(genre_pairs = genre_pairs)

genre_pairs_long_t <- genre_pairs_long %>%
  mutate(text = glue::glue("{genre1} \n {genre2} \n Count = {pair_share}")) %>%
  mutate(genre1 = as.factor(genre1), genre2 = as.factor(genre2)) %>%
  mutate(genre1 = reorder(genre1, desc(pair_share)), genre2 = reorder(genre2, pair_share))

genres_ordered <- levels(genre_pairs_long_t$genre1)
genres_ordered_y <- levels(genre_pairs_long_t$genre2)

gg <- ggplot(
  genre_pairs_long_t,
  aes(x = genre1, y = genre2, fill = log(pair_share), text = text)
) +
  geom_tile(show.legend = FALSE) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

plot_height <- max(520, length(genres_ordered) * 18)

gr <- ggplotly(
  gg,
  tooltip = "text",
  source = "heat_plot",
  height = plot_height
) %>%
  config(displayModeBar = FALSE)

saveRDS(
  list(
    gr = gr,
    genres_ordered = genres_ordered,
    genres_ordered_y = genres_ordered_y,
    n_genres = length(genres_ordered)
  ),
  heatmap_rds,
  compress = "xz"
)
message("Wrote ", heatmap_rds, " (", length(genres_ordered), " genres)")

message("Done. Start the app with shiny::runApp('app.R').")
