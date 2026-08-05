#!/usr/bin/env Rscript
# Benchmark startup vs first-click catalog load, and smoke-test core logic.

suppressPackageStartupMessages({
  library(dplyr)
})

root <- normalizePath(getwd(), winslash = "/")
setwd(root)

results <- list()

time_it <- function(label, expr) {
  gc()
  t <- system.time(expr)
  results[[label]] <<- t[["elapsed"]]
  message(sprintf("%s: %.3f s", label, t[["elapsed"]]))
  invisible(t)
}

# --- Startup benchmark (heatmap only, no movies.csv) ---
startup_env <- new.env(parent = globalenv())
time_it("startup_source_do_beauty", {
  sys.source("do_beauty.R", envir = startup_env, keep.source = FALSE)
})

gr <- startup_env$gr
genres_ordered <- startup_env$genres_ordered
n_genres <- startup_env$n_genres
load_movies <- startup_env$load_movies

stopifnot(!is.null(gr), !is.null(genres_ordered), !is.null(n_genres))
stopifnot(is.null(startup_env$df2))

# --- First-click catalog load ---
time_it("first_movies_load", {
  df2 <- load_movies()
})

stopifnot(nrow(df2) > 0)

# --- Genre click mapping smoke test ---
source("genre_script.R")

mapped_x_label <- genre_from_plotly_click("Drama", genres_ordered)
mapped_y_label <- genre_from_plotly_click("Comedy", genres_ordered)
mapped_x_idx <- genre_from_plotly_click(1, genres_ordered)
mapped_y_idx <- genre_from_plotly_click(1, genres_ordered)
stopifnot(mapped_x_label == "Drama", mapped_y_label == "Comedy")
stopifnot(mapped_x_idx == "Drama", nzchar(mapped_y_idx))

test_x <- "Drama"
test_y <- "Comedy"
sample <- findMoviesByGenreComb(df2, test_x, test_y)
stopifnot(nrow(sample) > 0)
stopifnot(all(c("linkedTitle", "averageRating") %in% names(sample)))
stopifnot(grepl("<a href", sample$linkedTitle[1]))

# --- Cached repeat lookup ---
time_it("cached_genre_lookup", {
  out <- findMoviesByGenreComb(df2, test_x, test_y)
})
stopifnot(nrow(out) == nrow(sample))

# --- Deployment bundle check ---
rscignore <- readLines(".rscignore", warn = FALSE)
rscignore <- rscignore[nchar(trimws(rscignore)) > 0 & !grepl("^#", trimws(rscignore))]

should_deploy <- c(
  "app.R", "do_beauty.R", "genre_script.R",
  "movies.rds", "heatmap.rds", ".rscignore"
)
missing_deploy <- should_deploy[!file.exists(should_deploy)]
if (length(missing_deploy) > 0) {
  stop("Missing deployment files: ", paste(missing_deploy, collapse = ", "))
}

excluded_hits <- c()
for (pattern in rscignore) {
  if (pattern == "archive/") {
    if (dir.exists("archive")) excluded_hits <- c(excluded_hits, "archive/")
  } else if (file.exists(pattern)) {
    excluded_hits <- c(excluded_hits, pattern)
  }
}

cat("\n=== Benchmark summary ===\n")
cat(sprintf("Startup (do_beauty.R):     %.3f s\n", results[["startup_source_do_beauty"]]))
cat(sprintf("First movies.rds load:     %.3f s\n", results[["first_movies_load"]]))
cat(sprintf("Genre lookup (cached):     %.3f s\n", results[["cached_genre_lookup"]]))
cat(sprintf("Genres in heatmap:         %d\n", n_genres))
cat(sprintf("Movies in catalog:         %d\n", nrow(df2)))
cat("\n=== Smoke test ===\n")
cat(sprintf("Click labels Drama+Comedy: %s + %s\n", mapped_x_label, mapped_y_label))
cat(sprintf("Click indices x=1 -> %s, y=1 -> %s\n", mapped_x_idx, mapped_y_idx))
cat(sprintf("Sample pair %s + %s: %d movies\n", test_x, test_y, nrow(sample)))
cat("IMDb link HTML: OK\n")
cat("\n=== Deployment bundle ===\n")
cat("Required files present: ", paste(should_deploy, collapse = ", "), "\n")
cat("Excluded by .rscignore:   ", paste(unique(excluded_hits), collapse = ", "), "\n")
cat("\nAll checks passed.\n")
