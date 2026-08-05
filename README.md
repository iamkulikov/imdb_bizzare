# IMDb Bizarre

Find movies by unusual genre pairings in the IMDb non-adult feature-film catalog.

**Live app:** <https://iamkulikov-imdb-bizzare.share.connect.posit.cloud/>

![Screenshot of the app](imdb_screen.JPG)

## How it works

The heatmap shows how often each genre pair appears across all movies. Brighter tiles mean more films share that combination; darker tiles highlight rare or “bizarre” mixes. Click a cell to list matching titles with year, rating, and vote count. Title links open the film on IMDb.

Genre order follows pair frequency: common genres sit toward the top-left, rare pairings toward the bottom-right.

## Project layout

```         
app.R                      # Shiny UI and server
do_beauty.R                # Startup: loads prebuilt heatmap cache
genre_script.R             # IMDb import helpers and genre-pair logic
scripts/prepare_runtime_data.R   # Builds movies.rds and heatmap.rds from CSV
movies.csv                 # Movie catalog (source, not loaded at startup)
pairs_count.csv            # Genre-pair counts (source for heatmap rebuild)
movies.rds                 # Optimized catalog (lazy-loaded on first click)
heatmap.rds                # Prebuilt plotly heatmap + genre metadata
archive/                   # Historical prototypes (not used at runtime)
deploy/                    # Deployment guides
```

## Quick start

1.  Install R packages: `shiny`, `bslib`, `dplyr`, `ggplot2`, `plotly`, `readr`, `DT`, `glue`, `stringr`, `tidyr`, `tibble`, `jsonlite`.

2.  Place `movies.csv` and `pairs_count.csv` in the project root (or rebuild them from IMDb TSV exports via `update_data <- 1` in `do_beauty.R`).

3.  Build runtime artifacts:

    ``` bash
    Rscript scripts/prepare_runtime_data.R
    ```

4.  Run locally:

    ``` r
    shiny::runApp("app.R")
    ```

Deployment instructions: [deploy/posit-connect-cloud.md](deploy/posit-connect-cloud.md).

## Data source

Movie metadata and ratings come from the [IMDb non-commercial datasets](https://www.imdb.com/interfaces/) (`title.basics.tsv.gz`, `title.ratings.tsv.gz`). The app keeps non-adult feature films (`titleType == "movie"`) and counts co-occurring genres per title.

## Future directions

- Filters by minimum rating, vote count, or release year
- Exact genre-token matching instead of substring detection
- OMDb posters and plot summaries with API keys kept out of source code
- Heatmap legend and clearer click/selection UX
- Scripted IMDb refresh and dependency pinning (`renv`)
- Automated tests / CI and always-on hosting to reduce platform cold starts
