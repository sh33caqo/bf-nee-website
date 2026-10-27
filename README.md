# BF-NEE website (pipeline v3)

Website and interactive map for **BF-NEE** – natural ecosystem loss through the
expansion of cropland and intensive pasture, attributed to commodities and German
final demand. The results come from the pipeline in `~/Symobio/Scripts_C_v3`.

- Website: <https://sh33caqo.github.io/bf-nee-website/>
- Map: <https://sh33caqo.shinyapps.io/bf-nee-global-map/>

## How it works

```
Scripts_C_v3 (cluster)  ->  ~/Symobio/Data/Processed/...   (large, not in git)
                                 |
data-raw/01_build_website_data.R |  small tables  -> data/*.csv        (in git)
data-raw/02_build_map_data.R     |  web rasters   -> shiny/global_map/data/ (not in git)
                                 v
render_website.R  ->  docs/  (GitHub Pages)      deploy.R -> shinyapps.io
```

The website only reads `data/*.csv`. New analyses that need the processed data are
added to `data-raw/01_build_website_data.R` as a new table, the figure to a `.qmd` page.

## Structure

```
├── _quarto.yml, *.qmd      pages: index, method, global, landuse, ecosystems,
│                           countries, commodities, regressions, maps, coverage
├── styles.css, theme.scss  layout and colours
├── R/theme.R, R/setup.R    colours/theme; data loading for all pages
├── data/lookup/ecosystem_classes.csv   IUCN class names + groups (edit here)
├── data/*.csv              tables built on the cluster
├── data-raw/               build scripts + SLURM job
├── shiny/global_map/       map app + deploy script (no credentials in the repo)
├── render_website.R        renders into docs/
└── docs/                   rendered website (GitHub Pages)
```

## Update (cluster)

```bash
cd ~/Symobio/bf-nee-website
sbatch data-raw/build_data.job        # tables + map data (tables only: --export=ALL,SKIP_MAP=1)
module load R
Rscript render_website.R
git add -A && git commit -m "Update results" && git push
Rscript shiny/global_map/deploy.R     # map (account registered once with rsconnect::setAccountInfo)
```

R packages: dplyr, readr, tidyr, purrr, stringr, forcats, ggplot2, scales, DT, plotly,
broom, knitr, rmarkdown, terra, sf, rnaturalearth, rnaturalearthdata, shiny, leaflet.
