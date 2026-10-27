# =========================================================
# Render the website into docs/ (served by GitHub Pages)
#
#   cd ~/Symobio/bf-nee-website
#   Rscript render_website.R
#
# Needs the tables in data/ (data-raw/01_build_website_data.R).
# =========================================================

needed <- c("countries", "country_transition", "country_ecosystem",
            "country_commodity", "build_info")

missing <- needed[!file.exists(file.path("data", paste0(needed, ".csv")))]

if (length(missing) > 0) {
  stop("Missing data tables: ", paste(missing, collapse = ", "),
       "\nRun data-raw/01_build_website_data.R first.")
}

Sys.setenv(LANG = "C.UTF-8", LC_ALL = "C.UTF-8")

pkgs <- c("dplyr", "readr", "tidyr", "ggplot2", "scales", "DT", "forcats",
          "stringr", "broom", "knitr", "rmarkdown", "plotly")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing R packages: install.packages(c(\"",
       paste(missing_pkgs, collapse = "\", \""), "\"))")
}

status <- system("quarto render")
if (status != 0) stop("quarto render failed")

message("Website rendered to docs/. Commit and push docs/ to publish.")
