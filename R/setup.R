# =========================================================
# Shared setup for all website pages
# - loads packages, theme and the compact data tables in data/
# - the tables are built on the cluster with
#   data-raw/01_build_website_data.R
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(DT)
})

source("R/theme.R")

knitr::opts_chunk$set(
  dev = "svglite",
  fig.width = 8.5,
  fig.height = 5,
  fig.align = "left",
  out.width = "100%",
  dev.args = list(bg = "transparent")
)

if (!requireNamespace("svglite", quietly = TRUE)) {
  knitr::opts_chunk$set(dev = "png", dpi = 150)
}

# ---------------------------------------------------------
# DATA
# ---------------------------------------------------------
read_data <- function(name, ...) {
  f <- file.path("data", paste0(name, ".csv"))
  if (!file.exists(f)) {
    stop("Missing data/", name, ".csv - run data-raw/01_build_website_data.R first.")
  }
  read_csv(f, show_col_types = FALSE, progress = FALSE, ...)
}

countries <- read_data("countries")

country_transition <- read_data(
  "country_transition",
  col_types = cols(transition = col_character(), .default = col_guess())
)

country_ecosystem <- read_data(
  "country_ecosystem",
  col_types = cols(ecosystem_class = col_character(),
                   transition = col_character(),
                   .default = col_guess())
)

country_commodity <- read_data(
  "country_commodity",
  col_types = cols(transition = col_character(), .default = col_guess())
)

commodity_expansion <- if (file.exists("data/commodity_expansion.csv")) {
  read_data("commodity_expansion",
            col_types = cols(transition = col_character(), .default = col_guess()))
} else {
  NULL
}

country_commodity_ecosystem <- if (file.exists("data/country_commodity_ecosystem.csv")) {
  read_data("country_commodity_ecosystem",
            col_types = cols(transition = col_character(), .default = col_guess()))
} else {
  NULL
}

build_info <- read_data("build_info")

opt_data <- function(name, ...) {
  if (file.exists(file.path("data", paste0(name, ".csv")))) read_data(name, ...) else NULL
}

landuse         <- opt_data("landuse")
coupling_checks <- opt_data("coupling_checks")
pipeline_status <- opt_data("pipeline_status", col_types = cols(.default = col_character()))

has_pasture <- "agri_gain_pasture_km2" %in% names(country_transition) &&
  any(country_transition$agri_gain_pasture_km2 > 0, na.rm = TRUE)

# ---------------------------------------------------------
# VALID COUNTRIES + LABELS
# ---------------------------------------------------------
valid_iso3 <- countries$iso3[countries$valid_for_analysis]

country_labels <- countries %>%
  transmute(
    iso3,
    country_name = coalesce(country_name, iso3),
    continent = ifelse(continent %in% continent_levels, continent, "Other"),
    continent = factor(continent, levels = continent_levels),
    region_un,
    subregion
  )

ct <- country_transition %>%
  filter(iso3 %in% valid_iso3) %>%
  left_join(country_labels, by = "iso3") %>%
  mutate(period = factor(transition_label(transition),
                         levels = transition_label(sort(unique(transition)))))

ce <- country_ecosystem %>%
  filter(iso3 %in% valid_iso3) %>%
  left_join(country_labels, by = "iso3") %>%
  mutate(
    eco_group = factor(eco_group, levels = eco_group_levels),
    period = factor(transition_label(transition),
                    levels = levels(ct$period))
  )

cc <- country_commodity %>%
  filter(iso3 %in% valid_iso3) %>%
  left_join(country_labels, by = "iso3") %>%
  mutate(
    commodity_group = fold_commodity(commodity),
    commodity_label = fold_commodity_label(commodity),
    period = factor(transition_label(transition),
                    levels = levels(ct$period))
  )

periods <- levels(ct$period)
latest_period <- tail(periods, 1)

# ---------------------------------------------------------
# GLOBAL TOTALS PER PERIOD
# ---------------------------------------------------------
global_period <- ct %>%
  group_by(period, year) %>%
  summarise(
    n_countries = n_distinct(iso3),
    loss_unconstrained_km2 = sum(loss_unconstrained_km2, na.rm = TRUE),
    agri_gain_km2 = sum(agri_gain_km2, na.rm = TRUE),
    loss_constrained_km2 = sum(loss_constrained_km2, na.rm = TRUE),
    bfnee_km2 = sum(bfnee_km2, na.rm = TRUE),
    bfnee_unconstrained_km2 = sum(bfnee_unconstrained_km2, na.rm = TRUE),
    agri_gain_cropland_km2 = if (has_pasture) sum(agri_gain_cropland_km2, na.rm = TRUE) else agri_gain_km2,
    agri_gain_pasture_km2 = if (has_pasture) sum(agri_gain_pasture_km2, na.rm = TRUE) else 0,
    loss_cropland_km2 = if (has_pasture) sum(loss_cropland_km2, na.rm = TRUE) else loss_constrained_km2,
    loss_pasture_km2 = if (has_pasture) sum(loss_pasture_km2, na.rm = TRUE) else 0,
    bfnee_cropland_km2 = if (has_pasture) sum(bfnee_cropland_km2, na.rm = TRUE) else bfnee_km2,
    bfnee_pasture_km2 = if (has_pasture) sum(bfnee_pasture_km2, na.rm = TRUE) else 0,
    .groups = "drop"
  ) %>%
  mutate(
    share_attributed = loss_constrained_km2 / loss_unconstrained_km2,
    bfnee_share_of_loss = bfnee_km2 / loss_constrained_km2
  )

# Small helper for DT tables in the house style
bf_table <- function(df, page_length = 10, ...) {
  datatable(
    df,
    rownames = FALSE,
    class = "compact hover",
    extensions = "Buttons",
    options = list(
      pageLength = page_length,
      dom = "Bfrtip",
      buttons = c("csv"),
      scrollX = TRUE
    ),
    ...
  )
}

# Round all numeric columns of a table for display
round_df <- function(df, digits = 2) {
  mutate(df, across(where(is.numeric), ~ signif(.x, digits + 2)))
}
