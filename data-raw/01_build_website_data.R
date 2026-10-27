# =========================================================
# 01_build_website_data.R
#
# Collects the per-country outputs of the BF-NEE pipeline v3
# (Scripts_C_v3: cropland + intensive pasture) into compact
# tables for the website.
#
# Run on the cluster:
#   Rscript data-raw/01_build_website_data.R
#   (or sbatch data-raw/build_data.job)
#
# Reads (from ~/Symobio/Data/Processed):
#   BF_NEE_v3_Countries/<ISO>/                     step 08
#   LandSHIFT_Countries/<ISO>/                     step 01 (pasture summary, cell area)
#   GlobES_on_LandSHIFT_agAdjusted_v3_Countries/   step 04 (coupling checks)
#   Commodity_shares_Countries/<country>/          step 06
#   Pipeline_Status/                               checkpoints of all steps
#
# Writes (data/, committed to git):
#   countries.csv                 one row per country + validity flags
#   country_transition.csv        country x period totals
#   country_ecosystem.csv         country x period x ecosystem class
#   country_commodity.csv         country x period x commodity (incl. grazing livestock)
#   country_commodity_ecosystem.csv  country x period x commodity x ecosystem group
#   commodity_expansion.csv       crop area change and shares (step 06)
#   landuse.csv                   country x year: cropland, grazing, intensive pasture
#   coupling_checks.csv           country x year: GlobES-LandSHIFT coupling (step 04)
#   pipeline_status.csv           country x step: latest checkpoint status
#   build_info.csv
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(tidyr)
})

# ---------------------------------------------------------
# PATHS (can be overridden with environment variables)
# ---------------------------------------------------------
dir_project   <- path.expand(Sys.getenv("BFNEE_PROJECT_DIR",   unset = "~/Symobio/bf-nee-website"))
dir_processed <- path.expand(Sys.getenv("BFNEE_PROCESSED_DIR", unset = "~/Symobio/Data/Processed"))
f_iso_list    <- path.expand(Sys.getenv("BFNEE_ISO_LIST",      unset = "~/Symobio/Scripts_C_v3/step08_iso_list.txt"))

dir_bf     <- file.path(dir_processed, "BF_NEE_v3_Countries")
dir_ls     <- file.path(dir_processed, "LandSHIFT_Countries")
dir_adj    <- file.path(dir_processed, "GlobES_on_LandSHIFT_agAdjusted_v3_Countries")
dir_comm   <- file.path(dir_processed, "Commodity_shares_Countries")
dir_status <- file.path(dir_processed, "Pipeline_Status")

dir_out <- file.path(dir_project, "data")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

f_lookup <- file.path(dir_out, "lookup", "ecosystem_classes.csv")

transitions <- c("1995_2000", "2000_2005", "2005_2010", "2010_2015")
years_ls    <- c(1995, 2000, 2005, 2010, 2015)

# v3 checkpoint names (run_country_by_iso_pipeline.R in Scripts_C_v3)
pipeline_steps <- c(
  "01_loaddata_landshift_v3",
  "02_loaddata_globes",
  "03_raster_adaptation",
  "04_raster_adaptation_shares_v3",
  "05_change_in_ecosystem_extent_v3",
  "06_Extraction_Commodity_Shares_v2",
  "07_Extraction_Production_Shares_v3",
  "08_calculating_indicator_v3"
)

# LandSHIFT cells are at most ~90 km² (5 arcmin)
max_plausible_cell_km2 <- 1000

stopifnot(dir.exists(dir_bf))
stopifnot(file.exists(f_lookup))

message("Project:   ", dir_project)
message("BF-NEE in: ", dir_bf)

# ---------------------------------------------------------
# HELPERS
# ---------------------------------------------------------
safe_filename <- function(x) {
  x <- str_replace_all(x, "[^[:alnum:]]+", "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  tolower(x)
}

read_csv_quiet <- function(f, ...) {
  if (!file.exists(f)) return(NULL)
  suppressWarnings(read_csv(f, show_col_types = FALSE, progress = FALSE, ...))
}

num <- function(df, col) {
  if (col %in% names(df)) as.numeric(df[[col]]) else rep(NA_real_, nrow(df))
}

# ecosystem_class must stay CHARACTER ("5.1" and "5.10" differ)
eco_col_types <- cols(
  ecosystem_class = col_character(),
  transition = col_character(),
  commodity = col_character(),
  .default = col_guess()
)

# ---------------------------------------------------------
# COUNTRY METADATA (same ISO3 fix as the pipeline)
# ---------------------------------------------------------
country_meta <- NULL

if (requireNamespace("rnaturalearth", quietly = TRUE) && requireNamespace("sf", quietly = TRUE)) {

  world <- sf::st_drop_geometry(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"))

  world$iso3 <- ifelse(is.na(world$iso_a3) | world$iso_a3 == "-99", world$adm0_a3, world$iso_a3)

  country_meta <- world %>%
    transmute(iso3, country_name = name_long, continent, region_un, subregion,
              country_slug = safe_filename(name_long)) %>%
    filter(!is.na(iso3), iso3 != "-99", iso3 != "ATA") %>%
    distinct(iso3, .keep_all = TRUE)
} else {
  warning("rnaturalearth not available: continent information will be missing.")
}

# ---------------------------------------------------------
# WHICH COUNTRIES?
# all countries of the pipeline ISO list + every output folder
# ---------------------------------------------------------
iso_list <- character(0)
if (file.exists(f_iso_list)) {
  iso_list <- toupper(trimws(gsub("\r", "", readLines(f_iso_list, warn = FALSE))))
  iso_list <- iso_list[nchar(iso_list) == 3]
}

iso_dirs <- basename(list.dirs(dir_bf, recursive = FALSE, full.names = TRUE))
iso_dirs <- iso_dirs[grepl("^[A-Z]{3}$", iso_dirs)]

iso_all <- sort(unique(c(iso_list, iso_dirs)))

message("Countries in ISO list: ", length(iso_list), " | output folders: ", length(iso_dirs))

# ---------------------------------------------------------
# READ ONE COUNTRY
# ---------------------------------------------------------
read_country <- function(iso3) {

  il <- tolower(iso3)
  d  <- file.path(dir_bf, iso3)

  summary <- read_csv_quiet(file.path(d, paste0("BF_NEE_summary_", il, ".csv")),
                            col_types = cols(transition = col_character(), .default = col_guess()))

  is_v3 <- !is.null(summary) && all(c("BF_NEE_pasture", "total_agriculture_gain_pasture") %in% names(summary))

  eco <- map_dfr(transitions, function(trans) {
    x <- read_csv_quiet(file.path(d, paste0("natural_ecosystem_loss_by_class_", il, "_", trans, ".csv")),
                        col_types = eco_col_types)
    if (is.null(x) || nrow(x) == 0 || !"natural_loss_area_pasture" %in% names(x)) return(NULL)
    x %>% transmute(
      iso3 = iso3, transition = trans, year = as.integer(year),
      ecosystem_class = as.character(ecosystem_class),
      loss_unconstrained_km2 = as.numeric(natural_loss_area_unconstrained),
      loss_constrained_km2   = as.numeric(natural_loss_area_agriConstrained),
      loss_cropland_km2      = as.numeric(natural_loss_area_cropland),
      loss_pasture_km2       = as.numeric(natural_loss_area_pasture),
      bfnee_km2              = as.numeric(BF_NEE)
    )
  })

  comm <- map_dfr(transitions, function(trans) {
    x <- read_csv_quiet(file.path(d, paste0("BF_NEE_by_commodity_", il, "_", trans, ".csv")),
                        col_types = eco_col_types)
    if (is.null(x) || nrow(x) == 0) return(NULL)
    x %>% transmute(
      iso3 = iso3, transition = trans, year = as.integer(year), commodity,
      loss_basis = if ("loss_basis" %in% names(x)) loss_basis else "cropland",
      S = as.numeric(S), P = as.numeric(P), weight = as.numeric(weight),
      loss_basis_km2 = as.numeric(total_natural_loss_area),
      bfnee_km2 = as.numeric(BF_NEE_commodity)
    )
  })

  comm_eco <- map_dfr(transitions, function(trans) {
    x <- read_csv_quiet(file.path(d, paste0("BF_NEE_by_ecosystem_and_commodity_", il, "_", trans, ".csv")),
                        col_types = eco_col_types)
    if (is.null(x) || nrow(x) == 0) return(NULL)
    x %>%
      group_by(ecosystem_class, commodity) %>%
      summarise(bfnee_km2 = sum(as.numeric(BF_NEE), na.rm = TRUE), .groups = "drop") %>%
      mutate(iso3 = iso3, transition = trans)
  })

  # Land use (step 01 v3)
  landuse <- read_csv_quiet(file.path(dir_ls, iso3, paste0("pasture_intensive_summary_", il, ".csv")))
  if (!is.null(landuse)) {
    landuse <- landuse %>%
      filter(year %in% years_ls) %>%
      transmute(iso3 = iso3, year = as.integer(year),
                stocking_threshold_lu_per_ha = as.numeric(stocking_threshold_lu_per_ha),
                cropland_km2 = as.numeric(cropland_area_km2),
                graze_km2 = as.numeric(graze_area_km2),
                pasture_int_km2 = as.numeric(pasture_int_area_km2),
                share_graze_intensive = as.numeric(share_graze_intensive))
  }

  # Coupling checks (step 04 v3)
  coupling <- map_dfr(years_ls, function(yr) {
    rd <- function(kind) {
      read_csv_quiet(file.path(dir_adj, iso3, paste0("globes_agAdjusted_", kind, "_", yr, "_", il, ".csv")))
    }
    s  <- rd("sum_stats")
    ag <- rd("ag_diff_stats")
    ps <- rd("pasture_diff_stats")
    if (is.null(s)) return(NULL)
    tibble(
      iso3 = iso3, year = as.integer(yr),
      class_sum_min = num(s, "min")[1], class_sum_max = num(s, "max")[1],
      crop_diff_max_abs = if (is.null(ag)) NA_real_ else max(abs(c(num(ag, "min")[1], num(ag, "max")[1]))),
      pasture_diff_max_abs = if (is.null(ps)) NA_real_ else max(abs(c(num(ps, "min")[1], num(ps, "max")[1])))
    )
  })

  # Pipeline checkpoints
  status <- read_csv_quiet(file.path(dir_status, paste0("pipeline_status_", iso3, ".csv")),
                           col_types = cols(.default = col_character()))
  if (!is.null(status) && nrow(status) > 0) {
    status <- status %>%
      mutate(row = row_number()) %>%
      group_by(step) %>%
      slice_max(row, n = 1) %>%
      ungroup() %>%
      transmute(iso3 = iso3, step, status, time, error)
  }

  # Scale check on the LandSHIFT cell area
  cell_area_max <- NA_real_
  f_cell <- file.path(dir_ls, iso3, paste0("cell_area_", il, ".tif"))
  if (file.exists(f_cell) && requireNamespace("terra", quietly = TRUE)) {
    cell_area_max <- as.numeric(terra::global(terra::rast(f_cell), "max", na.rm = TRUE)[1, 1])
  }

  list(
    summary = if (!is.null(summary)) mutate(summary, iso3 = iso3) else NULL,
    eco = eco, comm = comm, comm_eco = comm_eco,
    landuse = landuse, coupling = coupling, status = status,
    flags = tibble(
      iso3 = iso3,
      has_summary = !is.null(summary),
      is_v3 = is_v3,
      n_transitions = if (is.null(summary)) 0L else nrow(summary),
      cell_area_max_km2 = cell_area_max
    )
  )
}

# ---------------------------------------------------------
# READ ALL COUNTRIES
# ---------------------------------------------------------
all_countries <- compact(map(iso_all, function(iso3) {
  tryCatch(read_country(iso3), error = function(e) {
    warning("Failed reading ", iso3, ": ", conditionMessage(e))
    NULL
  })
}))

flags        <- map_dfr(all_countries, "flags")
summary_raw  <- map_dfr(all_countries, "summary")
eco_raw      <- map_dfr(all_countries, "eco")
comm_raw     <- map_dfr(all_countries, "comm")
comm_eco_raw <- map_dfr(all_countries, "comm_eco")
landuse      <- map_dfr(all_countries, "landuse")
coupling     <- map_dfr(all_countries, "coupling")
status_raw   <- map_dfr(all_countries, "status")

if (nrow(status_raw) == 0) {
  status_raw <- tibble(iso3 = character(), step = character(), status = character(),
                       time = character(), error = character())
}

lookup <- read_csv(f_lookup, show_col_types = FALSE,
                   col_types = cols(ecosystem_class = col_character(),
                                    tropical_moist_forest = col_logical(),
                                    .default = col_character()))

# ---------------------------------------------------------
# COUNTRY x PERIOD TOTALS
# ---------------------------------------------------------
if (nrow(summary_raw) == 0) stop("No v3 summaries found in ", dir_bf)

country_transition <- summary_raw %>%
  transmute(
    iso3,
    transition = as.character(transition),
    year = as.integer(year),
    n_natural_classes = as.integer(n_natural_classes),
    loss_unconstrained_km2 = num(summary_raw, "total_natural_loss_area_unconstrained"),
    agri_gain_km2          = num(summary_raw, "total_agriculture_gain_area"),
    agri_gain_cropland_km2 = num(summary_raw, "total_agriculture_gain_cropland"),
    agri_gain_pasture_km2  = num(summary_raw, "total_agriculture_gain_pasture"),
    loss_constrained_km2   = num(summary_raw, "total_natural_loss_area"),
    loss_cropland_km2      = num(summary_raw, "total_natural_loss_area_cropland"),
    loss_pasture_km2       = num(summary_raw, "total_natural_loss_area_pasture"),
    share_loss_attributed  = num(summary_raw, "share_loss_attributed_to_agri"),
    weight_sum             = num(summary_raw, "total_weight_sum_S_times_P"),
    P_pasture              = num(summary_raw, "P_pasture"),
    bfnee_km2              = num(summary_raw, "BF_NEE"),
    bfnee_cropland_km2     = num(summary_raw, "BF_NEE_cropland"),
    bfnee_pasture_km2      = num(summary_raw, "BF_NEE_pasture"),
    bfnee_unconstrained_km2 = num(summary_raw, "BF_NEE_unconstrained"),
    bfnee_sum_over_classes_km2 = num(summary_raw, "BF_NEE_sum_over_classes"),
    class_sum_minus_total  = num(summary_raw, "class_sum_minus_total")
  ) %>%
  mutate(class_sum_rel_error = ifelse(loss_constrained_km2 > 0,
                                      abs(class_sum_minus_total) / loss_constrained_km2, 0))

# ---------------------------------------------------------
# VALIDITY
# ---------------------------------------------------------
commodity_data <- comm_raw %>%
  group_by(iso3) %>%
  summarise(has_commodity_shares = any(!is.na(S) & loss_basis == "cropland"),
            has_production_shares = any(!is.na(P) & P > 0), .groups = "drop")

consistency <- country_transition %>%
  group_by(iso3) %>%
  summarise(
    classes_add_up = all(is.na(class_sum_rel_error) | class_sum_rel_error < 1e-6),
    constrained_le_unconstrained = all(is.na(loss_constrained_km2) |
                                         loss_constrained_km2 <= loss_unconstrained_km2 * (1 + 1e-9)),
    bfnee_parts_add_up = all(is.na(bfnee_km2) |
                               abs(bfnee_cropland_km2 + bfnee_pasture_km2 - bfnee_km2) <=
                               1e-6 * pmax(bfnee_km2, 1)),
    .groups = "drop"
  )

coupling_ok <- if (nrow(coupling) > 0) {
  coupling %>%
    group_by(iso3) %>%
    # cells with sum 0 (no GlobES cover) are set to NA in step 05 -> allowed
    summarise(coupling_sum_ok = all((class_sum_min > 0.999 | class_sum_min == 0) &
                                      class_sum_max < 1.001, na.rm = TRUE),
              .groups = "drop")
} else {
  tibble(iso3 = character(), coupling_sum_ok = logical())
}

step08 <- status_raw %>%
  filter(step == "08_calculating_indicator_v3") %>%
  transmute(iso3, step08_status = status)

countries <- tibble(iso3 = iso_all) %>%
  left_join(flags, by = "iso3") %>%
  left_join(commodity_data, by = "iso3") %>%
  left_join(consistency, by = "iso3") %>%
  left_join(coupling_ok, by = "iso3") %>%
  left_join(step08, by = "iso3") %>%
  mutate(
    across(c(has_summary, is_v3, has_commodity_shares, has_production_shares,
             classes_add_up, constrained_le_unconstrained, bfnee_parts_add_up),
           ~ coalesce(.x, FALSE)),
    coupling_sum_ok = coalesce(coupling_sum_ok, TRUE),
    n_transitions = coalesce(n_transitions, 0L),
    in_iso_list = iso3 %in% iso_list,
    scale_ok = is.na(cell_area_max_km2) | cell_area_max_km2 <= max_plausible_cell_km2,
    valid_for_analysis = has_summary & is_v3 & n_transitions == 4 & scale_ok &
      has_commodity_shares & classes_add_up & constrained_le_unconstrained &
      bfnee_parts_add_up & coupling_sum_ok,
    reason_invalid = case_when(
      !has_summary & coalesce(step08_status, "") == "failed" ~ "step 08 failed",
      !has_summary ~ "no v3 output (pipeline not finished)",
      !is_v3 ~ "old pipeline version",
      n_transitions < 4 ~ "missing periods",
      !scale_ok ~ "cell area on old scale",
      !has_commodity_shares ~ "no LandSHIFT commodity data",
      !classes_add_up ~ "ecosystem classes do not add up",
      !constrained_le_unconstrained ~ "constrained > unconstrained loss",
      !bfnee_parts_add_up ~ "cropland + pasture parts do not add up",
      !coupling_sum_ok ~ "GlobES composition does not sum to 1",
      TRUE ~ NA_character_
    )
  )

if (!is.null(country_meta)) {
  countries <- countries %>%
    left_join(country_meta, by = "iso3") %>%
    relocate(iso3, country_name, continent, region_un, subregion)
}

# ---------------------------------------------------------
# ECOSYSTEM / COMMODITY TABLES
# ---------------------------------------------------------
country_ecosystem <- eco_raw %>%
  left_join(lookup, by = "ecosystem_class") %>%
  mutate(ecosystem_name = coalesce(ecosystem_name, paste("Class", ecosystem_class)),
         eco_group = coalesce(eco_group, "Modified (counted as natural)"),
         tropical_moist_forest = coalesce(tropical_moist_forest, FALSE))

unknown_classes <- setdiff(unique(eco_raw$ecosystem_class), lookup$ecosystem_class)
if (length(unknown_classes) > 0) {
  warning("Ecosystem classes missing in lookup: ", paste(unknown_classes, collapse = ", "))
}

country_commodity <- comm_raw

country_commodity_ecosystem <- comm_eco_raw %>%
  left_join(lookup %>% select(ecosystem_class, eco_group), by = "ecosystem_class") %>%
  mutate(eco_group = coalesce(eco_group, "Modified (counted as natural)")) %>%
  group_by(iso3, transition, commodity, eco_group) %>%
  summarise(bfnee_km2 = sum(bfnee_km2, na.rm = TRUE), .groups = "drop")

# ---------------------------------------------------------
# COMMODITY EXPANSION (step 06)
# ---------------------------------------------------------
commodity_expansion <- tibble(iso3 = character(), transition = character(), year = integer(),
                              commodity = character(), area_start = numeric(), area_end = numeric(),
                              delta_area = numeric(), share = numeric())

if (dir.exists(dir_comm) && "country_slug" %in% names(countries)) {
  ce_tmp <- map_dfr(seq_len(nrow(countries)), function(i) {
    iso3 <- countries$iso3[i]
    slug <- countries$country_slug[i]
    if (is.na(slug)) return(NULL)
    map_dfr(transitions, function(trans) {
      x <- read_csv_quiet(file.path(dir_comm, slug, paste0("commodity_shares_", slug, "_", trans, ".csv")),
                          col_types = cols(transition = col_character(), commodity = col_character(),
                                           .default = col_guess()))
      if (is.null(x) || !"delta_area" %in% names(x)) return(NULL)
      x %>% transmute(iso3 = iso3, transition = trans, year = as.integer(year_end), commodity,
                      area_start = as.numeric(area_start), area_end = as.numeric(area_end),
                      delta_area = as.numeric(delta_area), share = as.numeric(share))
    })
  })
  if (nrow(ce_tmp) > 0) commodity_expansion <- ce_tmp
}

# ---------------------------------------------------------
# PIPELINE STATUS TABLE (latest entry per v3 step)
# ---------------------------------------------------------
pipeline_status <- tidyr::crossing(iso3 = iso_all, step = pipeline_steps) %>%
  left_join(status_raw, by = c("iso3", "step")) %>%
  mutate(status = coalesce(status, "not run"),
         step = factor(step, levels = pipeline_steps)) %>%
  arrange(iso3, step)

# ---------------------------------------------------------
# WRITE
# ---------------------------------------------------------
write_csv(countries, file.path(dir_out, "countries.csv"))
write_csv(country_transition, file.path(dir_out, "country_transition.csv"))
write_csv(country_ecosystem, file.path(dir_out, "country_ecosystem.csv"))
write_csv(country_commodity, file.path(dir_out, "country_commodity.csv"))
write_csv(country_commodity_ecosystem, file.path(dir_out, "country_commodity_ecosystem.csv"))
write_csv(commodity_expansion, file.path(dir_out, "commodity_expansion.csv"))
write_csv(landuse, file.path(dir_out, "landuse.csv"))
write_csv(coupling, file.path(dir_out, "coupling_checks.csv"))
write_csv(pipeline_status, file.path(dir_out, "pipeline_status.csv"))

write_csv(
  tibble(
    built_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
    source_dir = dir_processed,
    n_countries = nrow(countries),
    n_countries_v3 = sum(countries$is_v3),
    n_countries_valid = sum(countries$valid_for_analysis),
    pipeline_version = "v3: cropland + intensive pasture; commodity shares from positive area change; constrained loss split by ecosystem class"
  ),
  file.path(dir_out, "build_info.csv")
)

# ---------------------------------------------------------
# REPORT
# ---------------------------------------------------------
message("--------------------------------------------------")
message("Countries:            ", nrow(countries))
message("  with v3 outputs:    ", sum(countries$is_v3))
message("  valid for analysis: ", sum(countries$valid_for_analysis))

invalid <- countries %>% filter(!valid_for_analysis)
if (nrow(invalid) > 0) {
  message("Not valid:")
  print(count(invalid, reason_invalid), n = Inf)
}

message("Written to: ", dir_out)
