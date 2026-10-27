# =========================================================
# 02_build_map_data.R
#
# Builds the compact global map data for the Shiny app from
# the per-country rasters of the BF-NEE pipeline (v3).
#
# Instead of 155 country folders (~160 MB, in git before), the
# app gets one small multi-layer raster per transition:
#
#   layer "All natural ecosystems"  agriculture-constrained loss R
#   layer "<ecosystem group>"       R summed over the classes of a group
#   layer "Tropical moist forest"   R of classes 1.6 + 1.9
#   layer "Unconstrained loss"      N (gross natural loss)
#   layer "Agricultural expansion"  Gain_agri
#
# plus a country-ID raster, so the app can multiply R with the
# commodity weights W_c of each producing country (BF-NEE).
#
# Run on the cluster AFTER 01_build_website_data.R:
#   Rscript data-raw/02_build_map_data.R
#
# Output (NOT in git, uploaded with the app):
#   shiny/global_map/data/
# =========================================================

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
})

# ---------------------------------------------------------
# SETTINGS
# ---------------------------------------------------------
dir_project <- path.expand(Sys.getenv(
  "BFNEE_PROJECT_DIR",
  unset = "~/Symobio/bf-nee-website"
))

dir_processed <- path.expand(Sys.getenv(
  "BFNEE_PROCESSED_DIR",
  unset = "~/Symobio/Data/Processed"
))

# Aggregation factor for the web map (sum of km² per cell).
# 2 = ~18 km cells at 5 arcmin input. Larger = smaller app.
agg_fact <- as.integer(Sys.getenv("BFNEE_MAP_AGG", unset = "2"))

dir_bf  <- file.path(dir_processed, Sys.getenv("BFNEE_BF_FOLDER", unset = "BF_NEE_v3_Countries"))
dir_out <- file.path(dir_project, "shiny", "global_map", "data")
dir_tmp <- file.path(dir_out, "tmp")

dir.create(dir_tmp, recursive = TRUE, showWarnings = FALSE)

terraOptions(tempdir = dir_tmp, progress = 0)

transitions <- c("1995_2000", "2000_2005", "2005_2010", "2010_2015")

countries <- read_csv(file.path(dir_project, "data", "countries.csv"),
                      show_col_types = FALSE)

lookup <- read_csv(
  file.path(dir_project, "data", "lookup", "ecosystem_classes.csv"),
  show_col_types = FALSE,
  col_types = cols(ecosystem_class = col_character(),
                   tropical_moist_forest = col_logical(),
                   .default = col_character())
)

source(file.path(dir_project, "R", "theme.R"))   # eco_group_levels

valid <- countries %>% filter(valid_for_analysis)

message("Countries used for the map: ", nrow(valid))
message("Aggregation factor: ", agg_fact)

eco_layers <- c("All natural ecosystems", eco_group_levels, "Tropical moist forest")

# "<layer>" = total constrained loss R, "<layer> | pasture" = pasture
# part of R (v3). The cropland part is the difference.
layer_names <- c(
  eco_layers,
  paste(eco_layers, "| pasture"),
  "Unconstrained loss",
  "Agricultural expansion"
)

# ---------------------------------------------------------
# ONE COUNTRY, ONE TRANSITION -> stack with layer_names
# ---------------------------------------------------------
country_stack <- function(iso3, trans) {

  iso3_lower <- tolower(iso3)
  d <- file.path(dir_bf, iso3)

  f_class <- file.path(d, paste0("natural_ecosystem_loss_agriConstrained_by_class_",
                                 iso3_lower, "_", trans, ".tif"))
  f_unc   <- file.path(d, paste0("natural_ecosystem_loss_unconstrained_",
                                 iso3_lower, "_", trans, ".tif"))
  f_gain  <- file.path(d, paste0("agriculture_gain_area_",
                                 iso3_lower, "_", trans, ".tif"))

  f_past  <- file.path(d, paste0("natural_ecosystem_loss_pasture_by_class_",
                                 iso3_lower, "_", trans, ".tif"))

  if (!all(file.exists(c(f_class, f_unc, f_gain)))) {
    warning("Missing map input for ", iso3, " ", trans)
    return(NULL)
  }

  cls <- rast(f_class)
  pst <- if (file.exists(f_past)) rast(f_past) else cls * 0   # v2: no pasture
  codes <- names(cls)

  grp <- lookup$eco_group[match(codes, lookup$ecosystem_class)]
  grp[is.na(grp)] <- "Modified (counted as natural)"
  tmf <- lookup$tropical_moist_forest[match(codes, lookup$ecosystem_class)]
  tmf[is.na(tmf)] <- FALSE

  zero <- cls[[1]] * 0

  sum_layers <- function(r, idx) {
    if (!any(idx)) return(zero)
    sum(r[[which(idx)]], na.rm = TRUE)
  }

  eco_stack <- function(r) {
    c(
      sum_layers(r, rep(TRUE, length(codes))),
      do.call(c, lapply(eco_group_levels, function(g) sum_layers(r, grp == g))),
      sum_layers(r, tmf)
    )
  }

  layers <- c(
    eco_stack(cls),
    eco_stack(pst),
    rast(f_unc),
    rast(f_gain)
  )

  names(layers) <- layer_names
  layers
}

# ---------------------------------------------------------
# HELPERS
# Country rasters are crops of the same LandSHIFT grid, so
# they align. Instead of terra::mosaic over 151 multi-layer
# rasters (crashed with "Floating point exception"), the
# country values are added into one global matrix. Border
# cells that belong to two countries are summed (as in the
# country totals).
# ---------------------------------------------------------
to_web <- function(r, fun, method, filename) {
  if (agg_fact > 1) {
    r <- aggregate(r, fact = agg_fact, fun = fun, na.rm = TRUE)
  }
  r <- project(r, "EPSG:3857", method = method)
  writeRaster(r, filename, overwrite = TRUE,
              gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2"),
              datatype = if (fun == "modal") "INT2U" else "FLT4S")
}

# Cell numbers of raster s inside the global template (row-major,
# same order as values(s)); NULL if s is not on the template grid.
cells_in_template <- function(s, tmpl) {
  if (!isTRUE(all.equal(res(s), res(tmpl), tolerance = 1e-6))) return(NULL)
  r0 <- rowFromY(tmpl, yFromRow(s, 1))
  c0 <- colFromX(tmpl, xFromCol(s, 1))
  if (is.na(r0) || is.na(c0)) return(NULL)
  rows <- r0 + seq_len(nrow(s)) - 1
  cols <- c0 + seq_len(ncol(s)) - 1
  if (max(rows) > nrow(tmpl) || max(cols) > ncol(tmpl)) return(NULL)
  as.vector(t(outer((rows - 1) * ncol(tmpl), cols, "+")))
}

# ---------------------------------------------------------
# GLOBAL TEMPLATE (union of all country extents)
# ---------------------------------------------------------
country_index <- valid %>%
  transmute(country_id = row_number(), iso3,
            country_name = coalesce(country_name, iso3))

write_csv(country_index, file.path(dir_out, "country_index.csv"))

f_first <- function(iso3) {
  file.path(dir_bf, iso3, paste0("natural_ecosystem_loss_unconstrained_",
                                 tolower(iso3), "_", transitions[1], ".tif"))
}

have <- file.exists(vapply(country_index$iso3, f_first, character(1)))
exts <- lapply(country_index$iso3[have], function(i) ext(rast(f_first(i))))
e_all <- Reduce(terra::union, exts)

r_ref <- rast(f_first(country_index$iso3[have][1]))
tmpl  <- rast(extend(rast(r_ref), e_all, snap = "out"))   # empty template, same grid

message("Global template: ", nrow(tmpl), " x ", ncol(tmpl), " cells")

# ---------------------------------------------------------
# COUNTRY ID RASTER (for commodity weights in the app)
# ---------------------------------------------------------
message("Building country ID raster ...")

id_vals <- rep(NA_integer_, ncell(tmpl))

for (i in which(have)) {
  r  <- rast(f_first(country_index$iso3[i]))
  cl <- cells_in_template(r, tmpl)
  if (is.null(cl)) {
    warning("Not on template grid: ", country_index$iso3[i]); next
  }
  v  <- values(r, mat = FALSE)
  ok <- !is.na(v) & is.na(id_vals[cl])
  id_vals[cl[ok]] <- country_index$country_id[i]
}

id_full <- rast(tmpl)
values(id_full) <- id_vals
names(id_full) <- "country_id"

to_web(id_full, fun = "modal", method = "near",
       filename = file.path(dir_out, "country_id.tif"))

rm(id_vals); gc()

# ---------------------------------------------------------
# LOSS LAYERS PER TRANSITION
# ---------------------------------------------------------
n_lyr <- length(layer_names)

for (trans in transitions) {

  message("Building map layers for ", trans, " ...")

  acc  <- matrix(0, nrow = ncell(tmpl), ncol = n_lyr)
  n_ok <- 0

  for (k in seq_along(valid$iso3)) {

    iso3 <- valid$iso3[k]
    message("  [", k, "/", nrow(valid), "] ", iso3)

    s <- tryCatch(country_stack(iso3, trans), error = function(e) {
      warning("Failed ", iso3, " ", trans, ": ", conditionMessage(e))
      NULL
    })
    if (is.null(s)) next

    cl <- cells_in_template(s, tmpl)
    if (is.null(cl)) {
      warning("Not on template grid: ", iso3, " ", trans); next
    }

    v <- values(s, mat = TRUE)
    v[is.na(v)] <- 0
    acc[cl, ] <- acc[cl, ] + v
    n_ok <- n_ok + 1

    rm(s, v); gc(verbose = FALSE)
  }

  full <- rast(tmpl, nlyrs = n_lyr)
  values(full) <- acc
  names(full) <- layer_names
  full <- mask(full, id_full)

  message("  countries: ", n_ok,
          " | global sum (all natural ecosystems): ", round(sum(acc[, 1])), " km²")

  rm(acc); gc()

  to_web(full, fun = "sum", method = "near",
         filename = file.path(dir_out, paste0("loss_", trans, ".tif")))

  rm(full); gc()
}

# ---------------------------------------------------------
# TABLES FOR THE APP
# ---------------------------------------------------------
invisible(file.copy(file.path(dir_project, "data", "country_commodity.csv"),
          file.path(dir_out, "country_commodity.csv"), overwrite = TRUE))
invisible(file.copy(file.path(dir_project, "data", "country_ecosystem.csv"),
          file.path(dir_out, "country_ecosystem.csv"), overwrite = TRUE))
invisible(file.copy(file.path(dir_project, "data", "country_transition.csv"),
          file.path(dir_out, "country_transition.csv"), overwrite = TRUE))
invisible(file.copy(file.path(dir_project, "R", "theme.R"),
          file.path(dir_out, "theme.R"), overwrite = TRUE))

writeLines(layer_names, file.path(dir_out, "layer_names.txt"))

unlink(dir_tmp, recursive = TRUE)

sz <- sum(file.size(list.files(dir_out, full.names = TRUE, recursive = TRUE)))
message("Map data written to ", dir_out, " (", round(sz / 1e6, 1), " MB)")
