# load libraries
library(terra)
library(stringr)
library(rnaturalearth)
library(sf)

# ---------------------------------------------------------
# SET DIRECTORIES
# ---------------------------------------------------------
dir_ls_basemap       <- "~/Symobio/Data/LandSHIFT/history_raw_var3_small//history_raw_var3_small/BASEMAP"
dir_ls_FractionIR    <- "~/Symobio/Data/LandSHIFT/merge_var3/FractionIR_allCell"
dir_ls_FractionRF    <- "~/Symobio/Data/LandSHIFT/merge_var3/FractionRF_allCell"
dir_ls_FractionGraze <- "~/Symobio/Data/LandSHIFT/merge_var3/FractionGraze_allCell"
dir_ls_LUDENS        <- "~/Symobio/Data/LandSHIFT/merge_var3/LUDENS"

dir_out_base <- "~/Symobio/Data/Processed/LandSHIFT_Countries"
dir.create(dir_out_base, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# DEFINE RELEVANT YEARS
# ---------------------------------------------------------
years <- seq(1995, 2050, by = 5)

# Years used to derive the (fixed) country-specific stocking-rate
# threshold that separates intensive from extensive pasture
years_threshold <- c(1995, 2000, 2005, 2010, 2015)

# ---------------------------------------------------------
# LOAD GLOBAL CELL AREA ONCE
# LandSHIFT CellArea is stored as km2 * 10000
# ---------------------------------------------------------
f_area <- file.path(dir_ls_basemap, "BASEMAP2015CellArea.asc")
cell_area_global <- rast(f_area)

# ---------------------------------------------------------
# LOAD WORLD COUNTRY BOUNDARIES
# ---------------------------------------------------------
world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world_sf <- world_sf[!is.na(world_sf$iso_a3), ]
world_sf <- world_sf[world_sf$iso_a3 != "-99", ]
world_sf <- world_sf[world_sf$iso_a3 != "ATA", ]
world_sf <- world_sf[order(world_sf$iso_a3), ]

# ---------------------------------------------------------
# FUNCTION TO LOAD FRACTION DATA
# ---------------------------------------------------------
load_fraction_stack <- function(dir_path, prefix, years, country_vect = NULL) {
  
  files <- list.files(dir_path, pattern = "\\.asc$", full.names = TRUE)
  if (length(files) == 0) stop("No .asc files found in: ", dir_path)
  
  yrs <- as.integer(stringr::str_extract(basename(files), "\\d{4}"))
  
  keep <- !is.na(yrs) & (yrs %in% years)
  files <- files[keep]
  yrs   <- yrs[keep]
  
  missing <- setdiff(years, sort(unique(yrs)))
  if (length(missing) > 0) {
    stop(prefix, ": Missing years in folder: ", paste(missing, collapse = ", "))
  }
  
  ord <- order(yrs)
  files <- files[ord]
  yrs   <- yrs[ord]
  
  r <- rast(files)
  names(r) <- paste0(prefix, "_", yrs)
  
  if (!is.null(country_vect)) {
    country_vect <- project(country_vect, crs(r))
    r <- crop(r, country_vect)
    r <- mask(r, country_vect)
  }
  
  return(r)
}

# ---------------------------------------------------------
# PROCESS ONE COUNTRY
# ---------------------------------------------------------
process_country_landshift <- function(country_sf) {
  
  iso3 <- country_sf$iso_a3[1]
  country_name <- country_sf$name_long[1]
  
  message("--------------------------------------------------")
  message("Processing ", country_name, " (", iso3, ")")
  message("--------------------------------------------------")
  
  dir_out_ls <- file.path(dir_out_base, iso3)
  dir.create(dir_out_ls, recursive = TRUE, showWarnings = FALSE)
  
  suffix <- paste0("_", tolower(iso3))
  country_vect <- vect(country_sf)
  
  # ---------------------------------------------------------
  # CELL AREA FOR COUNTRY
  # ---------------------------------------------------------
  country_vect_area <- project(country_vect, crs(cell_area_global))
  
  cell_area <- crop(cell_area_global, country_vect_area)
  cell_area <- mask(cell_area, country_vect_area)
  
  # IMPORTANT:
  # LandSHIFT CellArea is stored as km2 * 10000.
  # Convert to km2 before any area calculations.
  cell_area <- cell_area / 10000
  
  n_cells <- global(!is.na(cell_area), "sum", na.rm = TRUE)[1, 1]
  
  if (is.na(n_cells) || n_cells == 0) {
    stop("No raster cells found after masking.")
  }
  
  # ---------------------------------------------------------
  # LOAD FRACTION DATA
  # ---------------------------------------------------------
  frac_ir <- load_fraction_stack(
    dir_ls_FractionIR,
    "IR",
    years,
    country_vect = country_vect
  )
  
  frac_rf <- load_fraction_stack(
    dir_ls_FractionRF,
    "RF",
    years,
    country_vect = country_vect
  )
  
  frac_graze <- load_fraction_stack(
    dir_ls_FractionGraze,
    "GRAZE",
    years,
    country_vect = country_vect
  )
  
  ludens <- load_fraction_stack(
    dir_ls_LUDENS,
    "LUDENS",
    years,
    country_vect = country_vect
  )
  
  # LandSHIFT fraction data is stored as 0-10000.
  # Convert to shares, 0-1.
  frac_ir    <- frac_ir / 10000
  frac_rf    <- frac_rf / 10000
  frac_graze <- frac_graze / 10000
  
  # LUDENS (livestock density) is stored as stocking rate * 10000
  # (e.g. 200 = 0.02 LU/ha, cf. mediumStockingRate in Scen_land).
  # Negative values are NODATA.
  ludens_names <- names(ludens)
  ludens <- ifel(ludens < 0, NA, ludens)
  names(ludens) <- ludens_names
  
  # ---------------------------------------------------------
  # PLAUSIBILITY CHECKS
  # ---------------------------------------------------------
  range_cell_area <- global(cell_area, c("min", "max", "mean", "sum"), na.rm = TRUE)
  range_ir        <- global(frac_ir, "range", na.rm = TRUE)
  range_rf        <- global(frac_rf, "range", na.rm = TRUE)
  range_graze     <- global(frac_graze, "range", na.rm = TRUE)
  
  write.csv(
    as.data.frame(range_cell_area),
    file.path(dir_out_ls, paste0("range_cell_area_km2", suffix, ".csv")),
    row.names = TRUE
  )
  
  write.csv(
    as.data.frame(range_ir),
    file.path(dir_out_ls, paste0("range_frac_ir", suffix, ".csv")),
    row.names = TRUE
  )
  
  write.csv(
    as.data.frame(range_rf),
    file.path(dir_out_ls, paste0("range_frac_rf", suffix, ".csv")),
    row.names = TRUE
  )
  
  write.csv(
    as.data.frame(range_graze),
    file.path(dir_out_ls, paste0("range_frac_graze", suffix, ".csv")),
    row.names = TRUE
  )
  
  # ---------------------------------------------------------
  # GEOMETRY CHECK
  # ---------------------------------------------------------
  compareGeom(frac_ir, cell_area, stopOnError = TRUE)
  
  # ---------------------------------------------------------
  # CALCULATE PIXEL-SPECIFIC AGRICULTURAL AREAS
  # Units: km2
  # ---------------------------------------------------------
  area_ir    <- frac_ir * cell_area
  area_rf    <- frac_rf * cell_area
  area_graze <- frac_graze * cell_area
  
  # as before: cropland = IR + RF
  area_agriculture <- area_ir + area_rf
  names(area_agriculture) <- paste0("AGRI_", years)
  
  share_agriculture <- area_agriculture / cell_area
  names(share_agriculture) <- paste0("AGRI_SHARE_", years)
  
  # ---------------------------------------------------------
  # INTENSIVE PASTURE
  #
  # LandSHIFT separates "artificial" (intensive) from "natural"
  # (extensive) pasture only by stocking density, using the
  # country mean as threshold.
  #
  # Threshold (FIXED per country, pooled over years_threshold):
  #   thr_r = sum_p,y (LUDENS_p,y * A_graze_p,y) / sum_p,y A_graze_p,y
  # i.e. the mean stocking density of the grazing land.
  # A fixed threshold avoids cells switching between intensive
  # and extensive only because the country mean shifts slightly.
  #
  # Intensive pasture fraction:
  #   F_past_p,y = F_graze_p,y   if LUDENS_p,y > thr_r
  #              = 0             otherwise
  # Extensive pasture stays part of the natural GlobES cover.
  # ---------------------------------------------------------
  thr_layers <- paste0("_", years_threshold)
  
  idx_thr_g <- which(names(area_graze) %in% paste0("GRAZE", thr_layers))
  idx_thr_l <- which(names(ludens) %in% paste0("LUDENS", thr_layers))
  
  ag_thr <- area_graze[[idx_thr_g]]
  lu_thr <- ludens[[idx_thr_l]]
  
  ag_thr <- ifel(is.na(lu_thr) | lu_thr <= 0, 0, ag_thr)
  
  sum_lu_area <- sum(global(lu_thr * ag_thr, "sum", na.rm = TRUE)[, 1], na.rm = TRUE)
  sum_area    <- sum(global(ag_thr, "sum", na.rm = TRUE)[, 1], na.rm = TRUE)
  
  stocking_threshold <- if (is.finite(sum_area) && sum_area > 0) {
    sum_lu_area / sum_area
  } else {
    NA_real_
  }
  
  message(
    "Stocking-rate threshold (mean of grazing land) for ", iso3, ": ",
    round(stocking_threshold, 2), " (= ", signif(stocking_threshold / 10000, 3), " LU/ha)"
  )
  
  if (is.na(stocking_threshold)) {
    frac_pasture_int <- frac_graze * 0
  } else {
    frac_pasture_int <- ifel(!is.na(ludens) & ludens > stocking_threshold, frac_graze, 0)
    frac_pasture_int <- mask(frac_pasture_int, frac_graze)
  }
  names(frac_pasture_int) <- paste0("PAST_INT_", years)
  
  # Guard: cropland + intensive pasture must not exceed the cell
  frac_total <- frac_ir + frac_rf + frac_pasture_int
  excess <- ifel(frac_total > 1, frac_total - 1, 0)
  frac_pasture_int <- frac_pasture_int - excess
  frac_pasture_int <- ifel(frac_pasture_int < 0, 0, frac_pasture_int)
  
  area_pasture_int <- frac_pasture_int * cell_area
  names(area_pasture_int) <- paste0("PAST_INT_", years)
  
  share_pasture_int <- area_pasture_int / cell_area
  names(share_pasture_int) <- paste0("PAST_SHARE_", years)
  
  # Total agricultural land = cropland + intensive pasture
  share_agri_total <- share_agriculture + share_pasture_int
  names(share_agri_total) <- paste0("AGRI_TOTAL_SHARE_", years)
  
  pasture_summary <- data.frame(
    country_iso3 = iso3,
    country_name = country_name,
    year = years,
    stocking_threshold_ludens = stocking_threshold,
    stocking_threshold_lu_per_ha = stocking_threshold / 10000,
    graze_area_km2 = as.numeric(global(area_graze, "sum", na.rm = TRUE)[, 1]),
    pasture_int_area_km2 = as.numeric(global(area_pasture_int, "sum", na.rm = TRUE)[, 1]),
    cropland_area_km2 = as.numeric(global(area_agriculture, "sum", na.rm = TRUE)[, 1])
  )
  
  pasture_summary$share_graze_intensive <- ifelse(
    pasture_summary$graze_area_km2 > 0,
    pasture_summary$pasture_int_area_km2 / pasture_summary$graze_area_km2,
    NA_real_
  )
  
  write.csv(
    pasture_summary,
    file.path(dir_out_ls, paste0("pasture_intensive_summary", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------
  # DIAGNOSTICS
  # ---------------------------------------------------------
  range_share_agriculture <- global(share_agriculture, "range", na.rm = TRUE)
  mean_share_agriculture  <- global(share_agriculture, "mean",  na.rm = TRUE)
  
  write.csv(
    as.data.frame(range_share_agriculture),
    file.path(dir_out_ls, paste0("range_share_agriculture", suffix, ".csv")),
    row.names = TRUE
  )
  
  write.csv(
    as.data.frame(mean_share_agriculture),
    file.path(dir_out_ls, paste0("mean_share_agriculture", suffix, ".csv")),
    row.names = TRUE
  )
  
  # ---------------------------------------------------------
  # SAVE RESULTS
  # ---------------------------------------------------------
  writeRaster(
    cell_area,
    filename = file.path(dir_out_ls, paste0("cell_area", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    frac_ir,
    filename = file.path(dir_out_ls, paste0("frac_ir_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    frac_rf,
    filename = file.path(dir_out_ls, paste0("frac_rf_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    frac_graze,
    filename = file.path(dir_out_ls, paste0("frac_graze_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    area_ir,
    filename = file.path(dir_out_ls, paste0("area_ir_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    area_rf,
    filename = file.path(dir_out_ls, paste0("area_rf_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    area_graze,
    filename = file.path(dir_out_ls, paste0("area_graze_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    area_agriculture,
    filename = file.path(dir_out_ls, paste0("area_agriculture_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    share_agriculture,
    filename = file.path(dir_out_ls, paste0("share_agriculture_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    ludens,
    filename = file.path(dir_out_ls, paste0("ludens_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    frac_pasture_int,
    filename = file.path(dir_out_ls, paste0("frac_pasture_int_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    area_pasture_int,
    filename = file.path(dir_out_ls, paste0("area_pasture_int_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    share_pasture_int,
    filename = file.path(dir_out_ls, paste0("share_pasture_int_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    share_agri_total,
    filename = file.path(dir_out_ls, paste0("share_agri_total_1995_2050", suffix, ".tif")),
    overwrite = TRUE
  )
  
  saveRDS(
    list(
      years = years,
      mode = "country",
      country_iso3 = iso3,
      country_name = country_name,
      boundary_source = "rnaturalearth::ne_countries(scale = 'medium')",
      n_valid_cells = n_cells,
      dir_ls_basemap = dir_ls_basemap,
      dir_ls_FractionIR = dir_ls_FractionIR,
      dir_ls_FractionRF = dir_ls_FractionRF,
      dir_ls_FractionGraze = dir_ls_FractionGraze,
      dir_ls_LUDENS = dir_ls_LUDENS,
      stocking_threshold_ludens = stocking_threshold,
      stocking_threshold_years = years_threshold,
      pasture_definition = "intensive pasture = FractionGraze where LUDENS > fixed country mean stocking density of grazing land (area-weighted, 1995-2015)",
      share_agriculture_definition = "share_agriculture = cropland (IR + RF); share_pasture_int = intensive pasture; share_agri_total = both",
      cell_area_unit = "km2",
      cell_area_conversion = "BASEMAP2015CellArea.asc / 10000",
      fraction_unit = "share",
      fraction_conversion = "LandSHIFT fraction rasters / 10000"
    ),
    file = file.path(dir_out_ls, paste0("landshift_metadata", suffix, ".rds"))
  )
  
  message("Finished ", country_name, " (", iso3, ")")
  
  return(data.frame(
    iso3 = iso3,
    country_name = country_name,
    status = "success",
    n_valid_cells = n_cells,
    error = NA_character_
  ))
}

# ---------------------------------------------------------
# LOOP OVER ALL COUNTRIES
# ---------------------------------------------------------
# results <- list()
# 
# for (i in seq_len(nrow(world_sf))) {
#   
#   iso3 <- world_sf$iso_a3[i]
#   
#   result <- tryCatch(
#     {
#       process_country_landshift(world_sf[i, ])
#     },
#     error = function(e) {
#       
#       warning("Failed for ", iso3, ": ", conditionMessage(e))
#       
#       data.frame(
#         iso3 = iso3,
#         country_name = world_sf$name_long[i],
#         status = "failed",
#         n_valid_cells = NA_real_,
#         error = conditionMessage(e)
#       )
#     }
#   )
#   
#   results[[iso3]] <- result
# }
# 
# results_df <- do.call(rbind, results)
# 
# write.csv(
#   results_df,
#   file.path(dir_out_base, "landshift_country_processing_summary.csv"),
#   row.names = FALSE
# )
# 
# print(results_df)
# table(results_df$status)