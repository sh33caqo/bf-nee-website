library(terra)
library(rnaturalearth)
library(sf)

# ---------------------------------------------------------
# RELEVANT YEARS
# ---------------------------------------------------------
years_match <- c(1995, 2000, 2005, 2010, 2015)

# ---------------------------------------------------------
# DIRECTORIES
# ---------------------------------------------------------
dir_ls_countries        <- "~/Symobio/Data/Processed/LandSHIFT_Countries"
dir_globes_ls_countries <- "~/Symobio/Data/Processed/GlobES_on_LandSHIFT_Countries"
dir_out_base            <- "~/Symobio/Data/Processed/GlobES_on_LandSHIFT_agAdjusted_v3_Countries"

dir.create(dir_out_base, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# LOAD COUNTRY BOUNDARIES
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
# DEFINE CLASS GROUPS
# ---------------------------------------------------------
# Cropland classes  <- LandSHIFT cropland (IR + RF)
ag_codes <- c("14.1", "15.8")

# Pasture class     <- LandSHIFT intensive pasture
pasture_codes <- c("14.2")

exclude_codes <- c("14.4", "14.5", "15.4", "15.5", "15.6", "15.11")

# ---------------------------------------------------------
# HELPER FUNCTIONS
# ---------------------------------------------------------
get_ls_ag_layer <- function(share_agriculture, year, prefix = "AGRI_SHARE_") {
  lyr_name <- paste0(prefix, year)

  if (!(lyr_name %in% names(share_agriculture))) {
    stop("Layer not found in LandSHIFT share stack: ", lyr_name)
  }

  share_agriculture[[lyr_name]]
}

# Adds zero layers for classes that are missing in the GlobES stack
# (e.g. no 14.2 in a country), so LandSHIFT pasture can be assigned.
add_missing_classes <- function(r, codes) {
  missing <- setdiff(codes, names(r))
  for (nm in missing) {
    z <- r[[1]] * 0
    names(z) <- nm
    r <- c(r, z)
  }
  r
}

get_layer_indices_strict <- function(r, codes) {
  idx <- match(codes, names(r))

  if (any(is.na(idx))) {
    stop(
      "Missing required class codes: ",
      paste(codes[is.na(idx)], collapse = ", ")
    )
  }

  idx
}

get_layer_indices_soft <- function(r, codes) {
  idx <- match(codes, names(r))
  idx[!is.na(idx)]
}

# ---------------------------------------------------------
# STEP 1: REMOVE EXCLUDED CLASSES + RENORMALIZE
# ---------------------------------------------------------
remove_excluded_and_renormalize <- function(globes_stack, exclude_idx) {

  fun <- function(v) {
    if (any(is.na(v))) return(rep(NA_real_, length(v)))

    out <- v

    if (length(exclude_idx) > 0) {
      out[exclude_idx] <- 0
    }

    keep_idx <- if (length(exclude_idx) > 0) {
      setdiff(seq_along(v), exclude_idx)
    } else {
      seq_along(v)
    }

    remain_sum <- sum(out[keep_idx])

    if (remain_sum > 0) {
      out[keep_idx] <- out[keep_idx] / remain_sum
    } else {
      out[keep_idx] <- 0
    }

    out
  }

  out <- app(globes_stack, fun = fun)
  names(out) <- names(globes_stack)

  out
}

# ---------------------------------------------------------
# STEP 2+3: MATCH CROPLAND AND INTENSIVE PASTURE TO LANDSHIFT
#
# Two separate constraints per pixel:
#   sum_{a in crop} G'_a = L_crop     (14.1, 15.8)
#   sum_{a in past} G'_a = L_past     (14.2)
# Within each set the GlobES composition is kept (q = G_a / G_set);
# if GlobES has none of the set, L is split equally.
# All other classes are rescaled with
#   alpha = (1 - L_crop - L_past) / (1 - G_crop - G_past)
# so that the pixel still sums to one. Extensive pasture therefore
# stays part of the natural GlobES cover (savanna, grassland, ...).
# ---------------------------------------------------------
adjust_agriculture_to_landshift <- function(globes_stack, ls_crop, ls_past,
                                            crop_idx, past_idx) {

  # Vectorised terra version (same result as a per-pixel function,
  # but much faster and chunk-wise for large countries).

  n_cls <- nlyr(globes_stack)
  other_idx <- setdiff(seq_len(n_cls), c(crop_idx, past_idx))

  # pixels with any NA class are NA in all layers (as before)
  valid <- !anyNA(globes_stack) & !is.na(ls_crop)

  lC <- clamp(ls_crop, 0, 1, values = TRUE)
  lP <- clamp(ifel(is.na(ls_past), 0, ls_past), 0, 1, values = TRUE)

  # LandSHIFT cropland + pasture must not exceed the cell
  lT <- lC + lP
  f  <- ifel(lT > 1, 1 / lT, 1)
  lC <- lC * f
  lP <- lP * f

  gC <- sum(globes_stack[[crop_idx]])
  gP <- sum(globes_stack[[past_idx]])
  gM <- clamp(gC + gP, 0, 1, values = TRUE)

  # rescale all other classes
  alpha <- ifel(gM < 1, (1 - lC - lP) / (1 - gM), 0)

  out_other <- globes_stack[[other_idx]] * alpha

  # cropland classes (keep GlobES composition, else equal split)
  out_crop <- ifel(gC > 0, globes_stack[[crop_idx]] / gC * lC, lC / length(crop_idx))

  # intensive pasture classes
  out_past <- ifel(gP > 0, globes_stack[[past_idx]] / gP * lP, lP / length(past_idx))

  out <- c(out_other, out_crop, out_past)
  names(out) <- names(globes_stack)[c(other_idx, crop_idx, past_idx)]
  out <- out[[names(globes_stack)]]

  # cleanup + renormalise
  out <- clamp(out, 0, 1, values = TRUE)
  s <- sum(out)
  out <- ifel(s > 0, out / s, out)

  out <- mask(out, valid, maskvalues = c(0, NA))
  names(out) <- names(globes_stack)

  out
}

# ---------------------------------------------------------
# PROCESS ONE COUNTRY
# ---------------------------------------------------------
process_country_ag_adjusted <- function(country_sf) {

  iso3 <- country_sf$iso_a3[1]
  country_name <- country_sf$name_long[1]
  suffix <- paste0("_", tolower(iso3))

  message("--------------------------------------------------")
  message("Processing ag-adjusted GlobES for ", country_name, " (", iso3, ")")
  message("--------------------------------------------------")

  dir_ls_country        <- file.path(dir_ls_countries, iso3)
  dir_globes_ls_country <- file.path(dir_globes_ls_countries, iso3)
  dir_out_adj           <- file.path(dir_out_base, iso3)

  dir.create(dir_out_adj, recursive = TRUE, showWarnings = FALSE)

  # ---------------------------------------------------------
  # REQUIRED INPUTS
  # ---------------------------------------------------------
  f_share_agriculture <- file.path(
    dir_ls_country,
    paste0("share_agriculture_1995_2050", suffix, ".tif")
  )

  f_share_pasture <- file.path(
    dir_ls_country,
    paste0("share_pasture_int_1995_2050", suffix, ".tif")
  )

  if (!file.exists(f_share_agriculture)) {
    stop("Missing LandSHIFT cropland share file: ", f_share_agriculture)
  }

  if (!file.exists(f_share_pasture)) {
    stop("Missing LandSHIFT intensive pasture share file (run step 01 v3): ", f_share_pasture)
  }

  share_agriculture <- rast(f_share_agriculture)
  share_pasture     <- rast(f_share_pasture)

  country_vect <- vect(country_sf)
  country_vect <- project(country_vect, crs(share_agriculture))

  globes_share_ls_agAdjusted <- list()

  diagnostics <- list()
  failed_years <- list()

  # ---------------------------------------------------------
  # LOOP OVER YEARS
  # ---------------------------------------------------------
  for (yr in years_match) {

    message("Processing year ", yr, " for ", iso3)

    f_globes_ls <- file.path(
      dir_globes_ls_country,
      paste0("globes_share_ls_", yr, suffix, ".tif")
    )

    if (!file.exists(f_globes_ls)) {
      warning("Missing GlobES-on-LandSHIFT file: ", f_globes_ls)
      failed_years[[as.character(yr)]] <- "missing_globes_share_ls_file"
      next
    }

    f_out_year <- file.path(dir_out_adj, paste0("globes_agAdjusted_", yr, suffix, ".tif"))

    # Resume: a finished year (newer than the pasture input) is reused,
    # so a job that hit the time limit continues with the next year.
    if (file.exists(f_out_year) && file.mtime(f_out_year) > file.mtime(f_share_pasture)) {
      message("Reusing finished year ", yr, " for ", iso3)
      globes_share_ls_agAdjusted[[as.character(yr)]] <- rast(f_out_year)
      failed_years[[as.character(yr)]] <- NA_character_
      next
    }

    ok <- tryCatch({

      g  <- rast(f_globes_ls)
      lA <- get_ls_ag_layer(share_agriculture, yr)
      lP <- get_ls_ag_layer(share_pasture, yr, prefix = "PAST_SHARE_")

      compareGeom(g, lA, stopOnError = TRUE)
      compareGeom(g, lP, stopOnError = TRUE)

      g  <- mask(g, country_vect)
      lA <- mask(lA, country_vect)
      lP <- mask(lP, country_vect)

      # make sure the pasture class exists in the stack
      missing_past <- setdiff(pasture_codes, names(g))
      if (length(missing_past) > 0) {
        message("Adding zero layer for missing pasture class(es) ",
                paste(missing_past, collapse = ", "), " (", iso3, " ", yr, ")")
        g <- add_missing_classes(g, pasture_codes)
      }

      # indices
      ag_idx       <- get_layer_indices_strict(g, ag_codes)
      past_idx     <- get_layer_indices_strict(g, pasture_codes)
      exclude_idx  <- get_layer_indices_soft(g, exclude_codes)
      missing_excl <- setdiff(exclude_codes, names(g))

      if (length(missing_excl) > 0) {
        message(
          "Missing excluded classes for ",
          iso3,
          " in ",
          yr,
          ": ",
          paste(missing_excl, collapse = ", ")
        )
      }

      # step 1
      g_clean <- remove_excluded_and_renormalize(g, exclude_idx)

      # step 2+3
      g_adj <- adjust_agriculture_to_landshift(g_clean, lA, lP, ag_idx, past_idx)

      globes_share_ls_agAdjusted[[as.character(yr)]] <- g_adj

      writeRaster(
        g_adj,
        file.path(
          dir_out_adj,
          paste0("globes_agAdjusted_", yr, suffix, ".tif")
        ),
        overwrite = TRUE
      )

      # diagnostics: sum across all classes should be close to 1
      s <- app(g_adj, sum, na.rm = TRUE)
      sum_stats <- global(s, c("min", "max", "mean"), na.rm = TRUE)

      # diagnostics: adjusted cropland / pasture should match LandSHIFT
      # (differences only where LandSHIFT crop + pasture > 1 was capped)
      gA_new <- sum(g_adj[[ag_idx]])
      diff_r <- gA_new - lA
      diff_stats <- global(diff_r, c("min", "max", "mean"), na.rm = TRUE)

      gP_new <- sum(g_adj[[past_idx]])
      diff_p <- gP_new - lP
      diff_p_stats <- global(diff_p, c("min", "max", "mean"), na.rm = TRUE)

      # NOTE: plain assignment (the tryCatch body runs in this function's
      # environment; "<<-" would write to the global environment)
      diagnostics[[as.character(yr)]] <- list(
        year = yr,
        missing_excluded_classes = missing_excl,
        sum_stats = sum_stats,
        agriculture_difference_stats = diff_stats,
        pasture_difference_stats = diff_p_stats
      )

      write.csv(
        as.data.frame(diff_p_stats),
        file.path(
          dir_out_adj,
          paste0("globes_agAdjusted_pasture_diff_stats_", yr, suffix, ".csv")
        ),
        row.names = TRUE
      )

      # save diagnostics as CSV
      write.csv(
        as.data.frame(sum_stats),
        file.path(
          dir_out_adj,
          paste0("globes_agAdjusted_sum_stats_", yr, suffix, ".csv")
        ),
        row.names = TRUE
      )

      write.csv(
        as.data.frame(diff_stats),
        file.path(
          dir_out_adj,
          paste0("globes_agAdjusted_ag_diff_stats_", yr, suffix, ".csv")
        ),
        row.names = TRUE
      )

      # frequency table of sum raster
      freq_s <- freq(s)

      write.csv(
        as.data.frame(freq_s),
        file.path(
          dir_out_adj,
          paste0("globes_agAdjusted_sum_freq_", yr, suffix, ".csv")
        ),
        row.names = FALSE
      )

      failed_years[[as.character(yr)]] <- NA_character_

      TRUE

    }, error = function(e) {

      warning("Failed ag adjustment for ", iso3, " in ", yr, ": ", conditionMessage(e))
      failed_years[[as.character(yr)]] <<- conditionMessage(e)
      FALSE
    })

    gc()
  }

  # ---------------------------------------------------------
  # SAVE RDS OUTPUTS
  # ---------------------------------------------------------
  saveRDS(
    globes_share_ls_agAdjusted,
    file = file.path(
      dir_out_adj,
      paste0("globes_share_ls_agAdjusted", suffix, ".rds")
    )
  )

  saveRDS(
    diagnostics,
    file = file.path(
      dir_out_adj,
      paste0("globes_agAdjusted_diagnostics", suffix, ".rds")
    )
  )

  saveRDS(
    failed_years,
    file = file.path(
      dir_out_adj,
      paste0("globes_agAdjusted_failed_years", suffix, ".rds")
    )
  )

  failed_df <- data.frame(
    year = names(failed_years),
    error = unlist(failed_years),
    stringsAsFactors = FALSE
  )

  failed_df <- failed_df[!is.na(failed_df$error), ]

  write.csv(
    failed_df,
    file.path(
      dir_out_adj,
      paste0("globes_agAdjusted_failed_years", suffix, ".csv")
    ),
    row.names = FALSE
  )

  # ---------------------------------------------------------
  # SAVE METADATA
  # ---------------------------------------------------------
  saveRDS(
    list(
      years_match = years_match,
      mode = "country",
      country_iso3 = iso3,
      country_name = country_name,
      boundary_source = "rnaturalearth::ne_countries(scale = 'medium')",
      ag_codes = ag_codes,
      pasture_codes = pasture_codes,
      exclude_codes = exclude_codes,
      landshift_agriculture_share_file = f_share_agriculture,
      landshift_pasture_share_file = f_share_pasture,
      globes_on_landshift_dir = dir_globes_ls_country,
      note = "v3: Excluded selected GlobES classes, renormalized, then set GlobES cropland classes (14.1, 15.8) to LandSHIFT cropland and GlobES pastureland (14.2) to LandSHIFT intensive pasture; all other classes rescaled."
    ),
    file = file.path(
      dir_out_adj,
      paste0("globes_agAdjusted_metadata", suffix, ".rds")
    )
  )

  n_failed_years <- nrow(failed_df)

  message("Finished ag-adjusted GlobES for ", country_name, " (", iso3, ")")

  return(data.frame(
    iso3 = iso3,
    country_name = country_name,
    status = ifelse(n_failed_years == length(years_match), "failed", "success"),
    n_failed_years = n_failed_years,
    error = ifelse(n_failed_years == length(years_match), "all years failed", NA_character_)
  ))
}

# # ---------------------------------------------------------
# # LOOP OVER ALL COUNTRIES
# # ---------------------------------------------------------
# results <- list()
# 
# for (i in seq_len(nrow(world_sf))) {
#   
#   iso3 <- world_sf$iso_a3[i]
#   
#   result <- tryCatch(
#     {
#       process_country_ag_adjusted(world_sf[i, ])
#     },
#     error = function(e) {
#       
#       warning("Failed country ", iso3, ": ", conditionMessage(e))
#       
#       data.frame(
#         iso3 = iso3,
#         country_name = world_sf$name_long[i],
#         status = "failed",
#         n_failed_years = length(years_match),
#         error = conditionMessage(e)
#       )
#     }
#   )
#   
#   results[[iso3]] <- result
# }
# 
# # ---------------------------------------------------------
# # SAVE PROCESSING SUMMARY
# # ---------------------------------------------------------
# results_df <- do.call(rbind, results)
# 
# write.csv(
#   results_df,
#   file.path(
#     dir_out_base,
#     "globes_agAdjusted_country_processing_summary.csv"
#   ),
#   row.names = FALSE
# )
# 
# print(results_df)
# table(results_df$status)