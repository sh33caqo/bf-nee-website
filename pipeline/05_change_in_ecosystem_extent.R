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
dir_ls_countries     <- "~/Symobio/Data/Processed/LandSHIFT_Countries"
dir_ag_adj_countries <- "~/Symobio/Data/Processed/GlobES_on_LandSHIFT_agAdjusted_v3_Countries"
dir_out_base         <- "~/Symobio/Data/Processed/GlobES_dA_v3_Countries"

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
# HELPER:
# align two area stacks to same class set
# missing classes are added as zero layers
# ---------------------------------------------------------
align_area_stacks <- function(A_prev, A_curr) {
  
  classes_all <- sort(unique(c(names(A_prev), names(A_curr))))
  
  add_missing_zero_layers <- function(r, target_names) {
    missing <- setdiff(target_names, names(r))
    
    if (length(missing) > 0) {
      for (nm in missing) {
        z <- r[[1]]
        values(z) <- 0
        names(z) <- nm
        r <- c(r, z)
      }
    }
    
    r[[target_names]]
  }
  
  A_prev_aligned <- add_missing_zero_layers(A_prev, classes_all)
  A_curr_aligned <- add_missing_zero_layers(A_curr, classes_all)
  
  list(
    A_prev = A_prev_aligned,
    A_curr = A_curr_aligned
  )
}

# ---------------------------------------------------------
# PROCESS ONE COUNTRY
# ---------------------------------------------------------
process_country_dA <- function(country_sf) {
  
  iso3 <- country_sf$iso_a3[1]
  country_name <- country_sf$name_long[1]
  suffix <- paste0("_", tolower(iso3))
  
  message("--------------------------------------------------")
  message("Processing dA for ", country_name, " (", iso3, ")")
  message("--------------------------------------------------")
  
  dir_ls_country     <- file.path(dir_ls_countries, iso3)
  dir_ag_adj_country <- file.path(dir_ag_adj_countries, iso3)
  dir_out_dA         <- file.path(dir_out_base, iso3)
  
  dir.create(dir_out_dA, recursive = TRUE, showWarnings = FALSE)
  
  # ---------------------------------------------------------
  # LOAD COUNTRY CELL AREA
  # ---------------------------------------------------------
  f_cell_area <- file.path(
    dir_ls_country,
    paste0("cell_area", suffix, ".tif")
  )
  
  if (!file.exists(f_cell_area)) {
    stop("Missing cell area file: ", f_cell_area)
  }
  
  cell_area <- rast(f_cell_area)
  cell_area[cell_area <= 0] <- NA
  
  country_vect <- vect(country_sf)
  country_vect <- project(country_vect, crs(cell_area))
  
  # ---------------------------------------------------------
  # OBJECT LISTS
  # ---------------------------------------------------------
  globes_share_ls_agAdjusted_fixed <- list()
  globes_area_ls_agAdjusted <- list()
  globes_dA_ls_agAdjusted <- list()
  
  failed_steps <- list()
  
  # ---------------------------------------------------------
  # STEP 0:
  # FIX INVALID PIXELS
  # Pixels with share sum = 0 are set to NA
  # ---------------------------------------------------------
  for (yr in years_match) {
    
    message("Checking adjusted shares for ", iso3, " in ", yr)
    
    f_g <- file.path(
      dir_ag_adj_country,
      paste0("globes_agAdjusted_", yr, suffix, ".tif")
    )
    
    if (!file.exists(f_g)) {
      warning("Missing ag-adjusted GlobES file: ", f_g)
      failed_steps[[paste0("fix_", yr)]] <- "missing_agAdjusted_file"
      next
    }
    
    ok <- tryCatch({
      
      g <- rast(f_g)
      
      compareGeom(g, cell_area, stopOnError = TRUE)
      
      g <- mask(g, country_vect)
      
      s <- app(g, sum, na.rm = TRUE)
      bad <- s == 0
      
      n_bad <- global(bad, "sum", na.rm = TRUE)[1, 1]
      message("Zero-sum pixels for ", iso3, " in ", yr, ": ", n_bad)
      
      if (!is.na(n_bad) && n_bad > 0) {
        g <- mask(g, bad, maskvalues = 1, updatevalue = NA)
      }
      
      globes_share_ls_agAdjusted_fixed[[as.character(yr)]] <- g
      
      writeRaster(
        g,
        filename = file.path(
          dir_out_dA,
          paste0("globes_share_ls_agAdjusted_fixed_", yr, suffix, ".tif")
        ),
        overwrite = TRUE
      )
      
      TRUE
      
    }, error = function(e) {
      warning("Failed fixing shares for ", iso3, " in ", yr, ": ", conditionMessage(e))
      failed_steps[[paste0("fix_", yr)]] <<- conditionMessage(e)
      FALSE
    })
    
    gc()
  }
  
  # ---------------------------------------------------------
  # STEP 1:
  # CALCULATE AREA PER PIXEL AND CLASS
  # A_(p,e,y) = share_(p,e,y) * cell_area_p
  # ---------------------------------------------------------
  for (yr in years_match) {
    
    message("Calculating area for ", iso3, " in ", yr)
    
    g <- globes_share_ls_agAdjusted_fixed[[as.character(yr)]]
    
    if (is.null(g)) {
      failed_steps[[paste0("area_", yr)]] <- "missing_fixed_share_stack"
      next
    }
    
    ok <- tryCatch({
      
      compareGeom(g, cell_area, stopOnError = TRUE)
      
      g <- mask(g, country_vect)
      
      area_stack <- g * cell_area
      names(area_stack) <- names(g)
      
      globes_area_ls_agAdjusted[[as.character(yr)]] <- area_stack
      
      writeRaster(
        area_stack,
        filename = file.path(
          dir_out_dA,
          paste0("globes_area_ls_agAdjusted_", yr, suffix, ".tif")
        ),
        overwrite = TRUE
      )
      
      area_sum <- app(area_stack, sum, na.rm = TRUE)
      
      writeRaster(
        area_sum,
        filename = file.path(
          dir_out_dA,
          paste0("globes_area_sum_", yr, suffix, ".tif")
        ),
        overwrite = TRUE
      )
      
      area_sum_stats <- global(
        area_sum - cell_area,
        c("min", "max", "mean"),
        na.rm = TRUE
      )
      
      write.csv(
        as.data.frame(area_sum_stats),
        file = file.path(
          dir_out_dA,
          paste0("globes_area_minus_cellarea_stats_", yr, suffix, ".csv")
        ),
        row.names = TRUE
      )
      
      TRUE
      
    }, error = function(e) {
      warning("Failed area calculation for ", iso3, " in ", yr, ": ", conditionMessage(e))
      failed_steps[[paste0("area_", yr)]] <<- conditionMessage(e)
      FALSE
    })
    
    gc()
  }
  
  # ---------------------------------------------------------
  # STEP 2:
  # CALCULATE dA_(p,e,y) = A_(p,e,y) - A_(p,e,y-1)
  # with aligned class sets across years
  # ---------------------------------------------------------
  for (i in 2:length(years_match)) {
    
    yr_prev <- years_match[i - 1]
    yr_curr <- years_match[i]
    transition <- paste0(yr_prev, "_", yr_curr)
    
    message("Calculating dA for ", iso3, ": ", yr_prev, " -> ", yr_curr)
    
    A_prev <- globes_area_ls_agAdjusted[[as.character(yr_prev)]]
    A_curr <- globes_area_ls_agAdjusted[[as.character(yr_curr)]]
    
    if (is.null(A_prev) || is.null(A_curr)) {
      failed_steps[[paste0("dA_", transition)]] <- "missing_area_stack"
      next
    }
    
    ok <- tryCatch({
      
      compareGeom(A_prev, A_curr, stopOnError = TRUE)
      
      aligned <- align_area_stacks(A_prev, A_curr)
      A_prev_aligned <- aligned$A_prev
      A_curr_aligned <- aligned$A_curr
      
      missing_in_prev <- setdiff(names(A_curr), names(A_prev))
      missing_in_curr <- setdiff(names(A_prev), names(A_curr))
      
      if (length(missing_in_prev) > 0) {
        message(
          "Classes missing in previous year for ",
          iso3,
          " ",
          transition,
          ": ",
          paste(missing_in_prev, collapse = ", ")
        )
      }
      
      if (length(missing_in_curr) > 0) {
        message(
          "Classes missing in current year for ",
          iso3,
          " ",
          transition,
          ": ",
          paste(missing_in_curr, collapse = ", ")
        )
      }
      
      dA <- A_curr_aligned - A_prev_aligned
      names(dA) <- names(A_curr_aligned)
      
      globes_dA_ls_agAdjusted[[transition]] <- dA
      
      writeRaster(
        dA,
        filename = file.path(
          dir_out_dA,
          paste0("globes_dA_", transition, suffix, ".tif")
        ),
        overwrite = TRUE
      )
      
      dA_sum <- app(dA, sum, na.rm = TRUE)
      
      writeRaster(
        dA_sum,
        filename = file.path(
          dir_out_dA,
          paste0("globes_dA_sum_", transition, suffix, ".tif")
        ),
        overwrite = TRUE
      )
      
      dA_sum_stats <- global(
        dA_sum,
        c("min", "max", "mean"),
        na.rm = TRUE
      )
      
      write.csv(
        as.data.frame(dA_sum_stats),
        file = file.path(
          dir_out_dA,
          paste0("globes_dA_sum_stats_", transition, suffix, ".csv")
        ),
        row.names = TRUE
      )
      
      dA_range <- global(dA, "range", na.rm = TRUE)
      
      write.csv(
        as.data.frame(dA_range),
        file = file.path(
          dir_out_dA,
          paste0("globes_dA_range_", transition, suffix, ".csv")
        ),
        row.names = TRUE
      )
      
      TRUE
      
    }, error = function(e) {
      warning("Failed dA calculation for ", iso3, " ", transition, ": ", conditionMessage(e))
      failed_steps[[paste0("dA_", transition)]] <<- conditionMessage(e)
      FALSE
    })
    
    gc()
  }
  
  # ---------------------------------------------------------
  # SAVE RDS OBJECTS
  # ---------------------------------------------------------
  saveRDS(
    globes_share_ls_agAdjusted_fixed,
    file = file.path(
      dir_out_dA,
      paste0("globes_share_ls_agAdjusted_fixed", suffix, ".rds")
    )
  )
  
  saveRDS(
    globes_area_ls_agAdjusted,
    file = file.path(
      dir_out_dA,
      paste0("globes_area_ls_agAdjusted", suffix, ".rds")
    )
  )
  
  saveRDS(
    globes_dA_ls_agAdjusted,
    file = file.path(
      dir_out_dA,
      paste0("globes_dA_ls_agAdjusted", suffix, ".rds")
    )
  )
  
  saveRDS(
    failed_steps,
    file = file.path(
      dir_out_dA,
      paste0("globes_dA_failed_steps", suffix, ".rds")
    )
  )
  
  failed_df <- data.frame(
    step = names(failed_steps),
    error = unlist(failed_steps),
    stringsAsFactors = FALSE
  )
  
  if (nrow(failed_df) > 0) {
    write.csv(
      failed_df,
      file.path(
        dir_out_dA,
        paste0("globes_dA_failed_steps", suffix, ".csv")
      ),
      row.names = FALSE
    )
  } else {
    write.csv(
      data.frame(step = character(), error = character()),
      file.path(
        dir_out_dA,
        paste0("globes_dA_failed_steps", suffix, ".csv")
      ),
      row.names = FALSE
    )
  }
  
  # ---------------------------------------------------------
  # FINAL CHECKS
  # ---------------------------------------------------------
  final_area_checks <- list()
  final_dA_checks <- list()
  
  for (yr in years_match) {
    
    A <- globes_area_ls_agAdjusted[[as.character(yr)]]
    
    if (is.null(A)) next
    
    A_sum <- app(A, sum, na.rm = TRUE)
    
    final_area_checks[[as.character(yr)]] <- global(
      A_sum - cell_area,
      c("min", "max", "mean"),
      na.rm = TRUE
    )
  }
  
  for (nm in names(globes_dA_ls_agAdjusted)) {
    
    dA <- globes_dA_ls_agAdjusted[[nm]]
    dA_sum <- app(dA, sum, na.rm = TRUE)
    
    final_dA_checks[[nm]] <- global(
      dA_sum,
      c("min", "max", "mean"),
      na.rm = TRUE
    )
  }
  
  saveRDS(
    final_area_checks,
    file = file.path(
      dir_out_dA,
      paste0("globes_dA_final_area_checks", suffix, ".rds")
    )
  )
  
  saveRDS(
    final_dA_checks,
    file = file.path(
      dir_out_dA,
      paste0("globes_dA_final_dA_checks", suffix, ".rds")
    )
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
      formula_area = "A_(p,e,y) = share_(p,e,y) * cell_area_p",
      formula_dA = "dA_(p,e,y) = A_(p,e,y) - A_(p,e,y-1)",
      share_source = "globes_agAdjusted_<year>_<iso3>.tif",
      share_source_fixed = "globes_share_ls_agAdjusted_fixed_<year>_<iso3>.tif",
      cell_area_source = f_cell_area,
      note = "Pixels with share sum = 0 were set to NA before area and dA calculation. Missing classes between years were filled with zero layers before differencing."
    ),
    file = file.path(
      dir_out_dA,
      paste0("globes_dA_metadata", suffix, ".rds")
    )
  )
  
  n_failed_steps <- length(failed_steps)
  
  message("Finished dA for ", country_name, " (", iso3, ")")
  
  return(data.frame(
    iso3 = iso3,
    country_name = country_name,
    status = ifelse(n_failed_steps == 0, "success", "partial"),
    n_failed_steps = n_failed_steps,
    error = ifelse(n_failed_steps == 0, NA_character_, "some steps failed")
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
#       process_country_dA(world_sf[i, ])
#     },
#     error = function(e) {
#       
#       warning("Failed country ", iso3, ": ", conditionMessage(e))
#       
#       data.frame(
#         iso3 = iso3,
#         country_name = world_sf$name_long[i],
#         status = "failed",
#         n_failed_steps = NA_real_,
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
#     "globes_dA_country_processing_summary.csv"
#   ),
#   row.names = FALSE
# )
# 
# print(results_df)
# table(results_df$status)



