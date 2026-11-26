library(terra)
library(rnaturalearth)
library(sf)
library(dplyr)
library(tibble)

# ---------------------------------------------------------
# TERRA / TEMP SETTINGS
# ---------------------------------------------------------
terraOptions(
  memfrac = 0.4,
  todisk = TRUE,
  progress = 1
)

tmp_base <- path.expand("~/Symobio/tmp")

Sys.setenv(
  GDAL_CACHEMAX = "512",
  CPL_TMPDIR = tmp_base,
  TMPDIR = tmp_base
)

dir.create(
  tmp_base,
  recursive = TRUE,
  showWarnings = FALSE
)

# ---------------------------------------------------------
# RELEVANT YEARS
# ---------------------------------------------------------
years_match <- c(1995, 2000, 2005, 2010, 2015)

# ---------------------------------------------------------
# DIRECTORIES
# ---------------------------------------------------------
dir_ls_countries <- path.expand(
  "~/Symobio/Data/Processed/LandSHIFT_Countries"
)

dir_globes_countries <- path.expand(
  "~/Symobio/Data/Processed/GlobES_Countries"
)

dir_out_base <- path.expand(
  "~/Symobio/Data/Processed/GlobES_on_LandSHIFT_Countries"
)

dir.create(
  dir_out_base,
  recursive = TRUE,
  showWarnings = FALSE
)

# ---------------------------------------------------------
# HELPER: REMOVE TEMP FILES
# ---------------------------------------------------------
clear_temp_directory <- function(dir_tmp) {
  
  if (!dir.exists(dir_tmp)) {
    return(invisible(TRUE))
  }
  
  tmp_files <- list.files(
    dir_tmp,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  
  if (length(tmp_files) > 0) {
    unlink(
      tmp_files,
      recursive = TRUE,
      force = TRUE
    )
  }
  
  invisible(TRUE)
}

# ---------------------------------------------------------
# PROCESS ONE COUNTRY
# ---------------------------------------------------------
process_country_globes_on_landshift <- function(country_sf) {
  
  if (nrow(country_sf) != 1) {
    stop(
      "country_sf must contain exactly one country. Rows found: ",
      nrow(country_sf)
    )
  }
  
  iso3 <- as.character(country_sf$iso_a3[1])
  country_name <- as.character(country_sf$name_long[1])
  suffix <- paste0("_", tolower(iso3))
  
  message("--------------------------------------------------")
  message(
    "Processing GlobES on LandSHIFT grid for ",
    country_name,
    " (",
    iso3,
    ")"
  )
  message("--------------------------------------------------")
  
  # -------------------------------------------------------
  # COUNTRY DIRECTORIES
  # -------------------------------------------------------
  dir_ls_country <- file.path(
    dir_ls_countries,
    iso3
  )
  
  dir_globes_country <- file.path(
    dir_globes_countries,
    iso3
  )
  
  dir_out_match <- file.path(
    dir_out_base,
    iso3
  )
  
  dir_tmp_country <- file.path(
    tmp_base,
    paste0("globes_ls_", iso3)
  )
  
  dir.create(
    dir_out_match,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    dir_tmp_country,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # -------------------------------------------------------
  # LANDSHIFT TEMPLATE
  # -------------------------------------------------------
  f_cell_area <- file.path(
    dir_ls_country,
    paste0("cell_area", suffix, ".tif")
  )
  
  if (!file.exists(f_cell_area)) {
    stop(
      "Missing LandSHIFT cell area file: ",
      f_cell_area
    )
  }
  
  cell_area <- rast(f_cell_area)
  cell_area[cell_area <= 0] <- NA
  
  ls_template <- cell_area
  ls_ext <- ext(ls_template)
  
  # Project country polygon to LandSHIFT CRS
  country_vect <- vect(country_sf)
  
  if (!same.crs(country_vect, ls_template)) {
    country_vect <- project(
      country_vect,
      crs(ls_template)
    )
  }
  
  # -------------------------------------------------------
  # RESULT CONTAINERS
  # -------------------------------------------------------
  globes_share_ls <- list()
  failed_files_all <- list()
  
  # -------------------------------------------------------
  # PROCESS YEARS
  # -------------------------------------------------------
  for (yr in years_match) {
    
    message("Processing year: ", yr, " for ", iso3)
    
    clear_temp_directory(dir_tmp_country)
    
    f_globes_stack <- file.path(
      dir_globes_country,
      paste0(
        "globes_stack_",
        yr,
        suffix,
        ".tif"
      )
    )
    
    out_stack_file <- file.path(
      dir_out_match,
      paste0(
        "globes_share_ls_",
        yr,
        suffix,
        ".tif"
      )
    )
    
    out_sum_file <- file.path(
      dir_out_match,
      paste0(
        "globes_share_sum_",
        yr,
        suffix,
        ".tif"
      )
    )
    
    out_range_file <- file.path(
      dir_out_match,
      paste0(
        "globes_share_range_",
        yr,
        suffix,
        ".csv"
      )
    )
    
    if (!file.exists(f_globes_stack)) {
      
      warning(
        "Missing GlobES stack for ",
        iso3,
        " in ",
        yr
      )
      
      failed_files_all[[as.character(yr)]] <- paste0(
        "Missing input file: ",
        f_globes_stack
      )
      
      next
    }
    
    ok <- tryCatch({
      
      # ---------------------------------------------------
      # READ STACK INFORMATION
      # ---------------------------------------------------
      r_info <- rast(f_globes_stack)
      
      n_layers <- nlyr(r_info)
      layer_names <- names(r_info)
      
      message(
        "Number of GlobES layers in ",
        yr,
        ": ",
        n_layers
      )
      
      if (
        is.null(layer_names) ||
        length(layer_names) != n_layers
      ) {
        stop(
          "Layer names missing or invalid for ",
          f_globes_stack
        )
      }
      
      processed_layer_files <- rep(
        NA_character_,
        n_layers
      )
      
      # ---------------------------------------------------
      # PROCESS EACH GLOBES LAYER
      # ---------------------------------------------------
      for (lyr in seq_len(n_layers)) {
        
        message(
          "  Processing layer ",
          lyr,
          "/",
          n_layers,
          " for ",
          iso3,
          " ",
          yr
        )
        
        tmp_crop <- file.path(
          dir_tmp_country,
          paste0(
            "crop_",
            iso3,
            "_",
            yr,
            "_layer_",
            lyr,
            ".tif"
          )
        )
        
        tmp_share <- file.path(
          dir_tmp_country,
          paste0(
            "share_",
            iso3,
            "_",
            yr,
            "_layer_",
            lyr,
            ".tif"
          )
        )
        
        tmp_resample <- file.path(
          dir_tmp_country,
          paste0(
            "resample_",
            iso3,
            "_",
            yr,
            "_layer_",
            lyr,
            ".tif"
          )
        )
        
        tmp_mask_template <- file.path(
          dir_tmp_country,
          paste0(
            "mask_template_",
            iso3,
            "_",
            yr,
            "_layer_",
            lyr,
            ".tif"
          )
        )
        
        tmp_mask_country <- file.path(
          dir_tmp_country,
          paste0(
            "mask_country_",
            iso3,
            "_",
            yr,
            "_layer_",
            lyr,
            ".tif"
          )
        )
        
        r_layer <- rast(
          f_globes_stack,
          lyrs = lyr
        )
        
        # Crop to LandSHIFT extent
        r_crop <- crop(
          r_layer,
          ls_ext,
          snap = "out",
          filename = tmp_crop,
          overwrite = TRUE
        )
        
        if (!file.exists(tmp_crop)) {
          stop(
            "Crop file was not written: ",
            tmp_crop
          )
        }
        
        # GlobES values are percentages 0–100.
        # Convert to fractional share 0–1.
        r_share <- r_crop / 100
        
        writeRaster(
          r_share,
          filename = tmp_share,
          overwrite = TRUE
        )
        
        if (!file.exists(tmp_share)) {
          stop(
            "Share file was not written: ",
            tmp_share
          )
        }
        
        r_share <- rast(tmp_share)
        
        # Resample to LandSHIFT grid
        r_resample <- resample(
          r_share,
          ls_template,
          method = "average",
          filename = tmp_resample,
          overwrite = TRUE
        )
        
        if (!file.exists(tmp_resample)) {
          stop(
            "Resample file was not written: ",
            tmp_resample
          )
        }
        
        # Mask with valid LandSHIFT cells
        r_mask_template <- mask(
          r_resample,
          ls_template,
          filename = tmp_mask_template,
          overwrite = TRUE
        )
        
        if (!file.exists(tmp_mask_template)) {
          stop(
            "Template mask file was not written: ",
            tmp_mask_template
          )
        }
        
        # Mask with country polygon
        r_mask_country <- mask(
          r_mask_template,
          country_vect,
          filename = tmp_mask_country,
          overwrite = TRUE
        )
        
        if (!file.exists(tmp_mask_country)) {
          stop(
            "Country mask file was not written: ",
            tmp_mask_country
          )
        }
        
        processed_layer_files[lyr] <- tmp_mask_country
        
        rm(
          r_layer,
          r_crop,
          r_share,
          r_resample,
          r_mask_template,
          r_mask_country
        )
        
        gc()
      }
      
      # ---------------------------------------------------
      # VALIDATE LAYER OUTPUTS
      # ---------------------------------------------------
      if (
        any(is.na(processed_layer_files)) ||
        any(processed_layer_files == "") ||
        any(!file.exists(processed_layer_files))
      ) {
        stop(
          "Not all processed layer files were created for ",
          iso3,
          " ",
          yr
        )
      }
      
      # ---------------------------------------------------
      # BUILD OUTPUT STACK
      # ---------------------------------------------------
      r_share_ls <- rast(processed_layer_files)
      names(r_share_ls) <- layer_names
      
      compareGeom(
        r_share_ls,
        ls_template,
        stopOnError = TRUE
      )
      
      writeRaster(
        r_share_ls,
        filename = out_stack_file,
        overwrite = TRUE
      )
      
      if (!file.exists(out_stack_file)) {
        stop(
          "Output stack was not written: ",
          out_stack_file
        )
      }
      
      # ---------------------------------------------------
      # CALCULATE SUM OF ALL GLOBES SHARES
      #
      # Every processing step uses a separate file.
      # This avoids source and target being identical.
      # ---------------------------------------------------
      tmp_sum_raw <- file.path(
        dir_tmp_country,
        paste0(
          "sum_raw_",
          iso3,
          "_",
          yr,
          ".tif"
        )
      )
      
      tmp_sum_template <- file.path(
        dir_tmp_country,
        paste0(
          "sum_template_",
          iso3,
          "_",
          yr,
          ".tif"
        )
      )
      
      tmp_sum_country <- file.path(
        dir_tmp_country,
        paste0(
          "sum_country_",
          iso3,
          "_",
          yr,
          ".tif"
        )
      )
      
      share_sum_raw <- app(
        rast(out_stack_file),
        sum,
        na.rm = TRUE,
        filename = tmp_sum_raw,
        overwrite = TRUE
      )
      
      if (!file.exists(tmp_sum_raw)) {
        stop(
          "Raw share-sum file was not written: ",
          tmp_sum_raw
        )
      }
      
      share_sum_template <- mask(
        share_sum_raw,
        ls_template,
        filename = tmp_sum_template,
        overwrite = TRUE
      )
      
      if (!file.exists(tmp_sum_template)) {
        stop(
          "Template-masked share-sum file was not written: ",
          tmp_sum_template
        )
      }
      
      share_sum_country <- mask(
        share_sum_template,
        country_vect,
        filename = tmp_sum_country,
        overwrite = TRUE
      )
      
      if (!file.exists(tmp_sum_country)) {
        stop(
          "Country-masked share-sum file was not written: ",
          tmp_sum_country
        )
      }
      
      writeRaster(
        share_sum_country,
        filename = out_sum_file,
        overwrite = TRUE
      )
      
      if (!file.exists(out_sum_file)) {
        stop(
          "Final share-sum file was not written: ",
          out_sum_file
        )
      }
      
      # ---------------------------------------------------
      # RANGE DIAGNOSTICS
      # ---------------------------------------------------
      share_range_year <- global(
        rast(out_stack_file),
        "range",
        na.rm = TRUE
      )
      
      write.csv(
        as.data.frame(share_range_year),
        file = out_range_file,
        row.names = TRUE
      )
      
      if (!file.exists(out_range_file)) {
        stop(
          "Range file was not written: ",
          out_range_file
        )
      }
      
      # Store only the final persisted raster reference
      globes_share_ls[[as.character(yr)]] <- rast(
        out_stack_file
      )
      
      rm(
        r_info,
        r_share_ls,
        share_sum_raw,
        share_sum_template,
        share_sum_country,
        share_range_year
      )
      
      gc()
      
      # Remove temporary files only after all final outputs exist
      clear_temp_directory(dir_tmp_country)
      
      TRUE
      
    }, error = function(e) {
      
      error_message <- conditionMessage(e)
      
      warning(
        "Failed for ",
        iso3,
        " in ",
        yr,
        ": ",
        error_message
      )
      
      failed_files_all[[as.character(yr)]] <- error_message
      
      FALSE
    })
    
    if (isTRUE(ok)) {
      failed_files_all[[as.character(yr)]] <- character()
    }
    
    gc()
  }
  
  # ---------------------------------------------------------
  # FAIL IF NO YEAR SUCCEEDED
  # ---------------------------------------------------------
  if (length(globes_share_ls) == 0) {
    
    saveRDS(
      failed_files_all,
      file = file.path(
        dir_out_match,
        paste0(
          "failed_files_all",
          suffix,
          ".rds"
        )
      )
    )
    
    stop(
      "No GlobES-on-LandSHIFT outputs were created for ",
      country_name,
      " (",
      iso3,
      "). Check failed_files_all."
    )
  }
  
  # ---------------------------------------------------------
  # SAVE FAILURE INFORMATION
  # ---------------------------------------------------------
  saveRDS(
    failed_files_all,
    file = file.path(
      dir_out_match,
      paste0(
        "failed_files_all",
        suffix,
        ".rds"
      )
    )
  )
  
  failed_df <- bind_rows(
    lapply(
      names(failed_files_all),
      function(y) {
        
        err <- failed_files_all[[y]]
        
        if (length(err) == 0) {
          return(NULL)
        }
        
        data.frame(
          year = as.integer(y),
          error = as.character(err),
          stringsAsFactors = FALSE
        )
      }
    )
  )
  
  if (nrow(failed_df) == 0) {
    failed_df <- data.frame(
      year = integer(),
      error = character(),
      stringsAsFactors = FALSE
    )
  }
  
  write.csv(
    failed_df,
    file = file.path(
      dir_out_match,
      paste0(
        "failed_files_all",
        suffix,
        ".csv"
      )
    ),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------
  # SAVE OUTPUT LIST
  # ---------------------------------------------------------
  saveRDS(
    globes_share_ls,
    file = file.path(
      dir_out_match,
      paste0(
        "globes_share_ls",
        suffix,
        ".rds"
      )
    )
  )
  
  # ---------------------------------------------------------
  # SAVE METADATA
  # ---------------------------------------------------------
  saveRDS(
    list(
      years_match = years_match,
      successful_years = as.integer(names(globes_share_ls)),
      failed_years = failed_df$year,
      mode = "country",
      country_iso3 = iso3,
      country_name = country_name,
      boundary_source = "rnaturalearth::ne_countries(scale = 'medium')",
      landshift_template = f_cell_area,
      globes_country_dir = dir_globes_country,
      note = paste(
        "Layer-wise GlobES resampling to LandSHIFT grid.",
        "Raw GlobES percent cover converted to share by dividing by 100.",
        "Separate temporary files are used for each crop, resample, mask and sum operation."
      )
    ),
    file = file.path(
      dir_out_match,
      paste0(
        "globes_on_landshift_metadata",
        suffix,
        ".rds"
      )
    )
  )
  
  clear_temp_directory(dir_tmp_country)
  
  message(
    "Finished GlobES on LandSHIFT grid for ",
    country_name,
    " (",
    iso3,
    ")"
  )
  
  message(
    "Successful years: ",
    paste(names(globes_share_ls), collapse = ", ")
  )
  
  if (nrow(failed_df) > 0) {
    message(
      "Failed years: ",
      paste(failed_df$year, collapse = ", ")
    )
  }
  
  return(
    data.frame(
      iso3 = iso3,
      country_name = country_name,
      status = ifelse(
        length(globes_share_ls) == length(years_match),
        "success",
        "partial_success"
      ),
      n_success_years = length(globes_share_ls),
      n_failed_years = length(years_match) - length(globes_share_ls),
      error = ifelse(
        length(globes_share_ls) == length(years_match),
        NA_character_,
        paste(
          "Failed years:",
          paste(failed_df$year, collapse = ", ")
        )
      ),
      stringsAsFactors = FALSE
    )
  )
}