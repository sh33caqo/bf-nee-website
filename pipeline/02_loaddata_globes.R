library(terra)
library(stringr)
library(rnaturalearth)
library(sf)

# ---------------------------------------------------------
# SET DIRECTORY
# ---------------------------------------------------------
dir_globes <- "~/Symobio/Data/GlobES"

dir_out_base <- "~/Symobio/Data/Processed/GlobES_Countries"
dir.create(dir_out_base, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# YEARS COMPATIBLE WITH LANDSHIFT
# ---------------------------------------------------------
years_use <- c(1995, 2000, 2005, 2010, 2015)

# ---------------------------------------------------------
# LOAD WORLD COUNTRY BOUNDARIES
# ---------------------------------------------------------
world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world_sf <- world_sf[!is.na(world_sf$iso_a3), ]
world_sf <- world_sf[world_sf$iso_a3 != "-99", ]
world_sf <- world_sf[world_sf$iso_a3 != "ATA", ]  # exclude Antarctica
world_sf <- world_sf[order(world_sf$iso_a3), ]

# ---------------------------------------------------------
# LIST ALL TIF FILES
# ---------------------------------------------------------
files_all <- list.files(dir_globes, pattern = "\\.tif$", full.names = TRUE)

# ---------------------------------------------------------
# BUILD GLOBAL FILE INFO TABLE
# ---------------------------------------------------------
file_info_global <- data.frame(
  file = files_all,
  name = basename(files_all),
  stringsAsFactors = FALSE
)

file_info_global$class_code <- str_match(
  file_info_global$name,
  "GlobES-(\\d{4})_(\\d{4})0000_1km\\.tif$"
)[, 2]

file_info_global$year <- as.integer(str_match(
  file_info_global$name,
  "GlobES-(\\d{4})_(\\d{4})0000_1km\\.tif$"
)[, 3])

file_info_global <- file_info_global[
  !is.na(file_info_global$class_code) &
    !is.na(file_info_global$year),
]

file_info_global <- file_info_global[
  file_info_global$year %in% years_use,
]

# ---------------------------------------------------------
# CONVERT 4-DIGIT CLASS CODE TO IUCN CODE FORMAT
# e.g. 0101 -> 1.1
# ---------------------------------------------------------
format_iucn_code <- function(x) {
  major <- as.integer(substr(x, 1, 2))
  minor <- as.integer(substr(x, 3, 4))
  paste0(major, ".", minor)
}

file_info_global$iucn_code <- format_iucn_code(file_info_global$class_code)

# ---------------------------------------------------------
# PROCESS ONE COUNTRY
# ---------------------------------------------------------
process_country_globes <- function(country_sf) {
  
  iso3 <- country_sf$iso_a3[1]
  country_name <- country_sf$name_long[1]
  
  message("--------------------------------------------------")
  message("Processing GlobES for ", country_name, " (", iso3, ")")
  message("--------------------------------------------------")
  
  dir_out_globes <- file.path(dir_out_base, iso3)
  dir.create(dir_out_globes, recursive = TRUE, showWarnings = FALSE)
  
  suffix <- paste0("_", tolower(iso3))
  country_vect <- vect(country_sf)
  
  file_info <- file_info_global
  file_info$readable <- NA
  
  # ---------------------------------------------------------
  # TEST WHICH FILES ARE READABLE FOR THIS COUNTRY
  # ---------------------------------------------------------
  for (i in seq_len(nrow(file_info))) {
    
    f <- file_info$file[i]
    
    ok <- tryCatch({
      
      r <- rast(f)
      country_tmp <- project(country_vect, crs(r))
      
      r <- crop(r, country_tmp)
      r <- mask(r, country_tmp)
      
      # check whether country has valid raster cells
      n_valid <- global(!is.na(r), "sum", na.rm = TRUE)[1, 1]
      
      if (is.na(n_valid) || n_valid == 0) {
        stop("No valid cells after masking.")
      }
      
      # tiny read test
      values(r, row = 1, nrows = 1, mat = FALSE)
      
      TRUE
      
    }, error = function(e) {
      FALSE
    })
    
    file_info$readable[i] <- ok
  }
  
  failed_files <- file_info[file_info$readable == FALSE, ]
  good_files   <- file_info[file_info$readable == TRUE, ]
  
  # if no readable files, stop for this country
  if (nrow(good_files) == 0) {
    stop("No readable GlobES files for this country.")
  }
  
  # ---------------------------------------------------------
  # SAVE FILE TABLES
  # ---------------------------------------------------------
  write.csv(
    file_info,
    file.path(dir_out_globes, paste0("globes_file_info", suffix, ".csv")),
    row.names = FALSE
  )
  
  saveRDS(
    file_info,
    file = file.path(dir_out_globes, paste0("globes_file_info", suffix, ".rds"))
  )
  
  write.csv(
    good_files,
    file.path(dir_out_globes, paste0("globes_file_info_readable", suffix, ".csv")),
    row.names = FALSE
  )
  
  saveRDS(
    good_files,
    file = file.path(dir_out_globes, paste0("globes_file_info_readable", suffix, ".rds"))
  )
  
  write.csv(
    failed_files,
    file.path(dir_out_globes, paste0("globes_failed_files", suffix, ".csv")),
    row.names = FALSE
  )
  
  saveRDS(
    failed_files,
    file = file.path(dir_out_globes, paste0("globes_failed_files", suffix, ".rds"))
  )
  
  # ---------------------------------------------------------
  # SAVE PER-YEAR FILE LISTS
  # ---------------------------------------------------------
  files_by_year <- split(good_files, good_files$year)
  
  saveRDS(
    files_by_year,
    file = file.path(dir_out_globes, paste0("globes_files_by_year", suffix, ".rds"))
  )
  
  for (yr in years_use) {
    
    fi <- good_files[good_files$year == yr, ]
    fi <- fi[order(fi$class_code), ]
    
    write.csv(
      fi,
      file.path(dir_out_globes, paste0("globes_files_", yr, suffix, ".csv")),
      row.names = FALSE
    )
  }
  
  # ---------------------------------------------------------
  # METADATA
  # ---------------------------------------------------------
  saveRDS(
    list(
      years_use = years_use,
      mode = "country",
      country_iso3 = iso3,
      country_name = country_name,
      boundary_source = "rnaturalearth::ne_countries(scale = 'medium')",
      dir_globes = dir_globes
    ),
    file = file.path(dir_out_globes, paste0("globes_metadata", suffix, ".rds"))
  )
  
  # ---------------------------------------------------------
  # BUILD AND SAVE STACKS PER YEAR
  # ---------------------------------------------------------
  globes_stacks <- list()
  
  for (yr in years_use) {
    
    fi <- good_files[good_files$year == yr, ]
    fi <- fi[order(fi$class_code), ]
    
    if (nrow(fi) == 0) next
    
    r <- rast(fi$file)
    
    country_tmp <- project(country_vect, crs(r))
    r <- crop(r, country_tmp)
    r <- mask(r, country_tmp)
    
    names(r) <- fi$iucn_code
    
    globes_stacks[[as.character(yr)]] <- r
    
    writeRaster(
      r,
      filename = file.path(
        dir_out_globes,
        paste0("globes_stack_", yr, suffix, ".tif")
      ),
      overwrite = TRUE
    )
  }
  
  saveRDS(
    globes_stacks,
    file = file.path(dir_out_globes, paste0("globes_stacks", suffix, ".rds"))
  )
  
  message("Finished GlobES for ", country_name, " (", iso3, ")")
  
  return(data.frame(
    iso3 = iso3,
    country_name = country_name,
    status = "success",
    n_good_files = nrow(good_files),
    n_failed_files = nrow(failed_files),
    error = NA_character_
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
#       process_country_globes(world_sf[i, ])
#     },
#     error = function(e) {
#       
#       warning("Failed GlobES for ", iso3, ": ", conditionMessage(e))
#       
#       data.frame(
#         iso3 = iso3,
#         country_name = world_sf$name_long[i],
#         status = "failed",
#         n_good_files = NA_real_,
#         n_failed_files = NA_real_,
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
#   file.path(dir_out_base, "globes_country_processing_summary.csv"),
#   row.names = FALSE
# )
# 
# print(results_df)
# table(results_df$status)