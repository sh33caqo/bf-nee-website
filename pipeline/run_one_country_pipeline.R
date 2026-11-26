library(rnaturalearth)
library(sf)

# ---------------------------------------------------------
# READ SLURM ARRAY TASK ID
# ---------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript run_one_country_pipeline.R <SLURM_ARRAY_TASK_ID>")
}

task_id <- as.integer(args[1])

if (is.na(task_id)) {
  stop("SLURM_ARRAY_TASK_ID is not a valid integer.")
}

# ---------------------------------------------------------
# PATH TO COUNTRY-LEVEL SCRIPTS
# ---------------------------------------------------------
script_dir <- "/home/sc.uni-leipzig.de/sh33caqo/Symobio/Scripts_C_v3"

# ---------------------------------------------------------
# LOAD COUNTRY LIST
# ---------------------------------------------------------
world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# ---------------------------------------------------------
# FIX ISO CODES
# ---------------------------------------------------------
# Some countries, notably France, have iso_a3 == "-99" in Natural Earth,
# but a valid adm0_a3 code. We use adm0_a3 as fallback.
world_sf$iso3_pipeline <- ifelse(
  is.na(world_sf$iso_a3) | world_sf$iso_a3 == "-99",
  world_sf$adm0_a3,
  world_sf$iso_a3
)

world_sf <- world_sf[!is.na(world_sf$iso3_pipeline), ]
world_sf <- world_sf[world_sf$iso3_pipeline != "-99", ]
world_sf <- world_sf[world_sf$iso3_pipeline != "ATA", ]

# Make downstream scripts work unchanged:
# they still read country_sf$iso_a3.
world_sf$iso_a3 <- world_sf$iso3_pipeline

world_sf <- world_sf[order(world_sf$continent, world_sf$iso_a3), ]

if (task_id < 1 || task_id > nrow(world_sf)) {
  stop("Task ID ", task_id, " is outside valid country range 1-", nrow(world_sf))
}

country_sf <- world_sf[task_id, ]

iso3 <- country_sf$iso_a3[1]
country_name <- country_sf$name_long[1]
continent <- country_sf$continent[1]

message("##################################################")
message("SLURM task ID: ", task_id)
message("Country: ", country_name, " (", iso3, ")")
message("Continent: ", continent)
message("##################################################")

# ---------------------------------------------------------
# STATUS / CHECKPOINT FILE
# ---------------------------------------------------------
dir_status <- "~/Symobio/Data/Processed/Pipeline_Status"
dir.create(dir_status, recursive = TRUE, showWarnings = FALSE)

status_file <- file.path(
  dir_status,
  paste0("pipeline_status_", iso3, ".csv")
)

is_done <- function(step) {
  
  if (!file.exists(status_file)) {
    return(FALSE)
  }
  
  x <- read.csv(status_file, stringsAsFactors = FALSE)
  
  any(
    x$iso3 == iso3 &
      x$step == step &
      x$status == "success"
  )
}

log_status <- function(step, status, error = NA_character_) {
  
  row <- data.frame(
    time = as.character(Sys.time()),
    task_id = task_id,
    continent = continent,
    iso3 = iso3,
    country_name = country_name,
    step = step,
    status = status,
    error = error,
    stringsAsFactors = FALSE
  )
  
  write.table(
    row,
    file = status_file,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(status_file),
    append = file.exists(status_file)
  )
}

run_step <- function(step, fun) {
  
  if (is_done(step)) {
    message("Skipping completed step: ", step)
    return(TRUE)
  }
  
  message("==================================================")
  message("Running step: ", step)
  message("Country: ", country_name, " (", iso3, ")")
  message("==================================================")
  
  ok <- tryCatch(
    {
      fun(country_sf)
      log_status(step, "success")
      TRUE
    },
    error = function(e) {
      log_status(step, "failed", conditionMessage(e))
      warning("FAILED step ", step, " for ", iso3, ": ", conditionMessage(e))
      FALSE
    }
  )
  
  gc()
  ok
}

run_and_maybe_stop <- function(step, script_file, fun_name) {
  
  source(file.path(script_dir, script_file))
  
  if (!exists(fun_name, mode = "function")) {
    stop("Function not found after sourcing ", script_file, ": ", fun_name)
  }
  
  fun <- get(fun_name, mode = "function")
  
  ok <- run_step(step, fun)
  
  if (!ok) {
    message("Stopping this country after failed step: ", step)
    return(FALSE)
  }
  
  TRUE
}

# ---------------------------------------------------------
# RUN PIPELINE FOR THIS COUNTRY
# ---------------------------------------------------------
pipeline_failed <- FALSE

ok <- run_and_maybe_stop(
  "01_loaddata_landshift_v3",
  "01_loaddata_landshift.R",
  "process_country_landshift"
)

if (!ok) {
  pipeline_failed <- TRUE
} else {
  
  ok <- run_and_maybe_stop(
    "02_loaddata_globes",
    "02_loaddata_globes.R",
    "process_country_globes"
  )
  
  if (!ok) {
    pipeline_failed <- TRUE
  } else {
    
    ok <- run_and_maybe_stop(
      "03_raster_adaptation",
      "03_raster_adaptation.R",
      "process_country_globes_on_landshift"
    )
    
    if (!ok) {
      pipeline_failed <- TRUE
    } else {
      
      ok <- run_and_maybe_stop(
        "04_raster_adaptation_shares_v3",
        "04_raster_adaptation_shares.R",
        "process_country_ag_adjusted"
      )
      
      if (!ok) {
        pipeline_failed <- TRUE
      } else {
        
        ok <- run_and_maybe_stop(
          "05_change_in_ecosystem_extent_v3",
          "05_change_in_ecosystem_extent.R",
          "process_country_dA"
        )
        
        if (!ok) {
          pipeline_failed <- TRUE
        } else {
          
          ok <- run_and_maybe_stop(
            "06_Extraction_Commodity_Shares_v2",
            "06_Extraction_Commodity_Shares.R",
            "process_country_commodity_sf"
          )
          
          if (!ok) {
            pipeline_failed <- TRUE
          } else {
            
            ok <- run_and_maybe_stop(
              "07_Extraction_Production_Shares_v3",
              "07_Extraction_Production_Shares.R",
              "process_country_gloria_sf"
            )
            
            if (!ok) {
              pipeline_failed <- TRUE
            } else {
              
              ok <- run_and_maybe_stop(
                "08_calculating_indicator_v3",
                "08_calculating_indicator.R",
                "process_country_bf_nee"
              )
              
              if (!ok) {
                pipeline_failed <- TRUE
              }
            }
          }
        }
      }
    }
  }
}

# ---------------------------------------------------------
# FINAL MESSAGE
# ---------------------------------------------------------
if (pipeline_failed) {
  message("##################################################")
  message("Country pipeline ended with failure: ", country_name, " (", iso3, ")")
  message("Other SLURM array countries continue independently.")
  message("##################################################")
} else {
  message("##################################################")
  message("Finished country pipeline successfully: ", country_name, " (", iso3, ")")
  message("##################################################")
}