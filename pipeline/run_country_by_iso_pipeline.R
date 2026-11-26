library(rnaturalearth)
library(sf)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript run_country_by_iso_pipeline.R <ISO3> [force_steps, e.g. 06,08]")
}

target_iso3 <- toupper(args[1])

script_dir <- "/home/sc.uni-leipzig.de/sh33caqo/Symobio/Scripts_C_v3"

# ---------------------------------------------------------
# TERRA MEMORY LIMIT
# terra sees the RAM of the whole node, not the SLURM limit, and
# may use more memory than the job is allowed (-> OOM kill).
# Limit terra to 50% of the SLURM allocation and write its
# temporary files to TMPDIR.
# ---------------------------------------------------------
slurm_mem_mb <- suppressWarnings(as.numeric(Sys.getenv("SLURM_MEM_PER_NODE", "")))

terra::terraOptions(
  memfrac = 0.5,
  tempdir = Sys.getenv("TMPDIR", unset = tempdir())
)

if (is.finite(slurm_mem_mb) && slurm_mem_mb > 0) {
  terra::terraOptions(memmax = 0.5 * slurm_mem_mb / 1024)   # GB
  message("terra memmax set to ", round(0.5 * slurm_mem_mb / 1024), " GB")
}

world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world_sf$iso3_pipeline <- ifelse(
  is.na(world_sf$iso_a3) | world_sf$iso_a3 == "-99",
  world_sf$adm0_a3,
  world_sf$iso_a3
)

world_sf <- world_sf[!is.na(world_sf$iso3_pipeline), ]
world_sf <- world_sf[world_sf$iso3_pipeline != "-99", ]
world_sf <- world_sf[world_sf$iso3_pipeline != "ATA", ]

world_sf$iso_a3 <- world_sf$iso3_pipeline
world_sf <- world_sf[order(world_sf$continent, world_sf$iso_a3), ]

if (!(target_iso3 %in% world_sf$iso_a3)) {
  stop("ISO3 not found in country list: ", target_iso3)
}

country_sf <- world_sf[world_sf$iso_a3 == target_iso3, ]

iso3 <- country_sf$iso_a3[1]
country_name <- country_sf$name_long[1]
continent <- country_sf$continent[1]

message("##################################################")
message("Running selected country: ", country_name, " (", iso3, ")")
message("Continent: ", continent)
message("##################################################")

dir_status <- "~/Symobio/Data/Processed/Pipeline_Status"
dir.create(dir_status, recursive = TRUE, showWarnings = FALSE)

status_file <- file.path(
  dir_status,
  paste0("pipeline_status_", iso3, ".csv")
)

is_done <- function(step) {
  if (!file.exists(status_file)) return(FALSE)
  x <- read.csv(status_file, stringsAsFactors = FALSE)
  any(x$iso3 == iso3 & x$step == step & x$status == "success")
}

log_status <- function(step, status, error = NA_character_) {
  row <- data.frame(
    time = as.character(Sys.time()),
    task_id = paste0("iso_", iso3),
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
  if (is_done(step) && !is_forced(step)) {
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
# PIPELINE STEPS
# Version tags in the checkpoint names decide what is re-run:
# v3 (intensive pasture) re-runs 01, 04, 05, 07 and 08 once.
# 02 and 03 (GlobES loading/resampling) and 06 v2 (crop shares)
# are unchanged and skipped if already completed.
# ---------------------------------------------------------
steps <- list(
  c("01_loaddata_landshift_v3", "01_loaddata_landshift.R", "process_country_landshift"),
  c("02_loaddata_globes", "02_loaddata_globes.R", "process_country_globes"),
  c("03_raster_adaptation", "03_raster_adaptation.R", "process_country_globes_on_landshift"),
  c("04_raster_adaptation_shares_v3", "04_raster_adaptation_shares.R", "process_country_ag_adjusted"),
  c("05_change_in_ecosystem_extent_v3", "05_change_in_ecosystem_extent.R", "process_country_dA"),
  c("06_Extraction_Commodity_Shares_v2", "06_Extraction_Commodity_Shares.R", "process_country_commodity_sf"),
  c("07_Extraction_Production_Shares_v3", "07_Extraction_Production_Shares.R", "process_country_gloria_sf"),
  c("08_calculating_indicator_v3", "08_calculating_indicator.R", "process_country_bf_nee")
)

# ---------------------------------------------------------
# OPTIONAL: FORCE RE-RUN OF SELECTED STEPS
# Second argument, comma-separated step prefixes, e.g.
#   Rscript run_country_by_iso_pipeline.R BRA 06,08
# ---------------------------------------------------------
force_steps <- character(0)

if (length(args) >= 2 && nchar(args[2]) > 0) {
  force_steps <- trimws(strsplit(args[2], ",")[[1]])
  message("Forcing re-run of steps: ", paste(force_steps, collapse = ", "))
}

is_forced <- function(step) {
  length(force_steps) > 0 && any(startsWith(step, force_steps))
}

pipeline_failed <- FALSE

for (s in steps) {
  ok <- run_and_maybe_stop(s[1], s[2], s[3])
  if (!ok) {
    pipeline_failed <- TRUE
    break
  }
}

if (pipeline_failed) {
  message("Country pipeline ended with failure: ", country_name, " (", iso3, ")")
} else {
  message("Finished country pipeline successfully: ", country_name, " (", iso3, ")")
}