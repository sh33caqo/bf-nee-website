args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop(
    "Usage: Rscript run_step08_by_iso.R <ISO3>"
  )
}

iso3 <- toupper(trimws(args[[1]]))

message("Running step 08 for ISO3: ", iso3)

suppressPackageStartupMessages({
  library(sf)
  library(rnaturalearth)
})

source(
  "/home/sc.uni-leipzig.de/sh33caqo/Symobio/Scripts_C_v3/08_calculating_indicator.R"
)

world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world_sf$iso3_pipeline <- ifelse(
  is.na(world_sf$iso_a3) |
    world_sf$iso_a3 == "-99",
  world_sf$adm0_a3,
  world_sf$iso_a3
)

world_sf$iso_a3 <- world_sf$iso3_pipeline

country_sf <- world_sf[
  world_sf$iso_a3 == iso3,
  ,
  drop = FALSE
]

if (nrow(country_sf) != 1) {
  stop(
    "Country not uniquely found for ISO3 ",
    iso3,
    ". Rows found: ",
    nrow(country_sf)
  )
}

message(
  "Processing ",
  country_sf$name_long[[1]],
  " (",
  iso3,
  ")"
)

result <- process_country_bf_nee(country_sf)

print(result)

if (
  !is.data.frame(result) ||
  nrow(result) == 0 ||
  result$status[[1]] != "success"
) {
  stop("Step 08 failed for ", iso3)
}

message("Step 08 completed successfully for ", iso3)