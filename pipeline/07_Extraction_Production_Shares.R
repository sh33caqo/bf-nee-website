library(dplyr)
library(stringr)

# ---------------------------------------------------------
# SETTINGS
# ---------------------------------------------------------
years_use <- c(1995, 2000, 2005, 2010, 2015)

f_gloria <- "~/Symobio/Data/GLORIArel59_production_share_for_final_demand_in_Germany.csv"

dir_out_gloria <- "~/Symobio/Data/Processed/GLORIA_production_shares_v3_Countries"
dir.create(dir_out_gloria, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# COMMODITY <-> GLORIA PRODUCT MAPPING
# ---------------------------------------------------------
commodity_gloria_map <- data.frame(
  commodity = c(
    "Rice", "Wheat", "Cereals n.e.c.", "Other crops n.e.c",
    "Spice - beverage - pharmaceutical crops", "Tobacco",
    "Roots and tubers", "Sugar crops", "Pulses", "Nuts",
    "Oil bearing crops", "Vegetables", "Fruits", "Fibres", "Maize"
  ),
  gloria_product = c(
    "Growing rice",
    "Growing wheat",
    "Growing cereals n.e.c",
    "Growing crops n.e.c.",
    "Growing spices, aromatic, drug and pharmaceutical crops",
    "Growing tobacco",
    "Growing vegetables roots tubers",
    "Growing sugar beet and cane",
    "Growing leguminous crops and oil seeds",
    "Growing fruits and nuts",
    "Growing leguminous crops and oil seeds",
    "Growing vegetables roots tubers",
    "Growing fruits and nuts",
    "Growing fibre crops",
    "Growing maize"
  ),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------
# GRAZING LIVESTOCK (intensive pasture, v3)
# P of the pseudo-commodity "Grazing livestock" is a weighted
# mean of the GLORIA grazing-animal sectors. Weights can be
# changed here (e.g. to livestock-unit shares per country).
# ---------------------------------------------------------
pasture_commodity <- "Grazing livestock"

pasture_gloria_weights <- c(
  "Raising of cattle"          = 0.5,
  "Raising of sheep and goats" = 0.5
)

# ---------------------------------------------------------
# HELPER FUNCTIONS
# ---------------------------------------------------------
safe_filename <- function(x) {
  x <- paste(x, collapse = "_")
  x <- str_replace_all(x, "[^[:alnum:]]+", "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  tolower(x)
}

# Natural Earth name -> GLORIA name(s)
standardize_country_name_gloria <- function(x) {
  x <- trimws(x)
  
  lookup <- list(
    "Côte d'Ivoire" = "Cote d'Ivoire",
    "Czechia" = "Czech Republic",
    "Russia" = "Russian Federation",
    "Vietnam" = "Viet Nam",
    "Republic of the Congo" = "Rep Congo",
    "Democratic Republic of the Congo" = "DR Congo",
    "Ethiopia" = "DR Ethiopia",
    "Syria" = "Syria",
    "Laos" = "Laos",
    "Lao PDR" = "Laos",
    "Iran" = "Iran",
    "Palestine" = "Palestine",
    "Brunei" = "Brunei Darussalam",
    "United States" = "United States of America",
    "Republic of Korea" = "South Korea",
    "Dem. Rep. Korea" = "North Korea",
    "Yemen" = c("DR Yemen (Aden)", "Yemen Arab Republic")
  )
  
  if (x %in% names(lookup)) {
    return(lookup[[x]])
  }
  
  x
}

# ---------------------------------------------------------
# READ GLORIA FILE ROBUSTLY
# ---------------------------------------------------------
gloria_raw <- read.csv(
  f_gloria,
  header = FALSE,
  sep = ";",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

if (ncol(gloria_raw) == 1) {
  gloria_raw <- read.csv(
    f_gloria,
    header = FALSE,
    sep = ",",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

if (ncol(gloria_raw) < 5) {
  stop("GLORIA file was not read correctly. Number of columns: ", ncol(gloria_raw))
}

header_row <- as.character(gloria_raw[1, ])
gloria <- gloria_raw[-1, ]

names(gloria)[1:4] <- c("col1", "country", "col3", "gloria_product")
names(gloria)[5:ncol(gloria)] <- header_row[5:ncol(gloria)]

gloria$country <- trimws(as.character(gloria$country))
gloria$gloria_product <- trimws(as.character(gloria$gloria_product))

year_cols <- as.character(years_use)

missing_years <- setdiff(year_cols, names(gloria))

if (length(missing_years) > 0) {
  stop(
    "Missing year columns in GLORIA file: ",
    paste(missing_years, collapse = ", ")
  )
}

for (yr in year_cols) {
  gloria[[yr]] <- as.numeric(
    gsub(",", ".", as.character(gloria[[yr]]))
  )
}

# ---------------------------------------------------------
# PROCESS ONE COUNTRY BY NAME
# ---------------------------------------------------------
process_country_gloria <- function(target_country) {
  
  target_country_original <- target_country
  target_country <- standardize_country_name_gloria(target_country)
  
  message("--------------------------------------------------")
  message("Processing GLORIA production shares for ", target_country_original)
  message(
    "Using GLORIA country name(s): ",
    paste(target_country, collapse = " + ")
  )
  message("--------------------------------------------------")
  
  # IMPORTANT:
  # Output slug uses original Natural Earth country name.
  # This keeps step 08 compatible with its expected file names.
  country_slug <- safe_filename(target_country_original)
  
  dir_out_country <- file.path(dir_out_gloria, country_slug)
  dir.create(dir_out_country, recursive = TRUE, showWarnings = FALSE)
  
  gloria_country <- gloria[
    gloria$country %in% target_country &
      gloria$gloria_product %in% unique(c(commodity_gloria_map$gloria_product,
                                         names(pasture_gloria_weights))),
    c("country", "gloria_product", year_cols),
    drop = FALSE
  ]
  
  if (nrow(gloria_country) == 0) {
    stop(
      "No matching GLORIA rows found for country: ",
      paste(target_country, collapse = " + ")
    )
  }
  
  # If several GLORIA regions map to one modern country
  # e.g. Yemen = DR Yemen (Aden) + Yemen Arab Republic,
  # aggregate production shares by product.
  gloria_country <- gloria_country %>%
    group_by(gloria_product) %>%
    summarise(
      country = paste(unique(country), collapse = " + "),
      across(
        all_of(year_cols),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  res <- merge(
    commodity_gloria_map,
    gloria_country,
    by = "gloria_product",
    all.x = TRUE
  )
  
  res <- res[, c("commodity", "gloria_product", "country", year_cols)]
  
  # ---------------------------------------------------------
  # ADD GRAZING LIVESTOCK (weighted mean of grazing sectors)
  # ---------------------------------------------------------
  past_rows <- gloria_country[gloria_country$gloria_product %in% names(pasture_gloria_weights), ]
  
  past_vals <- sapply(year_cols, function(yr) {
    if (nrow(past_rows) == 0) return(NA_real_)
    v <- past_rows[[yr]]
    w <- pasture_gloria_weights[past_rows$gloria_product]
    ok <- !is.na(v)
    if (!any(ok)) return(NA_real_)
    sum(v[ok] * w[ok]) / sum(w[ok])
  })
  
  past_res <- data.frame(
    commodity = pasture_commodity,
    gloria_product = paste(names(pasture_gloria_weights), collapse = " + "),
    country = if (nrow(past_rows) > 0) past_rows$country[1] else paste(target_country, collapse = " + "),
    t(past_vals),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  res <- rbind(res, past_res[, names(res)])
  
  res$country_original <- target_country_original
  
  res <- res[, c(
    "country_original",
    "commodity",
    "gloria_product",
    "country",
    year_cols
  )]
  
  write.csv(
    res,
    file = file.path(
      dir_out_country,
      paste0(
        "gloria_production_shares_",
        country_slug,
        "_1995_2015.csv"
      )
    ),
    row.names = FALSE
  )
  
  res_long <- do.call(
    rbind,
    lapply(year_cols, function(yr) {
      data.frame(
        country_original = res$country_original,
        country = res$country,
        commodity = res$commodity,
        gloria_product = res$gloria_product,
        year = as.integer(yr),
        production_share = res[[yr]],
        stringsAsFactors = FALSE
      )
    })
  )
  
  res_long <- res_long[order(res_long$commodity, res_long$year), ]
  
  write.csv(
    res_long,
    file = file.path(
      dir_out_country,
      paste0(
        "gloria_production_shares_",
        country_slug,
        "_long.csv"
      )
    ),
    row.names = FALSE
  )
  
  write.csv(
    commodity_gloria_map,
    file = file.path(
      dir_out_country,
      paste0("commodity_gloria_mapping_", country_slug, ".csv")
    ),
    row.names = FALSE
  )
  
  saveRDS(
    list(
      years_use = years_use,
      country_original = target_country_original,
      country_used = target_country,
      country_slug = country_slug,
      input_file = f_gloria,
      pasture_commodity = pasture_commodity,
      pasture_gloria_weights = pasture_gloria_weights,
      note = "v3: GLORIA production shares for crop commodities plus 'Grazing livestock' (weighted mean of grazing-animal sectors)."
    ),
    file = file.path(
      dir_out_country,
      paste0(
        "gloria_production_shares_",
        country_slug,
        "_metadata.rds"
      )
    )
  )
  
  message("Finished GLORIA production shares for ", target_country_original)
  
  return(data.frame(
    country_original = target_country_original,
    country_used = paste(target_country, collapse = " + "),
    country_slug = country_slug,
    status = "success",
    error = NA_character_,
    stringsAsFactors = FALSE
  ))
}

# ---------------------------------------------------------
# WRAPPER FOR MASTER PIPELINE
# ---------------------------------------------------------
process_country_gloria_sf <- function(country_sf) {
  country_name <- country_sf$name_long[1]
  process_country_gloria(country_name)
}