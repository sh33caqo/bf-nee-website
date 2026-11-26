library(stringr)

# ---------------------------------------------------------
# SETTINGS
# ---------------------------------------------------------
years_use <- c(1995, 2000, 2005, 2010, 2015)

# Transitions between consecutive time steps.
# The commodity share of a transition is stored under its END year
# (e.g. 2010_2015 -> column "2015"), because script 08 combines
# the transition y-1 -> y with the attribution factors of year y.
transitions_use <- data.frame(
  year_start = years_use[-length(years_use)],
  year_end   = years_use[-1]
)
transitions_use$transition <- paste0(
  transitions_use$year_start, "_", transitions_use$year_end
)

dir_scen_crops <- "~/Symobio/Data/LandSHIFT/Commodities"
dir_out_comm <- "~/Symobio/Data/Processed/Commodity_shares_Countries"

dir.create(dir_out_comm, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# DEFINE COMMODITY PAIRS: rf + ir
# ---------------------------------------------------------
commodity_map <- data.frame(
  commodity = c(
    "Rice", "Wheat", "Cereals n.e.c.", "Other crops n.e.c",
    "Spice - beverage - pharmaceutical crops", "Tobacco",
    "Roots and tubers", "Sugar crops", "Pulses", "Nuts",
    "Oil bearing crops", "Vegetables", "Fruits", "Fibres", "Maize"
  ),
  col_rf = c(
    "Rice_rf", "Wheat_rf", "Cereals n.e.c._rf", "Other crops n.e.c_rf",
    "Spice - beverage - pharmaceutical crops_rf", "Tobacco_rf",
    "Roots and tubers_rf", "Sugar crops_rf", "Pulses_rf", "Nuts_rf",
    "Oil bearing crops_rf", "Vegetables_rf", "Fruits_rf", "Fibres_rf", "Maize_rf"
  ),
  col_ir = c(
    "Rice_ir", "Wheat_ir", "Cereals n.e.c._ir", "Other crops n.e.c_ir",
    "Spice - beverage - pharmaceutical crops_ir", "Tobacco_ir",
    "Roots and tubers_ir", "Sugar crops_ir", "Pulses_ir", "Nuts_ir",
    "Oil bearing crops_ir", "Vegetables_ir", "Fruits_ir", "Fibres_ir", "Maize_ir"
  ),
  stringsAsFactors = FALSE
)

all_needed_cols <- c(commodity_map$col_rf, commodity_map$col_ir)

# ---------------------------------------------------------
# HELPERS
# ---------------------------------------------------------
find_country_column <- function(df) {
  cand <- c("Country", "country", "COUNTRY", "GeoRegion", "GeoSubRegion")
  hit <- cand[cand %in% names(df)]

  if (length(hit) == 0) {
    stop("No country column found. Available columns are:\n",
         paste(names(df), collapse = " | "))
  }

  hit[1]
}

check_required_columns <- function(df, cols_needed, year) {
  missing_cols <- setdiff(cols_needed, names(df))

  if (length(missing_cols) > 0) {
    stop("In year ", year, ", these columns are missing:\n",
         paste(missing_cols, collapse = ", "))
  }
}

safe_numeric <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA
  suppressWarnings(as.numeric(gsub(",", ".", x)))
}

safe_filename <- function(x) {
  x <- str_replace_all(x, "[^[:alnum:]]+", "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  tolower(x)
}

# Natural Earth name -> LandSHIFT / ScenCrops country name
standardize_country_name_commodity <- function(x) {
  x <- trimws(x)

  lookup <- c(
    "Democratic Republic of the Congo" = "Congo, The Democratic Republic of the",
    "Republic of the Congo" = "Congo",
    "Côte d'Ivoire" = "Cote d'Ivoire",
    "Czechia" = "Czech Republic",
    "Republic of Korea" = "Korea, Republic of",
    "Dem. Rep. Korea" = "Korea, Democratic People's Republic of",
    "North Korea" = "Korea, Democratic People's Republic of",
    "South Korea" = "Korea, Republic of",
    "Laos" = "Lao People's Democratic Republic",
    "Lao PDR" = "Lao People's Democratic Republic",
    "Syria" = "Syrian Arab Republic",
    "Vietnam" = "Viet Nam",
    "Russia" = "Russian Federation",
    "Macedonia" = "Macedonia, The Former Yugoslav Republic of",
    "Iran" = "Iran, Islamic Republic of",
    "Palestine" = "Palestinian Territory, Occupied",
    "Brunei" = "Brunei Darussalam",
    "Libya" = "Libyan Arab Jamahiriya",
    "Moldova" = "Moldova, Republic of",
    "Republic of Moldova" = "Moldova, Republic of"
  )

  ifelse(x %in% names(lookup), lookup[x], x)
}

# ---------------------------------------------------------
# FIRST PASS: READ ALL FILES
# ---------------------------------------------------------
input_data <- list()
country_names_all <- list()

for (yr in years_use) {

  f_csv <- file.path(dir_scen_crops, paste0("Scen_crops_", yr, ".csv"))

  if (!file.exists(f_csv)) {
    stop("File not found: ", f_csv)
  }

  df <- read.csv2(
    f_csv,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  country_col <- find_country_column(df)
  check_required_columns(df, all_needed_cols, yr)

  df[[country_col]] <- trimws(df[[country_col]])

  input_data[[as.character(yr)]] <- list(
    df = df,
    country_col = country_col
  )

  country_names_all[[as.character(yr)]] <- unique(df[[country_col]])
}

countries_all <- sort(unique(unlist(country_names_all)))
countries_all <- countries_all[!is.na(countries_all) & countries_all != ""]

# ---------------------------------------------------------
# PROCESS ONE COUNTRY BY NAME
# ---------------------------------------------------------
process_country_commodity <- function(target_country) {

  target_country_original <- target_country
  target_country_ls <- standardize_country_name_commodity(target_country)

  message("--------------------------------------------------")
  message("Processing commodity shares for ", target_country_original)
  message("Using LandSHIFT country name: ", target_country_ls)
  message("--------------------------------------------------")

  # IMPORTANT:
  # Slug uses original Natural Earth name so script 08 finds the expected file.
  country_slug <- safe_filename(target_country_original)

  dir_out_country <- file.path(dir_out_comm, country_slug)
  dir.create(dir_out_country, recursive = TRUE, showWarnings = FALSE)

  # ---------------------------------------------------------
  # STEP 1:
  # COMMODITY AREAS PER YEAR
  # A_(c,r,y) = A_(c,rf,r,y) + A_(c,ir,r,y)
  # ---------------------------------------------------------
  results_yearly <- list()

  share_abs_mat <- matrix(
    NA_real_,
    nrow = nrow(commodity_map),
    ncol = length(years_use),
    dimnames = list(commodity_map$commodity, as.character(years_use))
  )

  area_mat <- matrix(
    NA_real_,
    nrow = nrow(commodity_map),
    ncol = length(years_use),
    dimnames = list(commodity_map$commodity, as.character(years_use))
  )

  total_agri_vec <- setNames(
    rep(NA_real_, length(years_use)),
    as.character(years_use)
  )

  failed_years <- list()

  for (yr in years_use) {

    year_key <- as.character(yr)
    df <- input_data[[year_key]]$df
    country_col <- input_data[[year_key]]$country_col

    hit <- which(trimws(df[[country_col]]) == target_country_ls)

    if (length(hit) == 0) {
      failed_years[[year_key]] <- paste0(
        "country_not_found_in_year_using_name_",
        target_country_ls
      )
      next
    }

    if (length(hit) > 1) {
      warning(
        "More than one row found for ",
        target_country_ls,
        " in year ",
        yr,
        ". Using first match."
      )
      hit <- hit[1]
    }

    df_row <- df[hit, , drop = FALSE]

    area_combined <- numeric(nrow(commodity_map))
    names(area_combined) <- commodity_map$commodity

    area_rf_vec <- numeric(nrow(commodity_map))
    area_ir_vec <- numeric(nrow(commodity_map))

    for (i in seq_len(nrow(commodity_map))) {

      val_rf <- safe_numeric(df_row[[commodity_map$col_rf[i]]])
      val_ir <- safe_numeric(df_row[[commodity_map$col_ir[i]]])

      if (is.na(val_rf)) val_rf <- 0
      if (is.na(val_ir)) val_ir <- 0

      area_rf_vec[i] <- val_rf
      area_ir_vec[i] <- val_ir
      area_combined[i] <- val_rf + val_ir
    }

    total_agri_area <- sum(area_combined, na.rm = TRUE)
    total_agri_vec[year_key] <- total_agri_area

    # Absolute (static) share, kept only for reference / comparison.
    # It is NOT used any more in script 08.
    if (is.na(total_agri_area) || total_agri_area <= 0) {
      share_abs <- rep(NA_real_, length(area_combined))
    } else {
      share_abs <- area_combined / total_agri_area
    }

    names(share_abs) <- commodity_map$commodity

    area_mat[, year_key] <- area_combined
    share_abs_mat[, year_key] <- share_abs

    res_year <- data.frame(
      country_original = target_country_original,
      country = target_country_ls,
      year = yr,
      commodity = commodity_map$commodity,
      area_rf = area_rf_vec,
      area_ir = area_ir_vec,
      area_total_commodity = area_combined,
      total_agri_area = total_agri_area,
      share_absolute = share_abs,
      stringsAsFactors = FALSE
    )

    results_yearly[[year_key]] <- res_year

    write.csv(
      res_year,
      file = file.path(
        dir_out_country,
        paste0("commodity_areas_", country_slug, "_", yr, ".csv")
      ),
      row.names = FALSE
    )

    failed_years[[year_key]] <- NA_character_
  }

  # ---------------------------------------------------------
  # STEP 2:
  # COMMODITY SHARES OF AGRICULTURAL EXPANSION PER TRANSITION
  #
  # dA_(c,r,y)     = A_(c,r,y) - A_(c,r,y-1)
  # dA+_(c,r,y)    = max(dA_(c,r,y), 0)
  # S_(c,r,y)      = dA+_(c,r,y) / sum_c dA+_(c,r,y)
  #
  # Only commodities with a positive area change are taken into
  # account. Commodities with a decreasing area receive S = 0.
  # If no commodity expanded, all S = 0 (no attribution).
  # By construction sum_c S_(c,r,y) = 1 (or 0).
  # ---------------------------------------------------------
  share_mat <- matrix(
    NA_real_,
    nrow = nrow(commodity_map),
    ncol = nrow(transitions_use),
    dimnames = list(
      commodity_map$commodity,
      as.character(transitions_use$year_end)
    )
  )

  delta_mat <- share_mat
  delta_pos_mat <- share_mat

  results_transitions <- list()
  failed_transitions <- list()
  transition_totals <- list()

  for (k in seq_len(nrow(transitions_use))) {

    yr_prev <- transitions_use$year_start[k]
    yr_curr <- transitions_use$year_end[k]
    trans   <- transitions_use$transition[k]

    key_prev <- as.character(yr_prev)
    key_curr <- as.character(yr_curr)

    A_prev <- area_mat[, key_prev]
    A_curr <- area_mat[, key_curr]

    if (all(is.na(A_prev)) || all(is.na(A_curr))) {
      failed_transitions[[trans]] <- "missing_commodity_areas_in_start_or_end_year"
      next
    }

    dA_c <- A_curr - A_prev
    dA_c_pos <- pmax(dA_c, 0)

    sum_pos_change <- sum(dA_c_pos, na.rm = TRUE)
    net_total_change <- total_agri_vec[key_curr] - total_agri_vec[key_prev]

    if (is.na(sum_pos_change) || sum_pos_change <= 0) {
      share_trans <- rep(0, length(dA_c_pos))
      failed_transitions[[trans]] <- "no_positive_commodity_area_change_all_shares_zero"
      message(
        "No commodity with positive area change for ",
        target_country_original,
        " in ",
        trans,
        ". All commodity shares set to 0."
      )
    } else {
      share_trans <- dA_c_pos / sum_pos_change
      failed_transitions[[trans]] <- NA_character_
    }

    names(share_trans) <- commodity_map$commodity

    share_mat[, key_curr] <- share_trans
    delta_mat[, key_curr] <- dA_c
    delta_pos_mat[, key_curr] <- dA_c_pos

    res_trans <- data.frame(
      country_original = target_country_original,
      country = target_country_ls,
      transition = trans,
      year_start = yr_prev,
      year_end = yr_curr,
      year = yr_curr,
      commodity = commodity_map$commodity,
      area_start = A_prev,
      area_end = A_curr,
      delta_area = dA_c,
      delta_area_positive = dA_c_pos,
      expanding = dA_c > 0,
      total_agri_area_start = total_agri_vec[key_prev],
      total_agri_area_end = total_agri_vec[key_curr],
      net_total_agri_area_change = net_total_change,
      sum_positive_area_change = sum_pos_change,
      share = share_trans,
      stringsAsFactors = FALSE
    )

    results_transitions[[trans]] <- res_trans

    transition_totals[[trans]] <- data.frame(
      country_original = target_country_original,
      country = target_country_ls,
      transition = trans,
      year_start = yr_prev,
      year_end = yr_curr,
      total_agri_area_start = total_agri_vec[key_prev],
      total_agri_area_end = total_agri_vec[key_curr],
      net_total_agri_area_change = net_total_change,
      sum_positive_area_change = sum_pos_change,
      n_expanding_commodities = sum(dA_c > 0, na.rm = TRUE),
      sum_shares = sum(share_trans, na.rm = TRUE),
      stringsAsFactors = FALSE
    )

    write.csv(
      res_trans,
      file = file.path(
        dir_out_country,
        paste0("commodity_shares_", country_slug, "_", trans, ".csv")
      ),
      row.names = FALSE
    )
  }

  # ---------------------------------------------------------
  # SAVE TABLES
  # ---------------------------------------------------------

  # Main input for script 08:
  # columns = END year of each transition (2000, 2005, 2010, 2015)
  shares_df <- data.frame(
    commodity = rownames(share_mat),
    share_mat,
    row.names = NULL,
    check.names = FALSE
  )

  write.csv(
    shares_df,
    file = file.path(
      dir_out_country,
      paste0("commodity_shares_", country_slug, "_1995_2015.csv")
    ),
    row.names = FALSE
  )

  delta_df <- data.frame(
    commodity = rownames(delta_mat),
    delta_mat,
    row.names = NULL,
    check.names = FALSE
  )

  write.csv(
    delta_df,
    file = file.path(
      dir_out_country,
      paste0("commodity_delta_areas_", country_slug, "_1995_2015.csv")
    ),
    row.names = FALSE
  )

  shares_abs_df <- data.frame(
    commodity = rownames(share_abs_mat),
    share_abs_mat,
    row.names = NULL,
    check.names = FALSE
  )

  write.csv(
    shares_abs_df,
    file = file.path(
      dir_out_country,
      paste0("commodity_shares_absolute_", country_slug, "_1995_2015.csv")
    ),
    row.names = FALSE
  )

  areas_df <- data.frame(
    commodity = rownames(area_mat),
    area_mat,
    row.names = NULL,
    check.names = FALSE
  )

  write.csv(
    areas_df,
    file = file.path(
      dir_out_country,
      paste0("commodity_areas_", country_slug, "_1995_2015.csv")
    ),
    row.names = FALSE
  )

  totals_df <- data.frame(
    country_original = target_country_original,
    country = target_country_ls,
    year = years_use,
    total_agri_area = as.numeric(total_agri_vec[as.character(years_use)])
  )

  write.csv(
    totals_df,
    file = file.path(
      dir_out_country,
      paste0("commodity_total_agri_area_", country_slug, "_1995_2015.csv")
    ),
    row.names = FALSE
  )

  if (length(transition_totals) > 0) {
    transition_totals_df <- do.call(rbind, transition_totals)
    rownames(transition_totals_df) <- NULL
  } else {
    transition_totals_df <- data.frame()
  }

  write.csv(
    transition_totals_df,
    file = file.path(
      dir_out_country,
      paste0("commodity_area_change_totals_", country_slug, "_transitions.csv")
    ),
    row.names = FALSE
  )

  for (comm in commodity_map$commodity) {

    tmp <- data.frame(
      country_original = target_country_original,
      country = target_country_ls,
      commodity = comm,
      `1995_2000` = share_mat[comm, "2000"],
      `2000_2005` = share_mat[comm, "2005"],
      `2005_2010` = share_mat[comm, "2010"],
      `2010_2015` = share_mat[comm, "2015"],
      row.names = NULL,
      check.names = FALSE
    )

    fn <- paste0(
      "share_",
      safe_filename(comm),
      "_",
      country_slug,
      ".csv"
    )

    write.csv(tmp, file = file.path(dir_out_country, fn), row.names = FALSE)
  }

  saveRDS(
    results_yearly,
    file = file.path(
      dir_out_country,
      paste0("commodity_results_yearly_", country_slug, ".rds")
    )
  )

  saveRDS(
    results_transitions,
    file = file.path(
      dir_out_country,
      paste0("commodity_results_transitions_", country_slug, ".rds")
    )
  )

  saveRDS(
    share_mat,
    file = file.path(
      dir_out_country,
      paste0("commodity_share_matrix_", country_slug, ".rds")
    )
  )

  saveRDS(
    area_mat,
    file = file.path(
      dir_out_country,
      paste0("commodity_area_matrix_", country_slug, ".rds")
    )
  )

  failed_df <- data.frame(
    step = c(
      paste0("year_", names(failed_years)),
      paste0("transition_", names(failed_transitions))
    ),
    error = c(
      unlist(failed_years, use.names = FALSE),
      unlist(failed_transitions, use.names = FALSE)
    ),
    stringsAsFactors = FALSE
  )

  failed_df <- failed_df[!is.na(failed_df$error), , drop = FALSE]

  write.csv(
    failed_df,
    file = file.path(
      dir_out_country,
      paste0("commodity_failed_years_", country_slug, ".csv")
    ),
    row.names = FALSE
  )

  saveRDS(
    list(
      years_use = years_use,
      transitions = transitions_use$transition,
      country_original = target_country_original,
      country_used = target_country_ls,
      country_slug = country_slug,
      formula = "S_(c,r,y) = max(A_(c,r,y) - A_(c,r,y-1), 0) / sum_c max(A_(c,r,y) - A_(c,r,y-1), 0)",
      commodity_area_definition = "A_(c,r,y) = A_(c,rf,r,y) + A_(c,ir,r,y)",
      denominator_definition = "sum of the positive commodity-specific area changes between consecutive time steps (y-1 -> y); commodities with non-positive change get S = 0; if no commodity expanded, all S = 0",
      year_convention = "Share of transition y-1 -> y is stored in column y (end year), matching script 08",
      absolute_share_file = paste0("commodity_shares_absolute_", country_slug, "_1995_2015.csv"),
      input_dir = dir_scen_crops,
      file_format = "semicolon-separated csv"
    ),
    file = file.path(
      dir_out_country,
      paste0("commodity_shares_", country_slug, "_metadata.rds")
    )
  )

  n_failed_years <- sum(!is.na(unlist(failed_years)))
  n_missing_transitions <- nrow(transitions_use) - length(results_transitions)

  all_failed <- n_missing_transitions == nrow(transitions_use)

  return(data.frame(
    country_original = target_country_original,
    country_used = target_country_ls,
    country_slug = country_slug,
    status = ifelse(all_failed, "failed", "success"),
    n_failed_years = n_failed_years,
    n_missing_transitions = n_missing_transitions,
    error = ifelse(all_failed, "all transitions failed", NA_character_),
    stringsAsFactors = FALSE
  ))
}

# ---------------------------------------------------------
# WRAPPER FOR MASTER PIPELINE
# ---------------------------------------------------------
process_country_commodity_sf <- function(country_sf) {
  country_name <- country_sf$name_long[1]

  process_country_commodity(country_name)
}
