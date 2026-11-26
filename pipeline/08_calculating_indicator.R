library(terra)
library(stringr)

# =========================================================
# 08 BF-NEE indicator - v3 (cropland + intensive pasture)
#
# Changes compared to v2:
# - 14.2 (pastureland) is an agricultural class, coupled to
#   LandSHIFT intensive pasture in step 04 -> no longer natural
# - agricultural expansion = NET change of cropland + pasture:
#     Gain_(p,y) = max(dA_crop + dA_past, 0)
#   (pasture converted to cropland is not counted as expansion)
# - the constrained loss R is split per pixel into a cropland
#   and a pasture part in proportion to the positive changes:
#     s_past = max(dA_past,0) / (max(dA_crop,0) + max(dA_past,0))
#     R_crop = R * (1 - s_past),  R_past = R * s_past
# - R_crop is attributed to crops with W_c = S_c * P_c (as before)
# - R_past is attributed to "Grazing livestock" with P_past
# - the split onto ecosystem classes (v2) is kept for both parts
# =========================================================

# ---------------------------------------------------------
# SETTINGS
# ---------------------------------------------------------
years_use <- c(2000, 2005, 2010, 2015)

transitions <- c(
  "1995_2000",
  "2000_2005",
  "2005_2010",
  "2010_2015"
)

dir_dA_base     <- "~/Symobio/Data/Processed/GlobES_dA_v3_Countries"
dir_comm_base   <- "~/Symobio/Data/Processed/Commodity_shares_Countries"
dir_gloria_base <- "~/Symobio/Data/Processed/GLORIA_production_shares_v3_Countries"
dir_out_base    <- "~/Symobio/Data/Processed/BF_NEE_v3_Countries"

dir.create(dir_out_base, recursive = TRUE, showWarnings = FALSE)

pasture_commodity <- "Grazing livestock"

# ---------------------------------------------------------
# AGRICULTURAL ECOSYSTEM CLASSES
# ---------------------------------------------------------
crop_codes    <- c("14.1", "15.8")   # <- LandSHIFT cropland (IR + RF)
pasture_codes <- c("14.2")           # <- LandSHIFT intensive pasture

agri_codes <- c(crop_codes, pasture_codes)

# ---------------------------------------------------------
# ARTIFICIAL / NON-NATURAL ECOSYSTEM CLASSES
# (agricultural + excluded anthropogenic classes)
# ---------------------------------------------------------
artificial_codes <- c(
  agri_codes,
  "14.4",
  "14.5",
  "15.4",
  "15.5",
  "15.6",
  "15.11"
)

# ---------------------------------------------------------
# HELPER FUNCTIONS
# ---------------------------------------------------------
safe_filename <- function(x) {
  x <- str_replace_all(x, "[^[:alnum:]]+", "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  tolower(x)
}

standardize_country_name_commodity <- function(x) {
  x <- trimws(x)

  lookup <- c(
    "Democratic Republic of the Congo" = "Congo, The Democratic Republic of the",
    "Republic of the Congo" = "Congo",
    "Côte d'Ivoire" = "Cote d'Ivoire",
    "Czechia" = "Czech Republic",
    "North Korea" = "Korea, Democratic People's Republic of",
    "South Korea" = "Korea, Republic of",
    "Laos" = "Lao People's Democratic Republic",
    "Syria" = "Syrian Arab Republic",
    "Vietnam" = "Viet Nam",
    "Russia" = "Russian Federation",
    "Macedonia" = "Macedonia, The Former Yugoslav Republic of",
    "Iran" = "Iran, Islamic Republic of",
    "Palestine" = "Palestinian Territory, Occupied",
    "Brunei" = "Brunei Darussalam"
  )

  ifelse(x %in% names(lookup), lookup[x], x)
}

standardize_country_name_gloria <- function(x) {
  x <- trimws(x)

  lookup <- c(
    "Côte d'Ivoire" = "Cote d'Ivoire",
    "Czechia" = "Czech Republic",
    "Russia" = "Russian Federation",
    "Vietnam" = "Viet Nam",
    "Republic of the Congo" = "Rep Congo",
    "Democratic Republic of the Congo" = "DR Congo",
    "Ethiopia" = "DR Ethiopia",
    "Syria" = "Syria",
    "Laos" = "Laos",
    "Iran" = "Iran",
    "Palestine" = "Palestine",
    "Brunei" = "Brunei Darussalam",
    "United States" = "United States of America"
  )

  ifelse(x %in% names(lookup), lookup[x], x)
}

# Exact file-name matching (see v2)
find_country_file <- function(files, prefix, slugs, suffix, label) {

  candidates <- unique(paste0(prefix, slugs, suffix))

  for (cand in candidates) {
    hit <- files[tolower(basename(files)) == tolower(cand)]

    if (length(hit) > 1) {
      warning("More than one ", label, " file found for ", cand,
              ". Using first match:\n", hit[1])
    }

    if (length(hit) >= 1) {
      return(hit[1])
    }
  }

  stop("No ", label, " file found. Tried: ", paste(candidates, collapse = ", "))
}

# Sum over the layers of a stack; zero layer if no layer selected
sum_layers <- function(r, codes, template) {
  codes <- intersect(codes, names(r))
  if (length(codes) == 0) return(template * 0)
  app(r[[codes]], sum, na.rm = TRUE)
}

# ---------------------------------------------------------
# PROCESS ONE COUNTRY
# ---------------------------------------------------------
process_country_bf_nee <- function(country_sf) {

  iso3 <- country_sf$iso_a3[1]
  country_name <- country_sf$name_long[1]

  suffix <- paste0("_", tolower(iso3))

  commodity_country_name <- standardize_country_name_commodity(country_name)
  gloria_country_name    <- standardize_country_name_gloria(country_name)

  original_slug  <- safe_filename(country_name)
  commodity_slug <- safe_filename(commodity_country_name)
  gloria_slug    <- safe_filename(gloria_country_name)

  message("--------------------------------------------------")
  message("Processing BF-NEE v3 for ", country_name, " (", iso3, ")")
  message("--------------------------------------------------")

  dir_dA <- file.path(dir_dA_base, iso3)
  dir_out_bf <- file.path(dir_out_base, iso3)

  dir.create(dir_out_bf, recursive = TRUE, showWarnings = FALSE)

  if (!dir.exists(dir_dA)) {
    stop("dA directory not found (run steps 04/05 v3): ", dir_dA)
  }

  # ---------------------------------------------------------
  # FIND COMMODITY AND GLORIA FILES
  # ---------------------------------------------------------
  comm_files <- list.files(dir_comm_base, pattern = "_1995_2015\\.csv$",
                           recursive = TRUE, full.names = TRUE)

  gloria_files <- list.files(dir_gloria_base, pattern = "_1995_2015\\.csv$",
                             recursive = TRUE, full.names = TRUE)

  S_file <- find_country_file(comm_files, "commodity_shares_",
                              c(original_slug, commodity_slug),
                              "_1995_2015.csv", "commodity share")

  P_file <- find_country_file(gloria_files, "gloria_production_shares_",
                              c(original_slug, gloria_slug),
                              "_1995_2015.csv", "GLORIA production share")

  message("Commodity share file: ", S_file)
  message("GLORIA production share file: ", P_file)

  S_df <- read.csv(S_file, check.names = FALSE)
  P_df <- read.csv(P_file, check.names = FALSE)

  if (!(pasture_commodity %in% P_df$commodity)) {
    stop("'", pasture_commodity, "' missing in GLORIA file (run step 07 v3): ", P_file)
  }

  # ---------------------------------------------------------
  # BUILD WEIGHTS
  # crops:   W_c = S_c * P_c     (applied to R_crop)
  # pasture: W   = P_past        (applied to R_past)
  # ---------------------------------------------------------
  weights_list <- list()

  for (yr in years_use) {

    yr_col <- as.character(yr)

    if (!(yr_col %in% names(S_df))) stop("Year column missing in commodity shares: ", yr_col)
    if (!(yr_col %in% names(P_df))) stop("Year column missing in GLORIA shares: ", yr_col)

    S_tmp <- S_df[, c("commodity", yr_col)]
    names(S_tmp) <- c("commodity", "S")

    P_tmp <- P_df[P_df$commodity != pasture_commodity, c("commodity", yr_col)]
    names(P_tmp) <- c("commodity", "P")

    W <- merge(S_tmp, P_tmp, by = "commodity", all = TRUE)
    W$loss_basis <- "cropland"

    P_past <- P_df[P_df$commodity == pasture_commodity, yr_col][1]

    W <- rbind(
      W,
      data.frame(commodity = pasture_commodity, S = 1, P = P_past,
                 loss_basis = "pasture")
    )

    W$year <- yr
    W$weight <- W$S * W$P

    weights_list[[yr_col]] <- W

    write.csv(W, file.path(dir_out_bf, paste0("commodity_weights_", tolower(iso3), "_", yr, ".csv")),
              row.names = FALSE)
  }

  weights_all <- do.call(rbind, weights_list)

  write.csv(weights_all,
            file.path(dir_out_bf, paste0("commodity_weights_", tolower(iso3), "_all_years.csv")),
            row.names = FALSE)

  # ---------------------------------------------------------
  # CALCULATE BF-NEE
  # ---------------------------------------------------------
  bf_summary <- data.frame()
  bf_master_list <- list()

  for (i in seq_along(transitions)) {

    trans <- transitions[i]
    yr <- years_use[i]

    message("Calculating BF-NEE v3 for ", iso3, ": ", trans)

    dA_file <- file.path(dir_dA, paste0("globes_dA_", trans, suffix, ".tif"))

    if (!file.exists(dA_file)) stop("dA file not found: ", dA_file)

    dA <- rast(dA_file)

    natural_codes <- setdiff(names(dA), artificial_codes)

    if (length(natural_codes) == 0) {
      stop("No natural ecosystem classes found for transition: ", trans)
    }

    if (length(intersect(agri_codes, names(dA))) == 0) {
      warning("No agricultural classes in dA for ", iso3, " ", trans, ": gain = 0.")
    }

    dA_nat <- dA[[natural_codes]]

    # (29) gross loss per natural class
    lossA_nat <- ifel(dA_nat < 0, -dA_nat, 0)
    names(lossA_nat) <- names(dA_nat)

    # (30) total gross natural loss per pixel
    lossA_nat_pixel <- app(lossA_nat, sum, na.rm = TRUE)
    names(lossA_nat_pixel) <- "natural_loss_unconstrained"

    # ---------------------------------------------------------
    # (31) NET AGRICULTURAL EXPANSION (cropland + pasture)
    # ---------------------------------------------------------
    dA_crop <- sum_layers(dA, crop_codes, lossA_nat_pixel)
    dA_past <- sum_layers(dA, pasture_codes, lossA_nat_pixel)

    dA_crop <- ifel(is.na(dA_crop) & !is.na(lossA_nat_pixel), 0, dA_crop)
    dA_past <- ifel(is.na(dA_past) & !is.na(lossA_nat_pixel), 0, dA_past)

    agri_gain <- ifel(dA_crop + dA_past > 0, dA_crop + dA_past, 0)
    names(agri_gain) <- "agriculture_gain"

    pos_crop <- ifel(dA_crop > 0, dA_crop, 0)
    pos_past <- ifel(dA_past > 0, dA_past, 0)

    share_past <- ifel(pos_crop + pos_past > 0, pos_past / (pos_crop + pos_past), 0)
    names(share_past) <- "pasture_share_of_expansion"

    agri_gain_crop <- agri_gain * (1 - share_past)
    agri_gain_past <- agri_gain * share_past

    # ---------------------------------------------------------
    # (32) AGRICULTURE-CONSTRAINED LOSS + split crop / pasture
    # ---------------------------------------------------------
    lossA_agri_pixel <- ifel(lossA_nat_pixel < agri_gain, lossA_nat_pixel, agri_gain)
    names(lossA_agri_pixel) <- "natural_loss_agriConstrained"

    loss_crop_pixel <- lossA_agri_pixel * (1 - share_past)
    loss_past_pixel <- lossA_agri_pixel * share_past
    names(loss_crop_pixel) <- "natural_loss_cropland"
    names(loss_past_pixel) <- "natural_loss_pasture"

    # ---------------------------------------------------------
    # PROPORTIONAL SPLIT ONTO ECOSYSTEM CLASSES (v2)
    # R_(p,e,y) = R_(p,y) * Loss_(p,e,y) / N_(p,y)
    # ---------------------------------------------------------
    constraint_factor <- ifel(lossA_nat_pixel > 0, lossA_agri_pixel / lossA_nat_pixel, 0)

    lossA_agri_class <- lossA_nat * constraint_factor
    names(lossA_agri_class) <- names(lossA_nat)

    lossA_past_class <- lossA_agri_class * share_past
    names(lossA_past_class) <- names(lossA_nat)

    # ---------------------------------------------------------
    # WEIGHTS OF THIS YEAR
    # ---------------------------------------------------------
    W <- weights_list[[as.character(yr)]]
    W_crop <- W[W$loss_basis == "cropland", ]
    P_past <- W$weight[W$loss_basis == "pasture"][1]

    total_weight_crop <- sum(W_crop$weight, na.rm = TRUE)
    if (is.na(P_past)) P_past <- 0

    # ---------------------------------------------------------
    # LOSS BY ECOSYSTEM CLASS
    # ---------------------------------------------------------
    g_unc  <- global(lossA_nat, "sum", na.rm = TRUE)[, 1]
    g_con  <- global(lossA_agri_class, "sum", na.rm = TRUE)[, 1]
    g_past <- global(lossA_past_class, "sum", na.rm = TRUE)[, 1]

    loss_by_class <- data.frame(
      country_iso3 = iso3,
      country_name = country_name,
      transition = trans,
      year = yr,
      ecosystem_class = names(lossA_nat),
      natural_loss_area_unconstrained = as.numeric(g_unc),
      natural_loss_area_agriConstrained = as.numeric(g_con),
      natural_loss_area_pasture = as.numeric(g_past),
      stringsAsFactors = FALSE
    )

    loss_by_class$natural_loss_area_cropland <-
      loss_by_class$natural_loss_area_agriConstrained - loss_by_class$natural_loss_area_pasture

    # Column kept for compatibility: constrained loss used for the indicator
    loss_by_class$natural_loss_area <- loss_by_class$natural_loss_area_agriConstrained

    loss_by_class$share_of_class_loss_attributed <- ifelse(
      loss_by_class$natural_loss_area_unconstrained > 0,
      loss_by_class$natural_loss_area_agriConstrained / loss_by_class$natural_loss_area_unconstrained,
      NA_real_
    )

    # BF-NEE per ecosystem class (crop part + pasture part)
    loss_by_class$BF_NEE <-
      loss_by_class$natural_loss_area_cropland * total_weight_crop +
      loss_by_class$natural_loss_area_pasture * P_past

    loss_by_class <- loss_by_class[
      is.finite(loss_by_class$natural_loss_area_unconstrained) &
        loss_by_class$natural_loss_area_unconstrained > 0, , drop = FALSE]

    write.csv(loss_by_class,
              file.path(dir_out_bf, paste0("natural_ecosystem_loss_by_class_", tolower(iso3), "_", trans, ".csv")),
              row.names = FALSE)

    # ---------------------------------------------------------
    # RASTERS
    # ---------------------------------------------------------
    wr <- function(r, name) {
      writeRaster(r, file.path(dir_out_bf, paste0(name, "_", tolower(iso3), "_", trans, ".tif")),
                  overwrite = TRUE)
    }

    wr(lossA_nat_pixel, "natural_ecosystem_loss_unconstrained")
    wr(agri_gain, "agriculture_gain_area")
    wr(agri_gain_crop, "agriculture_gain_cropland")
    wr(agri_gain_past, "agriculture_gain_pasture")
    wr(lossA_agri_pixel, "natural_ecosystem_loss_agriConstrained")
    wr(lossA_agri_pixel, "natural_ecosystem_loss_area")          # compatibility (app)
    wr(loss_crop_pixel, "natural_ecosystem_loss_cropland")
    wr(loss_past_pixel, "natural_ecosystem_loss_pasture")
    wr(lossA_agri_class, "natural_ecosystem_loss_agriConstrained_by_class")
    wr(lossA_past_class, "natural_ecosystem_loss_pasture_by_class")

    # ---------------------------------------------------------
    # BF-NEE BY ECOSYSTEM CLASS AND COMMODITY
    # crops:   R_(e,crop) * S_c * P_c
    # pasture: R_(e,past) * P_past
    # ---------------------------------------------------------
    lc <- loss_by_class[, c("country_iso3", "country_name", "transition", "year",
                            "ecosystem_class", "natural_loss_area_unconstrained",
                            "natural_loss_area_agriConstrained",
                            "natural_loss_area_cropland", "natural_loss_area_pasture")]

    bf_crop <- merge(lc, W_crop[, c("commodity", "S", "P", "weight", "loss_basis")], by = NULL)
    bf_crop$natural_loss_area <- bf_crop$natural_loss_area_cropland

    bf_past <- merge(lc, W[W$loss_basis == "pasture", c("commodity", "S", "P", "weight", "loss_basis")],
                     by = NULL)
    bf_past$natural_loss_area <- bf_past$natural_loss_area_pasture

    bf_by_class_commodity <- rbind(bf_crop, bf_past)

    bf_by_class_commodity$commodity_country_name <- commodity_country_name
    bf_by_class_commodity$gloria_country_name <- gloria_country_name

    bf_by_class_commodity$BF_NEE <- bf_by_class_commodity$natural_loss_area * bf_by_class_commodity$weight
    bf_by_class_commodity$BF_NEE_class_commodity <- bf_by_class_commodity$BF_NEE

    bf_by_class_commodity <- bf_by_class_commodity[is.finite(bf_by_class_commodity$BF_NEE), , drop = FALSE]

    bf_by_class_commodity <- bf_by_class_commodity[, c(
      "country_iso3", "country_name", "commodity_country_name", "gloria_country_name",
      "transition", "year", "ecosystem_class",
      "natural_loss_area", "natural_loss_area_unconstrained",
      "natural_loss_area_agriConstrained", "natural_loss_area_cropland",
      "natural_loss_area_pasture",
      "commodity", "loss_basis", "S", "P", "weight",
      "BF_NEE", "BF_NEE_class_commodity"
    ), drop = FALSE]

    bf_master_list[[as.character(yr)]] <- bf_by_class_commodity

    write.csv(bf_by_class_commodity,
              file.path(dir_out_bf, paste0("BF_NEE_by_ecosystem_and_commodity_", tolower(iso3), "_", trans, ".csv")),
              row.names = FALSE)

    # ---------------------------------------------------------
    # BF-NEE MAP
    # ---------------------------------------------------------
    bf_map <- loss_crop_pixel * total_weight_crop + loss_past_pixel * P_past
    wr(bf_map, "BF_NEE_map")

    tot <- function(r) global(r, "sum", na.rm = TRUE)[1, 1]

    bf_total              <- tot(bf_map)
    total_unc             <- tot(lossA_nat_pixel)
    total_gain            <- tot(agri_gain)
    total_gain_crop       <- tot(agri_gain_crop)
    total_gain_past       <- tot(agri_gain_past)
    total_con             <- tot(lossA_agri_pixel)
    total_con_crop        <- tot(loss_crop_pixel)
    total_con_past        <- tot(loss_past_pixel)

    class_sum_diff <- sum(g_con, na.rm = TRUE) - total_con
    rel_diff <- ifelse(total_con > 0, abs(class_sum_diff) / total_con, abs(class_sum_diff))

    if (is.finite(rel_diff) && rel_diff > 1e-6) {
      warning("Ecosystem-class losses do not add up for ", iso3, " ", trans,
              ". Relative difference: ", signif(rel_diff, 4))
    }

    # commodity totals
    commodity_bf <- data.frame(
      country_iso3 = iso3,
      country_name = country_name,
      commodity_country_name = commodity_country_name,
      gloria_country_name = gloria_country_name,
      transition = trans,
      year = yr,
      commodity = W$commodity,
      loss_basis = W$loss_basis,
      S = W$S,
      P = W$P,
      weight = W$weight,
      total_natural_loss_area = ifelse(W$loss_basis == "pasture", total_con_past, total_con_crop),
      total_natural_loss_area_unconstrained = total_unc
    )

    commodity_bf$BF_NEE_commodity <- commodity_bf$total_natural_loss_area * commodity_bf$weight

    write.csv(commodity_bf,
              file.path(dir_out_bf, paste0("BF_NEE_by_commodity_", tolower(iso3), "_", trans, ".csv")),
              row.names = FALSE)

    bf_summary <- rbind(
      bf_summary,
      data.frame(
        country_iso3 = iso3,
        country_name = country_name,
        commodity_country_name = commodity_country_name,
        gloria_country_name = gloria_country_name,
        transition = trans,
        year = yr,
        n_natural_classes = length(natural_codes),
        n_agri_classes = length(intersect(agri_codes, names(dA))),
        total_weight_sum_S_times_P = total_weight_crop,
        P_pasture = P_past,
        total_natural_loss_area_unconstrained = total_unc,
        total_agriculture_gain_area = total_gain,
        total_agriculture_gain_cropland = total_gain_crop,
        total_agriculture_gain_pasture = total_gain_past,
        total_natural_loss_area = total_con,
        total_natural_loss_area_cropland = total_con_crop,
        total_natural_loss_area_pasture = total_con_past,
        share_loss_attributed_to_agri = ifelse(total_unc > 0, total_con / total_unc, NA_real_),
        sum_class_loss_agriConstrained = sum(g_con, na.rm = TRUE),
        class_sum_minus_total = class_sum_diff,
        BF_NEE = bf_total,
        BF_NEE_cropland = total_con_crop * total_weight_crop,
        BF_NEE_pasture = total_con_past * P_past,
        BF_NEE_sum_over_classes = sum(bf_by_class_commodity$BF_NEE, na.rm = TRUE),
        BF_NEE_unconstrained = total_unc * total_weight_crop
      )
    )

    rm(dA, dA_nat, lossA_nat, lossA_nat_pixel, dA_crop, dA_past, agri_gain,
       pos_crop, pos_past, share_past, agri_gain_crop, agri_gain_past,
       lossA_agri_pixel, loss_crop_pixel, loss_past_pixel, constraint_factor,
       lossA_agri_class, lossA_past_class, loss_by_class, bf_by_class_commodity, bf_map)
    gc()
  }

  # ---------------------------------------------------------
  # MASTER TABLE + SUMMARY
  # ---------------------------------------------------------
  bf_master_all_years <- do.call(rbind, bf_master_list)
  rownames(bf_master_all_years) <- NULL

  write.csv(bf_master_all_years,
            file.path(dir_out_bf, paste0("BF_NEE_master_country_commodity_ecosystem_", tolower(iso3), "_all_years.csv")),
            row.names = FALSE)

  write.csv(bf_summary,
            file.path(dir_out_bf, paste0("BF_NEE_summary_", tolower(iso3), ".csv")),
            row.names = FALSE)

  saveRDS(
    list(
      pipeline_version = "v3_pasture",
      country_iso3 = iso3,
      country_name = country_name,
      commodity_country_name = commodity_country_name,
      gloria_country_name = gloria_country_name,
      years_use = years_use,
      transitions = transitions,
      crop_codes = crop_codes,
      pasture_codes = pasture_codes,
      artificial_codes = artificial_codes,
      formula_gain = "Gain_(p,y) = max(dA_crop + dA_past, 0)  (net expansion of cropland + intensive pasture)",
      formula_split = "s_past = max(dA_past,0) / (max(dA_crop,0) + max(dA_past,0)); R_crop = R (1 - s_past); R_past = R s_past",
      formula_R = "R_(p,y) = min(N_(p,y), Gain_(p,y))",
      formula_R_class = "R_(p,e,y) = R_(p,y) * Loss_(p,e,y) / N_(p,y)",
      formula_L = "L_(p,e,c,y) = R_crop_(p,e,y) S_c P_c  |  L_(p,e,past,y) = R_past_(p,e,y) P_past",
      commodity_share_source = S_file,
      production_share_source = P_file,
      dA_source_dir = dir_dA
    ),
    file = file.path(dir_out_bf, paste0("BF_NEE_metadata_", tolower(iso3), ".rds"))
  )

  message("Finished BF-NEE v3 for ", country_name, " (", iso3, ")")

  return(data.frame(
    iso3 = iso3,
    country_name = country_name,
    commodity_country_name = commodity_country_name,
    gloria_country_name = gloria_country_name,
    status = "success",
    error = NA_character_,
    stringsAsFactors = FALSE
  ))
}
