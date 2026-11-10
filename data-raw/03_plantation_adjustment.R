# =========================================================
# 03_plantation_adjustment.R
#
# APPROXIMATE v3.1 correction while the cluster is unavailable:
# plantations (IUCN 14.3) and artificial aquatic classes are
# treated as anthropogenic, i.e. no longer as natural ecosystems.
#
# The exact correction needs the per-pixel rasters (step 08 with
# the classes added to artificial_codes). From the website tables
# it is approximated as:
#
#   Loss_unconstrained_new = N - Loss_removed           (exact)
#   R_new                  = R - R_removed              (approximation)
#   BF-NEE_new             = BF-NEE - BF-NEE_removed    (approximation)
#
# R - R_removed is a LOWER bound of the exact value; the old R is
# the upper bound. Because GlobES is rescaled proportionally to
# LandSHIFT in step 04, the lower bound equals the exact value in
# all pixels where GlobES itself does not shift area between
# classes, so the approximation is expected to be close.
#
# Idempotent: reads the original v3 tables from data/v3_original/
# (created on the first run) and writes the adjusted tables to data/.
#
#   Rscript data-raw/03_plantation_adjustment.R
#
# DELETE this step once step 08 has been rerun with the classes
# below in artificial_codes and 01_build_website_data.R has been
# run again (that overwrites data/ with exact values).
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

dir_data <- "data"
dir_orig <- file.path(dir_data, "v3_original")

# Classes that are anthropogenic in v3.1 (were counted as natural in v3)
removed_classes <- c(
  "14.3",                     # Plantations
  "15.1", "15.2", "15.3",     # Water storage areas, ponds, aquaculture ponds
  "15.7", "15.9", "15.10",    # Irrigated land, canals, artificial karst
  "15.12", "15.13"            # Mariculture cages, mari/brackish-culture ponds
)

tables <- c("country_ecosystem", "country_transition", "country_commodity",
            "country_commodity_ecosystem", "build_info")

# ---------------------------------------------------------
# keep the original v3 tables once
# ---------------------------------------------------------
if (!dir.exists(dir_orig)) {
  dir.create(dir_orig, recursive = TRUE)
  for (t in tables) {
    file.copy(file.path(dir_data, paste0(t, ".csv")),
              file.path(dir_orig, paste0(t, ".csv")))
  }
  message("Original v3 tables copied to ", dir_orig)
}

rd <- function(t) {
  ct <- if (t == "country_ecosystem") cols(ecosystem_class = col_character()) else cols()
  read_csv(file.path(dir_orig, paste0(t, ".csv")), show_col_types = FALSE,
           guess_max = 1e5, col_types = ct)
}

ce  <- rd("country_ecosystem")
ct  <- rd("country_transition")
cc  <- rd("country_commodity")
cce <- rd("country_commodity_ecosystem")
bi  <- rd("build_info")

# ---------------------------------------------------------
# 1 removed part per country and period
# ---------------------------------------------------------
rem <- ce %>%
  filter(ecosystem_class %in% removed_classes) %>%
  group_by(iso3, transition) %>%
  summarise(
    n_removed      = n_distinct(ecosystem_class),
    unc_removed    = sum(loss_unconstrained_km2, na.rm = TRUE),
    con_removed    = sum(loss_constrained_km2, na.rm = TRUE),
    crop_removed   = sum(loss_cropland_km2, na.rm = TRUE),
    past_removed   = sum(loss_pasture_km2, na.rm = TRUE),
    bf_removed     = sum(bfnee_km2, na.rm = TRUE),
    .groups = "drop"
  )

# share of the cropland and pasture part of the "Modified" group that
# remains (needed for country_commodity_ecosystem, which is by group)
mod_group <- "Modified (counted as natural)"

mod_keep <- ce %>%
  filter(eco_group == mod_group) %>%
  group_by(iso3, transition) %>%
  summarise(
    crop_all  = sum(loss_cropland_km2, na.rm = TRUE),
    past_all  = sum(loss_pasture_km2, na.rm = TRUE),
    crop_keep = sum(loss_cropland_km2[!ecosystem_class %in% removed_classes], na.rm = TRUE),
    past_keep = sum(loss_pasture_km2[!ecosystem_class %in% removed_classes], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    f_crop = ifelse(crop_all > 0, crop_keep / crop_all, 1),
    f_past = ifelse(past_all > 0, past_keep / past_all, 1)
  ) %>%
  select(iso3, transition, f_crop, f_past)

# ---------------------------------------------------------
# 2 country_ecosystem: drop the removed classes
# ---------------------------------------------------------
ce_new <- ce %>% filter(!ecosystem_class %in% removed_classes)

# ---------------------------------------------------------
# 3 country_transition: subtract the removed part
# ---------------------------------------------------------
ct_new <- ct %>%
  left_join(rem, by = c("iso3", "transition")) %>%
  mutate(across(c(n_removed, unc_removed, con_removed, crop_removed,
                  past_removed, bf_removed), ~ coalesce(.x, 0))) %>%
  mutate(
    # BF-NEE of the removed part, split by basis
    bf_crop_removed = crop_removed * weight_sum,
    bf_past_removed = past_removed * coalesce(P_pasture, 0),

    f_unc = ifelse(loss_unconstrained_km2 > 0,
                   (loss_unconstrained_km2 - unc_removed) / loss_unconstrained_km2, 1),
    bfnee_unconstrained_km2 = bfnee_unconstrained_km2 * f_unc,

    n_natural_classes      = n_natural_classes - n_removed,
    loss_unconstrained_km2 = loss_unconstrained_km2 - unc_removed,
    loss_constrained_km2   = loss_constrained_km2 - con_removed,
    loss_cropland_km2      = loss_cropland_km2 - crop_removed,
    loss_pasture_km2       = loss_pasture_km2 - past_removed,
    bfnee_km2              = bfnee_km2 - bf_removed,
    bfnee_cropland_km2     = bfnee_cropland_km2 - bf_crop_removed,
    bfnee_pasture_km2      = bfnee_pasture_km2 - bf_past_removed,
    bfnee_sum_over_classes_km2 = bfnee_sum_over_classes_km2 - bf_removed,
    share_loss_attributed  = ifelse(loss_unconstrained_km2 > 0,
                                    loss_constrained_km2 / loss_unconstrained_km2, NA_real_)
  ) %>%
  mutate(across(c(loss_unconstrained_km2, loss_constrained_km2, loss_cropland_km2,
                  loss_pasture_km2, bfnee_km2, bfnee_cropland_km2, bfnee_pasture_km2,
                  bfnee_sum_over_classes_km2), ~ pmax(.x, 0))) %>%
  select(all_of(names(ct)))

# recompute the class-sum check from the adjusted class table
chk <- ce_new %>%
  group_by(iso3, transition) %>%
  summarise(class_sum = sum(loss_constrained_km2, na.rm = TRUE), .groups = "drop")

ct_new <- ct_new %>%
  left_join(chk, by = c("iso3", "transition")) %>%
  mutate(class_sum_minus_total = coalesce(class_sum, 0) - loss_constrained_km2,
         class_sum_rel_error = ifelse(loss_constrained_km2 > 0,
                                      abs(class_sum_minus_total) / loss_constrained_km2, 0)) %>%
  select(all_of(names(ct)))

# ---------------------------------------------------------
# 4 country_commodity: new loss basis x unchanged weight
# ---------------------------------------------------------
cc_new <- cc %>%
  left_join(rem %>% select(iso3, transition, crop_removed, past_removed),
            by = c("iso3", "transition")) %>%
  mutate(
    removed = case_when(loss_basis == "pasture" ~ coalesce(past_removed, 0),
                        TRUE ~ coalesce(crop_removed, 0)),
    loss_basis_km2 = pmax(loss_basis_km2 - removed, 0),
    bfnee_km2 = loss_basis_km2 * weight
  ) %>%
  select(all_of(names(cc)))

# ---------------------------------------------------------
# 5 country_commodity_ecosystem: scale the "Modified" group
# ---------------------------------------------------------
cce_new <- cce %>%
  left_join(mod_keep, by = c("iso3", "transition")) %>%
  mutate(
    f = case_when(
      eco_group != mod_group ~ 1,
      commodity == "Grazing livestock" ~ coalesce(f_past, 1),
      TRUE ~ coalesce(f_crop, 1)
    ),
    bfnee_km2 = bfnee_km2 * f
  ) %>%
  select(all_of(names(cce)))

# ---------------------------------------------------------
# 6 build info
# ---------------------------------------------------------
bi_new <- bi %>%
  mutate(pipeline_version = paste0(
    "v3.1 (approximation): plantations (IUCN 14.3) and artificial aquatic classes ",
    "treated as anthropogenic; derived from the v3 tables, exact rerun pending | ",
    pipeline_version),
    adjusted_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z"))

# ---------------------------------------------------------
# write
# ---------------------------------------------------------
wr <- function(x, t) write_csv(x, file.path(dir_data, paste0(t, ".csv")), na = "NA")
wr(ce_new, "country_ecosystem")
wr(ct_new, "country_transition")
wr(cc_new, "country_commodity")
wr(cce_new, "country_commodity_ecosystem")
wr(bi_new, "build_info")

# ---------------------------------------------------------
# report
# ---------------------------------------------------------
cmp <- ct %>%
  group_by(transition) %>%
  summarise(bf_v3 = sum(bfnee_km2, na.rm = TRUE), .groups = "drop") %>%
  left_join(ct_new %>% group_by(transition) %>%
              summarise(bf_v31 = sum(bfnee_km2, na.rm = TRUE), .groups = "drop"),
            by = "transition") %>%
  mutate(change = bf_v31 / bf_v3 - 1)

message("Global BF-NEE (all countries in the tables), v3 vs. v3.1 approximation:")
print(cmp)

# consistency checks
stopifnot(all(abs(ct_new$bfnee_cropland_km2 + ct_new$bfnee_pasture_km2 - ct_new$bfnee_km2) <=
                1e-6 * pmax(ct_new$bfnee_km2, 1) + 1e-6 | ct_new$bfnee_km2 == 0))
message("Done.")
