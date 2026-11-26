# =========================================================
# check_v3_results.R
# Consistency check of all v3 results (BF_NEE_v3_Countries)
#
#   module load R
#   Rscript ~/Symobio/Scripts_C_v3/check_v3_results.R
# =========================================================

dir_bf <- path.expand(Sys.getenv("BFNEE_BF_DIR", unset = "~/Symobio/Data/Processed/BF_NEE_v3_Countries"))

files <- list.files(dir_bf, pattern = "^BF_NEE_summary_.*\\.csv$",
                    recursive = TRUE, full.names = TRUE)

if (length(files) == 0) stop("No summary files found in ", dir_bf)

s <- do.call(rbind, lapply(files, function(x) {
  d <- read.csv(x)
  d$iso3 <- basename(dirname(x))
  d
}))

tol <- function(x) 1e-6 * pmax(abs(x), 1)

checks <- data.frame(
  check = c(
    "Countries with v3 summary",
    "Countries with != 4 periods",
    "Constrained > unconstrained loss",
    "Ecosystem classes do not add up",
    "Cropland + pasture BF-NEE != total",
    "Cropland + pasture loss != total",
    "Sum over classes/commodities != BF-NEE",
    "BF-NEE missing (NA)",
    "P_pasture missing (NA)"
  ),
  value = c(
    length(unique(s$iso3)),
    sum(table(s$iso3) != 4),
    sum(s$total_natural_loss_area > s$total_natural_loss_area_unconstrained * (1 + 1e-9), na.rm = TRUE),
    sum(abs(s$class_sum_minus_total) > tol(s$total_natural_loss_area), na.rm = TRUE),
    sum(abs(s$BF_NEE_cropland + s$BF_NEE_pasture - s$BF_NEE) > tol(s$BF_NEE), na.rm = TRUE),
    sum(abs(s$total_natural_loss_area_cropland + s$total_natural_loss_area_pasture -
              s$total_natural_loss_area) > tol(s$total_natural_loss_area), na.rm = TRUE),
    sum(abs(s$BF_NEE_sum_over_classes - s$BF_NEE) > tol(s$BF_NEE), na.rm = TRUE),
    sum(is.na(s$BF_NEE)),
    sum(is.na(s$P_pasture))
  ),
  expected = c("155", "0", "0", "0", "0", "0", "0", "0", "few")
)

cat("\n================ CHECKS ================\n")
print(checks, row.names = FALSE)

# Which country-periods fail a check?
bad <- s[
  (s$total_natural_loss_area > s$total_natural_loss_area_unconstrained * (1 + 1e-9)) |
    (abs(s$class_sum_minus_total) > tol(s$total_natural_loss_area)) |
    (abs(s$BF_NEE_cropland + s$BF_NEE_pasture - s$BF_NEE) > tol(s$BF_NEE)) |
    is.na(s$BF_NEE),
  c("iso3", "transition", "BF_NEE", "BF_NEE_cropland", "BF_NEE_pasture",
    "total_natural_loss_area", "total_natural_loss_area_unconstrained")
]

if (nrow(bad) > 0) {
  cat("\nCountry-periods failing a check:\n")
  print(bad, row.names = FALSE)
}

if (any(is.na(s$P_pasture))) {
  cat("\nCountries without GLORIA grazing-livestock factor:",
      paste(sort(unique(s$iso3[is.na(s$P_pasture)])), collapse = ", "), "\n")
}

# Global totals per period
vars <- c("total_natural_loss_area_unconstrained",
          "total_agriculture_gain_cropland", "total_agriculture_gain_pasture",
          "total_natural_loss_area",
          "total_natural_loss_area_cropland", "total_natural_loss_area_pasture",
          "BF_NEE_cropland", "BF_NEE_pasture", "BF_NEE")

tot <- aggregate(s[, vars], by = list(period = s$transition), FUN = sum, na.rm = TRUE)

short <- c("unconstr_loss", "gain_crop", "gain_past", "constr_loss",
           "loss_crop", "loss_past", "BF_crop", "BF_past", "BF_NEE")
names(tot) <- c("period", short)

cat("\n========== GLOBAL TOTALS (km2) ==========\n")
print(format(tot, big.mark = ",", scientific = FALSE, digits = 4), row.names = FALSE)

tot$share_pasture_loss <- round(tot$loss_past / tot$constr_loss, 3)
tot$share_pasture_BF   <- round(tot$BF_past / tot$BF_NEE, 3)
cat("\nShare of pasture in constrained loss and in BF-NEE:\n")
print(tot[, c("period", "share_pasture_loss", "share_pasture_BF")], row.names = FALSE)

# Top 10 countries in the latest period
last <- s[s$transition == max(s$transition), ]
last <- last[order(-last$BF_NEE), c("iso3", "BF_NEE", "BF_NEE_cropland", "BF_NEE_pasture",
                                    "total_natural_loss_area", "P_pasture")]
cat("\n===== TOP 10 COUNTRIES,", max(s$transition), "=====\n")
print(format(head(last, 10), big.mark = ",", scientific = FALSE, digits = 4), row.names = FALSE)

out <- file.path(dir_bf, "check_v3_results_summary_all_countries.csv")
write.csv(s, out, row.names = FALSE)
cat("\nAll summaries combined in:", out, "\n")
