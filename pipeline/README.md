# BF-NEE calculation pipeline (v3)

R scripts and SLURM jobs that calculate BF-NEE per producing country from the
spatial input data. The tables on the website (`data/*.csv`) are built from the
output of this pipeline with `data-raw/01_build_website_data.R`.

The scripts are the version used for the v3 run on the Leipzig University HPC
cluster (October 2026). They are kept unchanged here, so the published results can
be traced back to the code.

## Steps (per country)

| Step | Script | What it does |
|---|---|---|
| 01 | `01_loaddata_landshift.R` | Reads LandSHIFT (cell area, rainfed/irrigated cropland, grazing fraction, livestock density); derives intensive pasture from a country-specific stocking-rate threshold |
| 02 | `02_loaddata_globes.R` | Reads the GlobES ecosystem shares (IUCN Habitats Classification Scheme) and cuts them to the country |
| 03 | `03_raster_adaptation.R` | Resamples GlobES to the LandSHIFT grid |
| 04 | `04_raster_adaptation_shares.R` | Couples GlobES to LandSHIFT: cropland classes = LandSHIFT cropland, class 14.2 = LandSHIFT intensive pasture, the other classes rescaled so each cell sums to 1 |
| 05 | `05_change_in_ecosystem_extent.R` | Change in area of every ecosystem class per five-year period |
| 06 | `06_Extraction_Commodity_Shares.R` | Commodity shares S_c from the expansion of each crop's area (shrinking crops get 0) |
| 07 | `07_Extraction_Production_Shares.R` | GLORIA production shares P for German final demand, incl. the pseudo-commodity "Grazing livestock" |
| 08 | `08_calculating_indicator.R` | Agriculture-constrained loss R = min(N, Gain), split into cropland and pasture parts and onto ecosystem classes, attributed with W = S × P → BF-NEE |

Periods: 1995–2000, 2000–2005, 2005–2010, 2010–2015.

## Running on the cluster

| File | Purpose |
|---|---|
| `submit_all_tiers.sh` | Starts a full run (login node: `bash submit_all_tiers.sh`) |
| `tiers.conf` | Memory/time tiers and paths |
| `tier_controller.job` | Submitted automatically; moves countries that ran out of memory/time to the next tier |
| `pipeline_tiers_common.sh` | Shared functions (sourced) |
| `job_all_countries.job`, `big_countries.job` | Simple array jobs without tiers |
| `run_country_by_iso_pipeline.R`, `run_one_country_pipeline.R`, `run_step08_by_iso.R` | Run steps 01–08 for one country; completed steps are skipped via checkpoint files in `Pipeline_Status` |
| `step08_iso_list.txt`, `big_country_iso_list.txt`, `medium_country_iso_list.txt` | Country lists |
| `check_v3_results.R` | Consistency checks and global totals of all results |

## Before running elsewhere

- **Paths:** input and output folders are set at the top of every script
  (`~/Symobio/Data/...`), and the SLURM files contain absolute paths of the Leipzig
  cluster (`SCRIPT_DIR`, `--output`, `STATUS_DIR`). Adapt them to the new system.
- **Partitions/memory:** `tiers.conf` uses the Leipzig partitions `paula` and
  `sirius`; large countries need up to several TB of RAM in step 03/04.
- **Input data are not in this repository** (size, usage rights): LandSHIFT
  (`merge_var3`, basemap, `Commodities/Scen_crops_*.csv`), GlobES ecosystem shares,
  and the GLORIA release 59 production shares for German final demand.
- R packages: terra, sf, stringr, dplyr, data.table, rnaturalearth (see the
  `library()` calls at the top of each script).

## v3.1 (plantations) – pending rerun

The website shows v3.1, in which plantations (IUCN 14.3) and artificial aquatic
classes (15.1, 15.2, 15.3, 15.7, 15.9, 15.10, 15.12, 15.13) are no longer natural.
These values are currently an approximation from the v3 tables
(`data-raw/03_plantation_adjustment.R`). For the exact values, add these classes to
`artificial_codes` in `08_calculating_indicator.R`, rerun step 08 for all countries
and rebuild the website data.
