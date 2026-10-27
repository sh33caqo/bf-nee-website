# =========================================================
# Deploy the BF-NEE map to shinyapps.io
#
# 1. Build the map data first (cluster):
#      sbatch data-raw/build_data.job
# 2. Register your account ONCE in the R console (never in a file!):
#      rsconnect::setAccountInfo(name = "sh33caqo",
#                                token = "<TOKEN>", secret = "<SECRET>")
#    Token + secret: shinyapps.io -> Account -> Tokens
# 3. Run this script from the project root:
#      Rscript shiny/global_map/deploy.R
# =========================================================

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

app_dir <- normalizePath("shiny/global_map", mustWork = TRUE)

if (!file.exists(file.path(app_dir, "data", "country_id.tif"))) {
  stop("Map data missing in ", file.path(app_dir, "data"),
       " - run data-raw/02_build_map_data.R first.")
}

if (nrow(rsconnect::accounts()) == 0) {
  stop("No shinyapps.io account registered. Run rsconnect::setAccountInfo(...) once in the console.")
}

files <- c("app.R", list.files(file.path(app_dir, "data"), recursive = TRUE,
                               full.names = FALSE) |> (\(x) file.path("data", x))())

message("Uploading ", length(files), " files (",
        round(sum(file.size(file.path(app_dir, files))) / 1e6, 1), " MB)")

rsconnect::deployApp(
  appDir = app_dir,
  appFiles = files,
  appName = "bf-nee-global-map",
  appTitle = "BF-NEE map",
  account = "sh33caqo",
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE
)
