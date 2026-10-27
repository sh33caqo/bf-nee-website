# =========================================================
# BF-NEE interactive map (Shiny)
#
# Data in ./data are built on the cluster with
#   data-raw/02_build_map_data.R
# and are NOT stored in git.
# =========================================================

library(shiny)
library(leaflet)
library(terra)
library(dplyr)
library(ggplot2)

dir_data <- Sys.getenv("BFNEE_MAP_DATA", unset = "data")

if (!file.exists(file.path(dir_data, "country_id.tif"))) {
  stop("Map data not found in '", dir_data, "'. Run data-raw/02_build_map_data.R first.")
}

source(file.path(dir_data, "theme.R"))   # colours + formatting helpers

# ---------------------------------------------------------
# DATA
# ---------------------------------------------------------
transitions <- c("1995_2000", "2000_2005", "2005_2010", "2010_2015")
periods <- setNames(transitions, transition_label(transitions))
period_year <- setNames(c(2000L, 2005L, 2010L, 2015L), transitions)

layer_names <- readLines(file.path(dir_data, "layer_names.txt"))

loss_rasters <- lapply(setNames(transitions, transitions), function(tr) {
  rast(file.path(dir_data, paste0("loss_", tr, ".tif")))
})

country_id <- rast(file.path(dir_data, "country_id.tif"))
country_index <- read.csv(file.path(dir_data, "country_index.csv"), stringsAsFactors = FALSE)

weights <- read.csv(file.path(dir_data, "country_commodity.csv"), stringsAsFactors = FALSE)
weights$transition <- as.character(weights$transition)
if (!"loss_basis" %in% names(weights)) weights$loss_basis <- "cropland"   # v2 data

ct <- read.csv(file.path(dir_data, "country_transition.csv"), stringsAsFactors = FALSE)
ct$transition <- as.character(ct$transition)
ct <- ct[ct$iso3 %in% country_index$iso3, ]

ce <- read.csv(file.path(dir_data, "country_ecosystem.csv"), stringsAsFactors = FALSE,
               colClasses = c(ecosystem_class = "character", transition = "character"))
ce <- ce[ce$iso3 %in% country_index$iso3, ]
if (!"loss_pasture_km2" %in% names(ce)) ce$loss_pasture_km2 <- 0

commodities <- sort(unique(weights$commodity))
commodity_choices <- c("All commodities" = "ALL",
                       setNames(commodities, label_commodity(commodities)))

eco_choices <- c(
  "All natural ecosystems",
  intersect(eco_group_levels, layer_names),
  "Tropical moist forest"
)

country_choices <- c(
  "Whole world" = "WORLD",
  setNames(country_index$iso3, country_index$country_name)[order(country_index$country_name)]
)

indicator_choices <- c(
  "BF-NEE (attributed to German final demand)" = "bfnee",
  "Agriculture-constrained natural loss" = "loss",
  "Unconstrained natural loss" = "unconstrained",
  "Agricultural expansion" = "gain"
)

# ---------------------------------------------------------
# HELPERS
# ---------------------------------------------------------

# Per-country weight for one loss basis ("cropland" or "pasture"):
# cropland: sum (or single) S*P of crops; pasture: P of grazing livestock
weight_table <- function(trans, commodity, basis) {
  w <- weights[weights$transition == trans & weights$loss_basis == basis, ]
  if (commodity != "ALL") w <- w[w$commodity == commodity, ]
  out <- data.frame(iso3 = country_index$iso3, weight = 0)
  if (nrow(w) > 0) {
    s <- tapply(w$weight, w$iso3, sum, na.rm = TRUE)
    out$weight <- as.numeric(s[out$iso3])
    out$weight[is.na(out$weight)] <- 0
  }
  out
}

weight_raster <- function(trans, commodity, basis) {
  w <- weight_table(trans, commodity, basis)
  m <- merge(country_index, w, by = "iso3", all.x = TRUE)
  m$weight[is.na(m$weight)] <- 0
  classify(country_id, cbind(m$country_id, m$weight), others = NA)
}

make_map_raster <- function(trans, indicator, eco, commodity, iso3) {

  r <- loss_rasters[[trans]]

  layer <- switch(indicator,
                  unconstrained = "Unconstrained loss",
                  gain = "Agricultural expansion",
                  eco)

  out <- r[[layer]]

  if (indicator == "bfnee") {
    past_layer <- paste(layer, "| pasture")
    past <- if (past_layer %in% names(r)) r[[past_layer]] else out * 0
    crop <- out - past
    out <- crop * weight_raster(trans, commodity, "cropland") +
      past * weight_raster(trans, commodity, "pasture")
  }

  if (iso3 != "WORLD") {
    id <- country_index$country_id[country_index$iso3 == iso3]
    out <- mask(out, country_id, inverse = TRUE, maskvalues = id)
    out <- trim(out)
  }

  # Drop numerical dust: values below 1e-6 of the maximum are treated as zero
  mx <- global(out, "max", na.rm = TRUE)[1, 1]
  if (!is.finite(mx) || mx <= 0) return(out * NA)
  ifel(out <= mx * 1e-6, NA, out)
}

summary_values <- function(trans, eco, commodity, iso3) {

  sel <- if (iso3 == "WORLD") country_index$iso3 else iso3

  e <- ce[ce$transition == trans & ce$iso3 %in% sel, ]
  if (eco == "Tropical moist forest") {
    e <- e[e$tropical_moist_forest %in% c(TRUE, "TRUE"), ]
  } else if (eco != "All natural ecosystems") {
    e <- e[e$eco_group == eco, ]
  }

  # BF-NEE = R_crop * W_crop + R_past * P_past (per country)
  past_by_country <- tapply(e$loss_pasture_km2, e$iso3, sum, na.rm = TRUE)
  tot_by_country  <- tapply(e$loss_constrained_km2, e$iso3, sum, na.rm = TRUE)

  wc <- weight_table(trans, commodity, "cropland")
  wp <- weight_table(trans, commodity, "pasture")

  isos <- names(tot_by_country)
  crop_part <- tot_by_country[isos] - past_by_country[isos]
  past_part <- past_by_country[isos]

  bfnee <- sum(crop_part * wc$weight[match(isos, wc$iso3)] +
                 past_part * wp$weight[match(isos, wp$iso3)], na.rm = TRUE)

  w <- weights[weights$transition == trans & weights$iso3 %in% sel, ]

  t <- ct[ct$transition == trans & ct$iso3 %in% sel, ]

  list(
    bfnee = bfnee,
    loss_constrained = sum(e$loss_constrained_km2, na.rm = TRUE),
    loss_unconstrained_all = sum(t$loss_unconstrained_km2, na.rm = TRUE),
    agri_gain = sum(t$agri_gain_km2, na.rm = TRUE),
    eco_table = e,
    weights = w
  )
}

# ---------------------------------------------------------
# UI
# ---------------------------------------------------------
ui <- fluidPage(

  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body { font-family: system-ui, -apple-system, 'Segoe UI', sans-serif;
             background: #f9f9f7; color: #0b0b0b; }
      .container-fluid { padding: 0; }
      .container-fluid > .row { margin: 0; }
      .bf-header { background: #1f5c4a; color: #fff; padding: 12px 20px; }
      .bf-header h1 { font-size: 1.25rem; margin: 0; font-weight: 700; }
      .bf-header p { margin: 2px 0 0 0; font-size: 0.85rem; opacity: 0.85; }
      .bf-side { background: #fcfcfb; border-right: 1px solid rgba(11,11,11,.1);
                 padding: 16px 18px; height: calc(100vh - 62px); overflow-y: auto; }
      .bf-side label { font-size: 0.82rem; font-weight: 600; color: #52514e; }
      .bf-stat { margin: 10px 0 14px 0; }
      .bf-stat .v { font-size: 1.55rem; font-weight: 750; line-height: 1.1; }
      .bf-stat .l { font-size: 0.8rem; color: #52514e; }
      .bf-small { font-size: 0.78rem; color: #898781; }
      #map { height: calc(100vh - 62px) !important; }
      .leaflet-container { background: #f1f1ee; }
      hr { margin: 12px 0; border-color: rgba(11,11,11,.1); }
      @media (max-width: 768px) {
        .bf-side { height: auto; border-right: none; }
        #map { height: 70vh !important; }
      }
    "))
  ),

  div(class = "bf-header",
      h1("BF-NEE map"),
      p("Natural ecosystem loss through agricultural expansion, attributed to German final demand")),

  fluidRow(
    column(
      width = 3, class = "bf-side",
      selectInput("period", "Period", periods, selected = tail(periods, 1)),
      selectInput("indicator", "Indicator", indicator_choices),
      conditionalPanel(
        "input.indicator == 'bfnee' || input.indicator == 'loss'",
        selectInput("eco", "Ecosystem group", eco_choices)
      ),
      conditionalPanel(
        "input.indicator == 'bfnee'",
        selectInput("commodity", "Commodity", commodity_choices)
      ),
      selectInput("country", "Producing country", country_choices),
      radioButtons("scale", "Colour scale", c("Logarithmic" = "log", "Linear" = "lin"),
                   inline = TRUE),
      hr(),
      uiOutput("stats"),
      plotOutput("eco_plot", height = "210px"),
      hr(),
      p(class = "bf-small",
        "Click on the map to see the value of a cell. Values are km² per displayed",
        "cell (≈18 km). Totals come from the country tables.")
    ),
    column(width = 9, style = "padding:0", leafletOutput("map"))
  )
)

# ---------------------------------------------------------
# SERVER
# ---------------------------------------------------------
server <- function(input, output, session) {

  eco_sel <- reactive({
    if (input$indicator %in% c("bfnee", "loss")) input$eco else "All natural ecosystems"
  })

  commodity_sel <- reactive({
    if (input$indicator == "bfnee") input$commodity else "ALL"
  })

  map_raster <- reactive({
    make_map_raster(input$period, input$indicator, eco_sel(), commodity_sel(), input$country)
  }) |> bindCache(input$period, input$indicator, eco_sel(), commodity_sel(), input$country)

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 2)) |>
      addProviderTiles(providers$CartoDB.PositronNoLabels) |>
      addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                       options = providerTileOptions(pane = "overlayPane")) |>
      setView(lng = 10, lat = 15, zoom = 2)
  })

  observe({
    r <- map_raster()
    vals <- values(r, mat = FALSE)
    vals <- vals[is.finite(vals) & vals > 0]

    proxy <- leafletProxy("map") |> clearImages() |> clearControls()

    if (length(vals) == 0) {
      proxy |> addControl("No values for this selection.", position = "topright")
      return()
    }

    # Robust colour range (ignore extreme outliers)
    q <- quantile(vals, c(0.02, 0.995), names = FALSE)
    
    if (input$scale == "log") {
      r_disp <- log10(clamp(r, lower = q[1], upper = q[2], values = TRUE))
      rng <- log10(q)
      pal <- colorNumeric(seq_ramp, domain = rng, na.color = "transparent")
      brks <- pretty(rng, n = 5)
      brks <- brks[brks >= rng[1] & brks <= rng[2]]
      legend_labels <- fmt_area(10^brks)
      legend_colors <- pal(brks)
    } else {
      r_disp <- clamp(r, upper = q[2], values = TRUE)
      rng <- c(0, q[2])
      pal <- colorNumeric(seq_ramp, domain = rng, na.color = "transparent")
      brks <- pretty(rng, n = 5)
      brks <- brks[brks <= rng[2]]
      legend_labels <- fmt_area(brks)
      legend_colors <- pal(brks)
    }

    unit <- names(indicator_choices)[indicator_choices == input$indicator]

    proxy |>
      addRasterImage(r_disp, colors = pal, opacity = 0.9, project = FALSE,
                     maxBytes = 32 * 1024^2) |>
      addLegend(position = "bottomright", colors = legend_colors, labels = legend_labels,
                title = HTML(paste0(unit, "<br><span style='font-weight:400'>km² per cell</span>")),
                opacity = 0.9)

    if (input$country != "WORLD") {
      e <- ext(project(as.polygons(ext(r), crs = crs(r)), "EPSG:4326"))
      proxy |> fitBounds(e$xmin, e$ymin, e$xmax, e$ymax)
    }
  })

  observeEvent(input$country, {
    if (input$country == "WORLD") {
      leafletProxy("map") |> setView(lng = 10, lat = 15, zoom = 2)
    }
  })

  # Click -> cell value
  observeEvent(input$map_click, {
    clk <- input$map_click
    pt <- project(vect(cbind(clk$lng, clk$lat), crs = "EPSG:4326"), "EPSG:3857")
    v <- extract(map_raster(), pt)[1, 2]
    cid <- extract(country_id, pt)[1, 2]
    cname <- country_index$country_name[match(cid, country_index$country_id)]
    txt <- paste0(
      "<b>", ifelse(is.na(cname), "–", cname), "</b><br>",
      ifelse(is.na(v), "no value", paste0(signif(v, 3), " km²"))
    )
    leafletProxy("map") |> clearPopups() |> addPopups(clk$lng, clk$lat, txt)
  })

  stats <- reactive({
    summary_values(input$period, eco_sel(), commodity_sel(), input$country)
  })

  output$stats <- renderUI({
    s <- stats()
    where <- if (input$country == "WORLD") "all countries" else
      names(country_choices)[country_choices == input$country]

    stat <- function(v, l) div(class = "bf-stat", div(class = "v", v), div(class = "l", l))

    tagList(
      div(class = "bf-small", paste0(where, " · ", transition_label(input$period))),
      stat(fmt_km2(s$bfnee), paste0("BF-NEE · ", eco_sel(),
                                    if (commodity_sel() != "ALL")
                                      paste0(" · ", label_commodity(commodity_sel())))),
      stat(fmt_km2(s$loss_constrained), paste0("Agriculture-constrained loss · ", eco_sel())),
      stat(fmt_km2(s$loss_unconstrained_all), "Unconstrained natural loss (all ecosystems)"),
      stat(fmt_km2(s$agri_gain), "Agricultural expansion")
    )
  })

  output$eco_plot <- renderPlot({
    s <- stats()
    e <- s$eco_table
    if (nrow(e) == 0) return(NULL)

    d <- aggregate(loss_constrained_km2 ~ eco_group, data = e, FUN = sum)
    d$eco_group <- factor(d$eco_group, levels = rev(eco_group_levels))
    d <- d[d$loss_constrained_km2 > 0, ]

    ggplot(d, aes(loss_constrained_km2, eco_group, fill = eco_group)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = eco_group_colors, guide = "none") +
      scale_x_continuous(labels = fmt_area, expand = expansion(mult = c(0, 0.05))) +
      labs(title = "Constrained loss by ecosystem group", x = "km²", y = NULL) +
      theme_bfnee(base_size = 10, grid = "x")
  }, bg = "transparent")
}

shinyApp(ui, server)
