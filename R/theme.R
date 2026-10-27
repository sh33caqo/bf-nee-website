# =========================================================
# BF-NEE website: colours, ggplot theme, formatting helpers
# =========================================================
#
# Colour rules
# - Ecosystem groups and commodities always keep the SAME colour
#   (colour follows the entity, never its rank).
# - The categorical orders below were checked for colour-vision
#   deficiency (CVD) and normal-vision separation of neighbours
#   in light and dark mode.
# - Magnitude (maps) uses one hue, light -> dark.
# - More than 8 categories are folded into "Other" (grey).
# ---------------------------------------------------------

library(ggplot2)
library(scales)

# ---------------------------------------------------------
# CHART CHROME
# ---------------------------------------------------------
bf_ink <- list(
  surface   = "#fcfcfb",
  primary   = "#0b0b0b",
  secondary = "#52514e",
  muted     = "#898781",
  grid      = "#e1e0d9",
  axis      = "#c3c2b7",
  other     = "#b5b3ab"
)

bf_brand <- list(
  primary      = "#1f5c4a",
  primary_dark = "#174638",
  accent       = "#d99b3d"
)

# ---------------------------------------------------------
# ECOSYSTEM GROUPS (fixed order and colour)
# ---------------------------------------------------------
eco_group_levels <- c(
  "Forest",
  "Wetlands",
  "Shrubland",
  "Grassland",
  "Savanna",
  "Desert and rocky",
  "Marine and coastal",
  "Modified (counted as natural)"
)

eco_group_colors <- c(
  "Forest"                        = "#008300",
  "Wetlands"                      = "#2a78d6",
  "Shrubland"                     = "#eb6834",
  "Grassland"                     = "#1baf7a",
  "Savanna"                       = "#eda100",
  "Desert and rocky"              = "#e87ba4",
  "Marine and coastal"            = "#6250d6",
  "Modified (counted as natural)" = "#e34948"
)

# ---------------------------------------------------------
# COMMODITIES (7 named slots + Other)
# ---------------------------------------------------------
commodity_named <- c(
  "Oil bearing crops",
  "Cereals n.e.c.",
  "Sugar crops",
  "Spice - beverage - pharmaceutical crops",
  "Fruits",
  "Wheat",
  "Grazing livestock"
)

commodity_colors <- c(
  "Oil bearing crops"                       = "#2a78d6",
  "Cereals n.e.c."                          = "#eb6834",
  "Sugar crops"                             = "#1baf7a",
  "Spice - beverage - pharmaceutical crops" = "#eda100",
  "Fruits"                                  = "#e87ba4",
  "Wheat"                                   = "#008300",
  "Grazing livestock"                       = "#6250d6",
  "Other"                                   = bf_ink$other
)

commodity_labels <- c(
  "Spice - beverage - pharmaceutical crops" = "Spice, beverage & pharma crops",
  "Other crops n.e.c"                       = "Other crops n.e.c.",
  "Oil bearing crops"                       = "Oil-bearing crops"
)

label_commodity <- function(x) {
  out <- ifelse(x %in% names(commodity_labels), commodity_labels[x], x)
  unname(out)
}

# Same as commodity_colors, but keyed by the display label
commodity_colors_lab <- setNames(commodity_colors, label_commodity(names(commodity_colors)))

# Folded commodity as factor with DISPLAY labels (for legends incl. plotly)
fold_commodity_label <- function(x) {
  f <- fold_commodity(x)
  factor(label_commodity(as.character(f)), levels = label_commodity(levels(f)))
}

fold_commodity <- function(x) {
  factor(
    ifelse(x %in% commodity_named, x, "Other"),
    levels = c(commodity_named, "Other")
  )
}

# ---------------------------------------------------------
# CONTINENTS (6 + Other)
# ---------------------------------------------------------
continent_levels <- c(
  "South America", "Africa", "Asia",
  "North America", "Europe", "Oceania", "Other"
)

continent_colors <- c(
  "South America" = "#2a78d6",
  "Africa"        = "#eb6834",
  "Asia"          = "#1baf7a",
  "North America" = "#eda100",
  "Europe"        = "#e87ba4",
  "Oceania"       = "#008300",
  "Other"         = bf_ink$other
)

# Two-measure comparisons (e.g. unconstrained vs. constrained)
# Land-use basis of the attributed loss (v3)
basis_colors <- c(
  "Cropland"          = "#eb6834",
  "Intensive pasture" = "#1baf7a"
)

measure_colors <- c(
  "Agriculture-constrained loss" = "#1f5c4a",
  "Unconstrained natural loss"   = "#b5b3ab",
  "BF-NEE"                       = "#2a78d6",
  "Agricultural expansion"       = "#eb6834"
)

# Sequential ramp (one hue, light -> dark) for maps / heatmaps
seq_ramp <- c(
  "#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5",
  "#256abf", "#184f95", "#0d366b"
)

# ---------------------------------------------------------
# THEME
# ---------------------------------------------------------
theme_bfnee <- function(base_size = 13, grid = "y") {

  t <- theme_minimal(base_size = base_size, base_family = "") +
    theme(
      plot.background  = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),

      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", colour = bf_ink$primary,
                                   size = rel(1.15), margin = margin(b = 4)),
      plot.subtitle = element_text(colour = bf_ink$secondary,
                                   margin = margin(b = 12)),
      plot.caption  = element_text(colour = bf_ink$muted, size = rel(0.8),
                                   hjust = 0, margin = margin(t = 10)),

      axis.title = element_text(colour = bf_ink$secondary, size = rel(0.9)),
      axis.text  = element_text(colour = bf_ink$muted),
      axis.line.x  = element_line(colour = bf_ink$axis, linewidth = 0.4),
      axis.ticks   = element_blank(),

      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = bf_ink$grid, linewidth = 0.35),

      legend.position  = "top",
      legend.justification = "left",
      legend.title = element_text(colour = bf_ink$secondary, size = rel(0.85)),
      legend.text  = element_text(colour = bf_ink$secondary, size = rel(0.85)),
      legend.key.size = unit(0.8, "lines"),

      strip.text = element_text(face = "bold", colour = bf_ink$secondary,
                                hjust = 0, size = rel(0.9)),
      panel.spacing = unit(1.2, "lines"),
      plot.margin = margin(8, 12, 8, 8)
    )

  if (grid == "y") {
    t <- t + theme(panel.grid.major.x = element_blank())
  } else if (grid == "x") {
    t <- t + theme(panel.grid.major.y = element_blank(),
                   axis.line.x = element_blank(),
                   axis.line.y = element_line(colour = bf_ink$axis, linewidth = 0.4))
  } else if (grid == "none") {
    t <- t + theme(panel.grid.major = element_blank())
  }

  t
}

theme_set(theme_bfnee(base_size = 13))

update_geom_defaults("line",  list(linewidth = 0.9))
update_geom_defaults("point", list(size = 2.4))

# ---------------------------------------------------------
# FORMATTING
# ---------------------------------------------------------

# Compact area labels: 12 300 -> "12.3k"
fmt_area <- function(x, digits = 3) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    a <- abs(v)
    if (a >= 1e9) return(paste0(format(signif(v / 1e9, digits), trim = TRUE), "B"))
    if (a >= 1e6) return(paste0(format(signif(v / 1e6, digits), trim = TRUE), "M"))
    if (a >= 1e4) return(paste0(format(signif(v / 1e3, digits), trim = TRUE), "k"))
    if (a > 0 && a < 0.01) return(formatC(v, format = "g", digits = 2))
    format(signif(v, digits), big.mark = ",", trim = TRUE, scientific = FALSE)
  }, character(1))
}

fmt_km2 <- function(x, digits = 3) {
  paste0(fmt_area(x, digits), " km²")
}

fmt_pct <- function(x, accuracy = 1) {
  percent(x, accuracy = accuracy)
}

transition_label <- function(x) {
  gsub("_", "–", x)
}

# ---------------------------------------------------------
# INTERACTIVITY
# Wraps a ggplot in plotly if available (hover tooltips);
# otherwise returns the static ggplot.
# Use aes(text = ...) in the ggplot for tooltip content.
# ---------------------------------------------------------
bf_interactive <- function(p, height = NULL) {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    return(p)
  }

  title <- p$labels$title
  subtitle <- p$labels$subtitle
  title_html <- if (is.null(title)) NULL else paste0(
    "<b>", title, "</b>",
    if (!is.null(subtitle)) paste0("<br><span style='font-size:12px;color:",
                                   bf_ink$secondary, "'>", subtitle, "</span>")
  )

  plotly::ggplotly(p, tooltip = "text", height = height) |>
    plotly::layout(
      title = list(text = title_html, x = 0, xanchor = "left", xref = "paper",
                   y = 1, yref = "container", yanchor = "top",
                   pad = list(t = 14),
                   font = list(size = 15, color = bf_ink$primary)),
      font = list(family = "system-ui, -apple-system, 'Segoe UI', sans-serif"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      hoverlabel = list(bgcolor = "white", bordercolor = bf_ink$axis,
                        font = list(color = bf_ink$primary)),
      legend = list(orientation = "h", x = 0, xanchor = "left",
                    y = 1.02, yanchor = "bottom", title = list(text = "")),
      margin = list(t = if (is.null(subtitle)) 80 else 105, l = 10, r = 10, b = 10)
    ) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d",
                                              "autoScale2d", "toggleSpikelines"))
}
