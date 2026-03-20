# ============================================================
# energtx Infographic Generator — Setup & Theme
# ============================================================
# Packages: install once, then source this file from main script
# ============================================================

# -- Install packages (run once) --
pkgs <- c(
 "tidyverse", "showtext", "ggtext", "scales", "ggrepel",
 "treemapify", "patchwork",
 "countrycode", "glue"
)
installed <- pkgs %in% rownames(installed.packages())
if (any(!installed)) install.packages(pkgs[!installed])

library(tidyverse)
library(showtext)
library(ggtext)
library(scales)
library(ggrepel)
library(treemapify)
# ggstream and ggbump not available for R 4.5.0 — using alternatives
library(patchwork)
library(countrycode)
library(glue)

# -- Fonts --
font_add_google("Inter", "inter")
font_add_google("Space Grotesk", "spacegrotesk")
showtext_auto()
showtext_opts(dpi = 300)

# -- energtx Dark Palette --
etx_bg      <- "#0B1120"
etx_panel   <- "#0F172A"
etx_grid    <- "#1E293B"
etx_text    <- "#E2E8F0"
etx_sub     <- "#94A3B8"
etx_accent  <- "#00E5A0"
etx_accent2 <- "#38BDF8"
etx_accent3 <- "#FB923C"
etx_accent4 <- "#F87171"
etx_accent5 <- "#A78BFA"
etx_accent6 <- "#FACC15"
etx_accent7 <- "#EC4899"
etx_accent8 <- "#14B8A6"

etx_palette <- c(
 etx_accent, etx_accent2, etx_accent3, etx_accent4,
 etx_accent5, etx_accent6, etx_accent7, etx_accent8
)

# Fuel-specific colors
etx_coal    <- "#6B7280"
etx_oil     <- "#92400E"
etx_gas     <- "#FB923C"
etx_nuclear <- "#A78BFA"
etx_hydro   <- "#38BDF8"
etx_solar   <- "#FACC15"
etx_wind    <- "#14B8A6"
etx_bio     <- "#22C55E"
etx_other   <- "#64748B"

# -- Custom Theme --
theme_energtx <- function(base_size = 14,
                          title_size = 22,
                          subtitle_size = 13,
                          caption_size = 9) {
 theme_void(base_family = "inter", base_size = base_size) +
   theme(
     plot.background    = element_rect(fill = etx_bg, colour = etx_bg),
     panel.background   = element_rect(fill = etx_bg, colour = etx_bg),
     plot.title         = element_text(
       family = "spacegrotesk", colour = etx_text,
       size = title_size, face = "bold",
       lineheight = 1.2, margin = margin(b = 8),
       hjust = 0
     ),
     plot.subtitle      = element_text(
       colour = etx_sub, size = subtitle_size,
       lineheight = 1.4, margin = margin(b = 16),
       hjust = 0
     ),
     plot.caption       = element_text(
       colour = alpha(etx_sub, 0.7), size = caption_size,
       lineheight = 1.3, margin = margin(t = 12),
       hjust = 0
     ),
     plot.margin        = margin(24, 24, 20, 24),
     axis.text          = element_text(colour = etx_sub, size = base_size * 0.8),
     axis.title         = element_text(colour = etx_sub, size = base_size * 0.85),
     panel.grid.major.y = element_line(colour = etx_grid, linewidth = 0.3),
     panel.grid.major.x = element_blank(),
     panel.grid.minor   = element_blank(),
     legend.position    = "none",
     strip.text         = element_text(
       colour = etx_text, family = "spacegrotesk",
       face = "bold", size = base_size * 0.9
     )
   )
}

# -- Caption builder --
etx_caption <- function(source = "Our World in Data") {
 glue(
   "Source: {source} | energtx.com | info@energtx.com"
 )
}

# -- Save helper --
etx_save <- function(plot, filename, w = 12, h = 6.75) {
 ggsave(
   filename = filename,
   plot = plot,
   width = w, height = h, units = "in", dpi = 300,
   bg = etx_bg
 )
 cat("Saved:", filename, "\n")
}

# -- Output directory --
out_dir <- "C:/Users/bartu/Desktop/energtx-infographics/png"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -- Common fuel color map --
fuel_colors <- c(
 "Coal" = etx_coal, "Oil" = etx_oil, "Gas" = etx_gas,
 "Nuclear" = etx_nuclear, "Hydro" = etx_hydro,
 "Wind" = etx_wind, "Solar" = etx_solar
)

cat("energtx theme loaded.\n")
