# =============================================================================
# Generate Oil Crisis Bluesky Infographics for energtx
# High-quality infographic cards — 3000x1688px (16:9) at 300 DPI
# Topics: Hormuz crisis, oil dependency, JODI data, OPEC, vulnerability
# =============================================================================

library(ggplot2)
library(ggtext)
library(showtext)
library(jsonlite)
library(dplyr)
library(tidyr)
library(glue)

# ---------- Load Data ----------
raw <- fromJSON("src/app/datasets/energy_data_combined.json")
energy <- as.data.frame(raw) %>%
  mutate(year = as.integer(year), value = as.numeric(value)) %>%
  filter(!is.na(value), value > 0)

# ---------- Fonts ----------
font_add_google("Inter", "inter")
showtext_auto()
showtext_opts(dpi = 300)

main_font <- "inter"

# ---------- Output ----------
dir.create("public/bluesky", showWarnings = FALSE, recursive = TRUE)

# ---------- Colors ----------
bg       <- "#0F172A"
emerald  <- "#10B981"
cyan     <- "#06B6D4"
white    <- "#F8FAFC"
slate300 <- "#CBD5E1"
slate400 <- "#94A3B8"
slate600 <- "#475569"
amber    <- "#F59E0B"
violet   <- "#8B5CF6"
rose     <- "#F43F5E"
red      <- "#EF4444"
orange   <- "#F97316"
grid_col <- "#1E293B"

# ---------- Shared theme ----------
bsky_theme <- function(accent = emerald) {
  theme_minimal(base_family = main_font, base_size = 11) +
  theme(
    plot.background    = element_rect(fill = bg, color = NA),
    panel.background   = element_rect(fill = bg, color = NA),
    plot.title         = element_text(color = white, size = 15, face = "bold",
                                       margin = margin(b = 2), lineheight = 1.1),
    plot.subtitle      = element_text(color = slate400, size = 9,
                                       margin = margin(b = 14)),
    plot.caption       = element_text(color = accent, size = 8, hjust = 1,
                                       margin = margin(t = 12)),
    plot.margin        = margin(25, 25, 15, 15),
    axis.text.y        = element_text(color = slate300, size = 9, hjust = 1),
    axis.text.x        = element_text(color = slate600, size = 7.5),
    axis.title         = element_blank(),
    panel.grid.major.x = element_line(color = grid_col, linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none"
  )
}

bsky_save <- function(filename, plot) {
  ggsave(filename, plot, width = 10, height = 5.625, dpi = 300,
         bg = bg, units = "in")
}

# =============================================================================
# 1: Top Crude Oil Producers (JODI data)
# =============================================================================
cat("1/6 Top Crude Oil Producers (JODI)...\n")

d_prod <- energy %>%
  filter(indicator_name == "Crude oil production — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  head(15) %>%
  mutate(
    country_name = factor(country_name, levels = rev(country_name)),
    label = format(round(value), big.mark = ",")
  )

yr_prod <- max(d_prod$year)

p1 <- ggplot(d_prod, aes(x = country_name, y = value)) +
  geom_col(fill = amber, width = 0.6) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.2, color = slate300,
            family = main_font) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = function(x) paste0(format(x, big.mark = ","))) +
  labs(title = glue("Top 15 Crude Oil Producers ({yr_prod})"),
       subtitle = "Thousand barrels per day (KB/D)  |  Source: JODI Oil World Database",
       caption = "energtx.com/subscribe  |  @energtx.bsky.social") +
  bsky_theme(amber)

bsky_save("public/bluesky/bsky-oil-producers-jodi.png", p1)

# =============================================================================
# 2: Top Crude Oil Importers (JODI data)
# =============================================================================
cat("2/6 Top Crude Oil Importers (JODI)...\n")

d_imp <- energy %>%
  filter(indicator_name == "Crude oil imports — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  head(15) %>%
  mutate(
    country_name = factor(country_name, levels = rev(country_name)),
    label = format(round(value), big.mark = ",")
  )

yr_imp <- max(d_imp$year)

p2 <- ggplot(d_imp, aes(x = country_name, y = value)) +
  geom_col(fill = red, width = 0.6) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.2, color = slate300,
            family = main_font) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = function(x) paste0(format(x, big.mark = ","))) +
  labs(title = glue("Top 15 Crude Oil Importers ({yr_imp})"),
       subtitle = "Thousand barrels per day (KB/D)  |  Source: JODI  |  Most exposed to Hormuz disruption",
       caption = "energtx.com/subscribe  |  @energtx.bsky.social") +
  bsky_theme(red)

bsky_save("public/bluesky/bsky-oil-importers-jodi.png", p2)

# =============================================================================
# 3: Total Oil Products Demand (JODI)
# =============================================================================
cat("3/6 Total Oil Products Demand (JODI)...\n")

d_demand <- energy %>%
  filter(indicator_name == "Total oil products demand — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  head(15) %>%
  mutate(
    country_name = factor(country_name, levels = rev(country_name)),
    label = format(round(value), big.mark = ",")
  )

yr_demand <- max(d_demand$year)

p3 <- ggplot(d_demand, aes(x = country_name, y = value)) +
  geom_col(fill = orange, width = 0.6) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.2, color = slate300,
            family = main_font) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = function(x) paste0(format(x, big.mark = ","))) +
  labs(title = glue("Top 15 Countries by Total Oil Products Demand ({yr_demand})"),
       subtitle = "Thousand barrels per day (KB/D)  |  Source: JODI Oil World Database",
       caption = "energtx.com/subscribe  |  @energtx.bsky.social") +
  bsky_theme(orange)

bsky_save("public/bluesky/bsky-oil-demand-jodi.png", p3)

# =============================================================================
# 4: Diesel Demand Top 15 (JODI)
# =============================================================================
cat("4/6 Diesel Demand (JODI)...\n")

d_diesel <- energy %>%
  filter(indicator_name == "Diesel demand — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  head(15) %>%
  mutate(
    country_name = factor(country_name, levels = rev(country_name)),
    label = format(round(value), big.mark = ",")
  )

yr_diesel <- max(d_diesel$year)

p4 <- ggplot(d_diesel, aes(x = country_name, y = value)) +
  geom_col(fill = cyan, width = 0.6) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.2, color = slate300,
            family = main_font) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = function(x) paste0(format(x, big.mark = ","))) +
  labs(title = glue("Top 15 Countries by Diesel Demand ({yr_diesel})"),
       subtitle = "Thousand barrels per day (KB/D)  |  Source: JODI  |  Diesel = industrial backbone",
       caption = "energtx.com/subscribe  |  @energtx.bsky.social") +
  bsky_theme(cyan)

bsky_save("public/bluesky/bsky-diesel-demand-jodi.png", p4)

# =============================================================================
# 5: Crude Oil Production Trend — Top 5 (JODI, 2002-latest)
# =============================================================================
cat("5/6 Crude Oil Production Trend — Top 5...\n")

top5_prod <- energy %>%
  filter(indicator_name == "Crude oil production — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  head(5) %>%
  pull(country_name)

d_trend <- energy %>%
  filter(
    indicator_name == "Crude oil production — JODI (thousand barrels/day)",
    country_name %in% top5_prod,
    year >= 2005
  ) %>%
  mutate(country_name = factor(country_name, levels = top5_prod))

country_colors <- c(amber, red, cyan, emerald, violet)
names(country_colors) <- top5_prod

p5 <- ggplot(d_trend, aes(x = year, y = value, color = country_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(
    data = d_trend %>% group_by(country_name) %>% filter(year == max(year)),
    size = 3
  ) +
  geom_text(
    data = d_trend %>% group_by(country_name) %>% filter(year == max(year)),
    aes(label = country_name),
    hjust = -0.1, size = 3, family = main_font, show.legend = FALSE
  ) +
  scale_color_manual(values = country_colors) +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.25)),
    breaks = seq(2005, 2025, 5)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ","), " KB/D")
  ) +
  labs(title = "Crude Oil Production Trend — Top 5 Producers",
       subtitle = "Thousand barrels per day (2005–present)  |  Source: JODI Oil World Database",
       caption = "energtx.com/subscribe  |  @energtx.bsky.social") +
  bsky_theme(amber) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = grid_col, linewidth = 0.3)
  )

bsky_save("public/bluesky/bsky-oil-production-trend.png", p5)

# =============================================================================
# 6: Energy Import Dependency — Most Vulnerable
# =============================================================================
cat("6/6 Energy Import Dependency...\n")

d_dep <- energy %>%
  filter(indicator_name == "Energy import dependency (% of total)") %>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  head(15) %>%
  mutate(
    country_name = factor(country_name, levels = rev(country_name)),
    label = paste0(round(value, 1), "%"),
    risk = case_when(
      value >= 80 ~ "Critical",
      value >= 60 ~ "High",
      value >= 40 ~ "Moderate",
      TRUE         ~ "Low"
    ),
    fill_color = case_when(
      value >= 80 ~ red,
      value >= 60 ~ orange,
      value >= 40 ~ amber,
      TRUE         ~ emerald
    )
  )

yr_dep <- max(d_dep$year)

p6 <- ggplot(d_dep, aes(x = country_name, y = value, fill = fill_color)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.2, color = slate300,
            family = main_font) +
  scale_fill_identity() +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) paste0(x, "%")) +
  labs(title = glue("Most Energy Import Dependent Countries ({yr_dep})"),
       subtitle = "% of total energy imported  |  Red = critical vulnerability to supply disruptions",
       caption = "energtx.com/subscribe  |  @energtx.bsky.social") +
  bsky_theme(red)

bsky_save("public/bluesky/bsky-energy-import-dependency.png", p6)

cat("\n✓ All 6 oil crisis infographics generated!\n")
