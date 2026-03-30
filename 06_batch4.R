# ============================================================
# energtx Infographic Generator — Batch 4 (131–150)
# 20 Premium Infographics · nrennie-inspired quality
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(forcats)
library(stringr)
library(glue)
library(scales)
library(ggtext)
library(ggrepel)
library(showtext)
library(sysfonts)
library(countrycode)
library(patchwork)
library(cowplot)
library(ggforce)

# ── Fonts ──────────────────────────────────────────────────
font_add_google("Space Grotesk", "spacegrotesk")
font_add_google("Inter", "inter")
font_add_google("JetBrains Mono", "jetbrains")
showtext_auto()
showtext_opts(dpi = 300)

# ── Colour System ──────────────────────────────────────────
bg_col       <- "#080E1C"
panel_col    <- "#0D1526"
text_col     <- "#E8ECF1"
sub_col      <- "#7B8494"
grid_col     <- "#1A2236"
accent1      <- "#00D4AA"   # teal
accent2      <- "#4ECDC4"   # cyan
accent3      <- "#FF6B6B"   # coral
accent4      <- "#FFE66D"   # yellow
accent5      <- "#A78BFA"   # purple
accent6      <- "#38BDF8"   # sky blue
accent7      <- "#F472B6"   # pink
accent8      <- "#FB923C"   # orange

fuel_cols <- c(
  "Coal" = "#6B7280", "Oil" = "#A0522D", "Gas" = "#FB923C",
  "Nuclear" = "#A78BFA", "Hydro" = "#3B82F6", "Solar" = "#FFE66D",
  "Wind" = "#00D4AA", "Bioenergy" = "#22C55E", "Other renewables" = "#F472B6"
)

continent_cols <- c(
  "Africa" = accent8, "Americas" = accent6, "Asia" = accent3,
  "Europe" = accent5, "Oceania" = accent1
)

# ── Theme ──────────────────────────────────────────────────
theme_etx <- function(base_size = 11) {
  theme_void(base_family = "inter", base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = bg_col, colour = NA),
      panel.background = element_rect(fill = panel_col, colour = NA),
      plot.title = element_textbox_simple(
        colour = text_col, size = 20, face = "bold",
        family = "spacegrotesk", halign = 0,
        margin = margin(b = 4), lineheight = 1.1
      ),
      plot.subtitle = element_textbox_simple(
        colour = sub_col, size = 10, halign = 0,
        margin = margin(b = 14), lineheight = 1.3
      ),
      plot.caption = element_textbox_simple(
        colour = sub_col, size = 7, halign = 0,
        margin = margin(t = 14)
      ),
      axis.text   = element_text(colour = sub_col, size = 9, family = "inter"),
      axis.title  = element_text(colour = sub_col, size = 9, family = "inter"),
      legend.text = element_text(colour = text_col, size = 8),
      legend.title = element_blank(),
      legend.position = "bottom",
      plot.margin = margin(24, 28, 18, 28),
      panel.grid = element_blank()
    )
}

etx_caption <- function(src = "Our World in Data") {
  glue("Source: {src} · **energtx.com** · @energtx.bsky.social")
}

etx_save <- function(p, path, w = 12, h = 6.75) {
  ggsave(path, p, width = w, height = h, dpi = 300, bg = bg_col, device = "png")
  cat(glue("  ✓ Saved: {basename(path)}"), "\n")
}

out_dir <- "/home/fbartuyurdacan/Desktop/energtx_png"
dir.create(out_dir, showWarnings = FALSE)

# ── Data ───────────────────────────────────────────────────
data_file <- "/home/fbartuyurdacan/Desktop/owid-energy-data.csv"
if (!file.exists(data_file)) {
  download.file(
    "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv",
    data_file, quiet = TRUE
  )
}
raw <- read_csv(data_file, show_col_types = FALSE)

etx_countries <- c(
  "United States", "China", "India", "Japan", "Germany", "United Kingdom",
  "France", "Brazil", "Canada", "South Korea", "Russia", "Australia",
  "Italy", "Mexico", "Indonesia", "Turkey", "Saudi Arabia", "Argentina",
  "South Africa", "Spain", "Netherlands", "Belgium", "Poland", "Sweden",
  "Denmark", "Finland", "Austria", "Portugal", "Czech Republic", "Romania",
  "Greece", "Ireland", "Hungary", "Norway", "Switzerland", "New Zealand",
  "Chile", "Colombia", "Egypt", "Iran", "Israel", "Malaysia", "Nigeria",
  "Pakistan", "Philippines", "Thailand", "Vietnam", "United Arab Emirates",
  "Bangladesh", "Kazakhstan", "Ukraine", "Morocco", "Kenya", "Algeria",
  "Peru", "Singapore"
)

energy <- raw %>%
  filter(country %in% etx_countries, year >= 1990) %>%
  mutate(
    iso3 = countrycode(country, "country.name", "iso3c"),
    continent = countrycode(country, "country.name", "continent")
  )

latest_year <- energy %>% filter(!is.na(primary_energy_consumption)) %>% pull(year) %>% max()
latest <- energy %>% filter(year == latest_year)

g7  <- c("United States", "Japan", "Germany", "United Kingdom", "France", "Canada", "Italy")
g20 <- c(g7, "China", "India", "Brazil", "South Korea", "Russia", "Australia",
         "Mexico", "Indonesia", "Turkey", "Saudi Arabia", "Argentina", "South Africa")

cat(glue("Data: {nrow(energy)} rows · Latest: {latest_year}"), "\n\n")

# ============================================================
# 131 — Gas Dependency: Top 20 (Lollipop with gradient)
# ============================================================
cat("131 — Gas Dependency Lollipop\n")

p131_data <- latest %>%
  filter(!is.na(gas_share_energy)) %>%
  slice_max(gas_share_energy, n = 20) %>%
  mutate(country = fct_reorder(country, gas_share_energy))

p131 <- ggplot(p131_data, aes(x = gas_share_energy, y = country)) +
  geom_segment(aes(x = 0, xend = gas_share_energy, yend = country),
               colour = grid_col, linewidth = 0.8) +
  geom_point(aes(colour = gas_share_energy), size = 4) +
  geom_text(aes(label = paste0(round(gas_share_energy, 1), "%")),
            hjust = -0.4, colour = text_col, family = "jetbrains", size = 3) +
  scale_colour_gradient(low = accent8, high = accent3, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Who Depends Most on <span style='color:#FB923C'>Natural Gas</span>?",
    subtitle = glue("Share of primary energy from natural gas (%) · Top 20 countries · {latest_year}"),
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 8.5, hjust = 1),
    panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.2)
  )
etx_save(p131, file.path(out_dir, "131_gas_dependency_top20.png"))


# ============================================================
# 132 — Nuclear Heatmap: 35-Year View
# ============================================================
cat("132 — Nuclear Heatmap\n")

nuc_countries <- c("France", "United States", "China", "South Korea", "Russia",
                   "Japan", "Canada", "United Kingdom", "Germany", "Sweden",
                   "Spain", "India", "Belgium", "Switzerland", "Finland")

p132_data <- energy %>%
  filter(country %in% nuc_countries, !is.na(nuclear_share_elec)) %>%
  mutate(country = factor(country, levels = rev(nuc_countries)))

p132 <- ggplot(p132_data, aes(x = year, y = country, fill = nuclear_share_elec)) +
  geom_tile(colour = bg_col, linewidth = 0.4) +
  scale_fill_gradient2(
    low = panel_col, mid = accent5, high = "#E0AAFF",
    midpoint = 35, limits = c(0, 80),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(breaks = seq(1990, 2025, 5), expand = c(0, 0)) +
  labs(
    title = "<span style='color:#A78BFA'>Nuclear Power</span> Across the World: A 35-Year Heatmap",
    subtitle = "Nuclear share of electricity generation (%) · 1990–present",
    caption = etx_caption("IAEA / OWID"), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 9, hjust = 1),
    axis.text.x = element_text(colour = sub_col),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) +
  guides(fill = guide_colorbar(title = "Nuclear share %", title.position = "top", title.hjust = 0.5,
                                title.theme = element_text(colour = sub_col, size = 8)))
etx_save(p132, file.path(out_dir, "132_nuclear_heatmap_35yr.png"))


# ============================================================
# 133 — CO2 Intensity: Cleanest vs Dirtiest Grids
# ============================================================
cat("133 — CO₂ Intensity Diverging\n")

p133_clean <- latest %>%
  filter(!is.na(carbon_intensity_elec), carbon_intensity_elec > 0) %>%
  slice_min(carbon_intensity_elec, n = 10) %>% mutate(group = "Cleanest")
p133_dirty <- latest %>%
  filter(!is.na(carbon_intensity_elec), carbon_intensity_elec > 0) %>%
  slice_max(carbon_intensity_elec, n = 10) %>% mutate(group = "Dirtiest")

p133_data <- bind_rows(p133_clean, p133_dirty) %>%
  mutate(country = fct_reorder(country, carbon_intensity_elec))

p133 <- ggplot(p133_data, aes(x = carbon_intensity_elec, y = country, fill = group)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c("Cleanest" = accent1, "Dirtiest" = accent3)) +
  geom_text(aes(label = paste0(round(carbon_intensity_elec), " g")),
            hjust = -0.1, colour = text_col, family = "jetbrains", size = 2.8) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Carbon Intensity of Electricity: <span style='color:#00D4AA'>Clean</span> vs <span style='color:#FF6B6B'>Dirty</span>",
    subtitle = glue("gCO₂ per kWh of electricity generated · {latest_year}"),
    caption = etx_caption("Ember / OWID"), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 8.5, hjust = 1),
    panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.2),
    legend.position = c(0.8, 0.35)
  )
etx_save(p133, file.path(out_dir, "133_co2_intensity_electricity.png"))


# ============================================================
# 134 — Coal Phase-Out: Slope Chart 2010 → 2024
# ============================================================
cat("134 — Coal Phase-Out Slope\n")

slope_c <- c("Germany", "United Kingdom", "United States", "China", "India",
             "Poland", "Australia", "Japan", "South Korea", "South Africa")

p134_data <- energy %>%
  filter(country %in% slope_c, year %in% c(2010, latest_year),
         !is.na(coal_share_energy)) %>%
  select(country, year, coal_share_energy) %>%
  mutate(year_label = ifelse(year == 2010, "2010", as.character(latest_year)))

p134 <- ggplot(p134_data, aes(x = year, y = coal_share_energy, group = country)) +
  geom_line(aes(colour = country), linewidth = 1.1, alpha = 0.85) +
  geom_point(aes(colour = country), size = 3) +
  geom_text_repel(
    data = filter(p134_data, year == latest_year),
    aes(label = paste0(country, " ", round(coal_share_energy, 1), "%"), colour = country),
    hjust = -0.1, direction = "y", nudge_x = 0.5, size = 3, family = "inter",
    segment.colour = grid_col, segment.size = 0.3
  ) +
  geom_text_repel(
    data = filter(p134_data, year == 2010),
    aes(label = paste0(round(coal_share_energy, 1), "%"), colour = country),
    hjust = 1.2, direction = "y", nudge_x = -0.3, size = 2.8, family = "jetbrains",
    segment.colour = grid_col, segment.size = 0.3
  ) +
  scale_x_continuous(breaks = c(2010, latest_year), limits = c(2008, latest_year + 4),
                     labels = c("2010", as.character(latest_year))) +
  scale_colour_manual(values = setNames(
    c(accent6, accent1, accent2, accent3, accent4, "#9CA3AF", accent8, accent5, accent7, "#EF4444"),
    slope_c
  ), guide = "none") +
  labs(
    title = "The <span style='color:#6B7280'>Coal</span> Phase-Out: Who's Actually Declining?",
    subtitle = glue("Coal share of primary energy (%) · 2010 vs {latest_year} · 10 major coal users"),
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(axis.text.x = element_text(colour = text_col, size = 12, face = "bold"))
etx_save(p134, file.path(out_dir, "134_coal_phaseout_slope.png"))


# ============================================================
# 135 — Solar Explosion: Faceted Sparklines (12 countries)
# ============================================================
cat("135 — Solar Explosion Sparklines\n")

solar12 <- c("China", "United States", "India", "Japan", "Germany", "Australia",
             "Spain", "South Korea", "Brazil", "Chile", "United Kingdom", "Turkey")

p135_data <- energy %>%
  filter(country %in% solar12, year >= 2005, !is.na(solar_electricity)) %>%
  group_by(country) %>%
  mutate(peak = max(solar_electricity, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, -peak))

p135_labels <- p135_data %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

p135 <- ggplot(p135_data, aes(x = year, y = solar_electricity)) +
  geom_area(fill = accent4, alpha = 0.15) +
  geom_line(colour = accent4, linewidth = 0.8) +
  geom_point(data = p135_labels, colour = accent4, size = 1.5) +
  geom_text(data = p135_labels,
            aes(label = paste0(round(solar_electricity), " TWh")),
            vjust = -1, colour = text_col, family = "jetbrains", size = 2.3) +
  facet_wrap(~country, ncol = 4, scales = "free_y") +
  labs(
    title = "The <span style='color:#FFE66D'>Solar</span> Explosion: 12 Countries, One Story",
    subtitle = "Solar electricity generation (TWh) · 2005–present · Free y-axis per country",
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    strip.text = element_text(colour = text_col, size = 9, face = "bold",
                              family = "spacegrotesk", margin = margin(b = 4)),
    panel.spacing = unit(0.8, "lines")
  )
etx_save(p135, file.path(out_dir, "135_solar_explosion_faceted.png"), w = 12, h = 8)


# ============================================================
# 136 — Wind Power Leaders: Horizontal Dot Plot
# ============================================================
cat("136 — Wind Power Dot Plot\n")

p136_data <- latest %>%
  filter(!is.na(wind_share_elec)) %>%
  slice_max(wind_share_elec, n = 15) %>%
  mutate(country = fct_reorder(country, wind_share_elec))

p136 <- ggplot(p136_data, aes(x = wind_share_elec, y = country)) +
  geom_segment(aes(x = 0, xend = wind_share_elec, yend = country),
               colour = grid_col, linewidth = 0.6, linetype = "dotted") +
  geom_point(colour = accent1, size = 5) +
  geom_text(aes(label = paste0(round(wind_share_elec, 1), "%")),
            hjust = -0.5, colour = text_col, family = "jetbrains", size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Where Does the <span style='color:#00D4AA'>Wind</span> Blow Hardest?",
    subtitle = glue("Wind share of electricity generation (%) · Top 15 · {latest_year}"),
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(axis.text.y = element_text(colour = text_col, size = 9, hjust = 1))
etx_save(p136, file.path(out_dir, "136_wind_power_leaders.png"))


# ============================================================
# 137 — G7 Energy Mix: Stacked Proportional Bar
# ============================================================
cat("137 — G7 Energy Mix\n")

mix_vars <- c("coal_share_energy", "oil_share_energy", "gas_share_energy",
              "nuclear_share_energy", "hydro_share_energy", "solar_share_energy",
              "wind_share_energy", "biofuel_share_energy")
mix_labels <- c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Solar", "Wind", "Bioenergy")

p137_data <- latest %>%
  filter(country %in% g7) %>%
  select(country, all_of(mix_vars)) %>%
  pivot_longer(-country, names_to = "fuel", values_to = "share") %>%
  mutate(
    fuel = factor(mix_labels[match(fuel, mix_vars)], levels = rev(mix_labels)),
    country = fct_relevel(country, g7)
  )

p137 <- ggplot(p137_data, aes(x = country, y = share, fill = fuel)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = fuel_cols) +
  coord_flip() +
  labs(
    title = "G7 Energy DNA: What Powers the World's Richest Economies?",
    subtitle = glue("Share of primary energy by source (%) · {latest_year}"),
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 10, hjust = 1, face = "bold"),
    axis.text.x = element_text(colour = sub_col),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE,
         override.aes = list(size = 3),
         label.theme = element_text(colour = text_col, size = 8)))
etx_save(p137, file.path(out_dir, "137_g7_energy_mix.png"))


# ============================================================
# 138 — Fossil vs Renewable Share: Dumbbell (2000 → 2024)
# ============================================================
cat("138 — Fossil vs Renewable Dumbbell\n")

dumb_c <- c("Germany", "United Kingdom", "Spain", "Denmark", "China",
            "India", "Brazil", "United States", "Japan", "France",
            "Australia", "South Korea", "Italy", "Turkey", "Poland")

p138_data <- energy %>%
  filter(country %in% dumb_c, year %in% c(2000, latest_year),
         !is.na(renewables_share_energy)) %>%
  select(country, year, renewables_share_energy) %>%
  pivot_wider(names_from = year, values_from = renewables_share_energy, names_prefix = "y") %>%
  mutate(
    change = .data[[paste0("y", latest_year)]] - y2000,
    country = fct_reorder(country, .data[[paste0("y", latest_year)]])
  )

p138 <- ggplot(p138_data) +
  geom_segment(aes(x = y2000, xend = .data[[paste0("y", latest_year)]],
                   y = country, yend = country),
               colour = grid_col, linewidth = 1.5) +
  geom_point(aes(x = y2000, y = country), colour = accent3, size = 3.5) +
  geom_point(aes(x = .data[[paste0("y", latest_year)]], y = country), colour = accent1, size = 3.5) +
  geom_text(aes(x = .data[[paste0("y", latest_year)]], y = country,
                label = paste0("+", round(change, 1), " pp")),
            hjust = -0.3, colour = accent1, family = "jetbrains", size = 2.7) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.02, 0.18))) +
  labs(
    title = "Renewable Energy Gains: <span style='color:#FF6B6B'>2000</span> → <span style='color:#00D4AA'>{latest_year}</span>",
    subtitle = "Renewable share of primary energy (percentage points gained)",
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 8.5, hjust = 1),
    panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.2)
  )
etx_save(p138, file.path(out_dir, "138_renewable_dumbbell.png"))


# ============================================================
# 139 — Per Capita Electricity: The Global Divide
# ============================================================
cat("139 — Per Capita Electricity Divide\n")

p139_data <- latest %>%
  filter(!is.na(per_capita_electricity)) %>%
  mutate(
    country = fct_reorder(country, per_capita_electricity),
    tier = case_when(
      per_capita_electricity >= 10000 ~ "High (>10 MWh)",
      per_capita_electricity >= 3000  ~ "Medium (3-10 MWh)",
      TRUE ~ "Low (<3 MWh)"
    ),
    tier = factor(tier, levels = c("Low (<3 MWh)", "Medium (3-10 MWh)", "High (>10 MWh)"))
  )

top10 <- p139_data %>% slice_max(per_capita_electricity, n = 10)
bot10 <- p139_data %>% slice_min(per_capita_electricity, n = 10)
p139_sub <- bind_rows(top10, bot10)

p139 <- ggplot(p139_sub, aes(x = per_capita_electricity / 1000, y = country, fill = tier)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c("Low (<3 MWh)" = accent3, "Medium (3-10 MWh)" = accent8, "High (>10 MWh)" = accent1)) +
  geom_text(aes(label = paste0(round(per_capita_electricity / 1000, 1), " MWh")),
            hjust = -0.1, colour = text_col, family = "jetbrains", size = 2.8) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "The Electricity Divide: <span style='color:#00D4AA'>Richest</span> vs <span style='color:#FF6B6B'>Poorest</span>",
    subtitle = glue("Per capita electricity consumption (MWh/person) · Top 10 vs Bottom 10 · {latest_year}"),
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 8.5, hjust = 1),
    legend.position = c(0.75, 0.35)
  )
etx_save(p139, file.path(out_dir, "139_percapita_electricity_divide.png"))


# ============================================================
# 140 — Hydropower Giants: Top 15 Treemap-Style Bar
# ============================================================
cat("140 — Hydropower Giants\n")

p140_data <- latest %>%
  filter(!is.na(hydro_electricity)) %>%
  slice_max(hydro_electricity, n = 15) %>%
  mutate(
    country = fct_reorder(country, hydro_electricity),
    label = paste0(round(hydro_electricity), " TWh")
  )

p140 <- ggplot(p140_data, aes(x = hydro_electricity, y = country)) +
  geom_col(fill = "#3B82F6", width = 0.65, alpha = 0.85) +
  geom_text(aes(label = label), hjust = -0.1, colour = text_col,
            family = "jetbrains", size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "The <span style='color:#3B82F6'>Hydropower</span> Giants",
    subtitle = glue("Hydroelectric generation (TWh) · Top 15 · {latest_year}"),
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 9, hjust = 1),
    panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.2)
  )
etx_save(p140, file.path(out_dir, "140_hydropower_giants.png"))


# ============================================================
# 141 — Renewable Heatmap: G20 × Years
# ============================================================
cat("141 — Renewable Heatmap G20\n")

p141_data <- energy %>%
  filter(country %in% g20, year >= 2000, !is.na(renewables_share_energy)) %>%
  mutate(country = factor(country, levels = rev(g20)))

p141 <- ggplot(p141_data, aes(x = year, y = country, fill = renewables_share_energy)) +
  geom_tile(colour = bg_col, linewidth = 0.3) +
  scale_fill_gradient(low = panel_col, high = accent1,
                      labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), expand = c(0, 0)) +
  labs(
    title = "G20 <span style='color:#00D4AA'>Renewable</span> Energy Progress: A Quarter Century",
    subtitle = "Renewable share of primary energy (%) · 2000–present",
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 8, hjust = 1),
    axis.text.x = element_text(colour = sub_col),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) +
  guides(fill = guide_colorbar(title = "Renewable %", title.position = "top", title.hjust = 0.5,
                                title.theme = element_text(colour = sub_col, size = 8)))
etx_save(p141, file.path(out_dir, "141_renewable_heatmap_g20.png"), w = 13, h = 8)


# ============================================================
# 142 — Bioenergy: The Overlooked Renewable (Top 15)
# ============================================================
cat("142 — Bioenergy Share\n")

p142_data <- latest %>%
  filter(!is.na(biofuel_share_energy)) %>%
  slice_max(biofuel_share_energy, n = 15) %>%
  mutate(country = fct_reorder(country, biofuel_share_energy))

p142 <- ggplot(p142_data, aes(x = biofuel_share_energy, y = country)) +
  geom_segment(aes(x = 0, xend = biofuel_share_energy, yend = country),
               colour = "#22C55E", linewidth = 1, alpha = 0.4) +
  geom_point(colour = "#22C55E", size = 4.5) +
  geom_text(aes(label = paste0(round(biofuel_share_energy, 1), "%")),
            hjust = -0.5, colour = text_col, family = "jetbrains", size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "<span style='color:#22C55E'>Bioenergy</span>: The Overlooked Renewable",
    subtitle = glue("Bioenergy share of primary energy (%) · Top 15 · {latest_year}"),
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(axis.text.y = element_text(colour = text_col, size = 9, hjust = 1))
etx_save(p142, file.path(out_dir, "142_bioenergy_share.png"))


# ============================================================
# 143 — Carbon Intensity Improvement: 2010 → 2024
# ============================================================
cat("143 — Carbon Intensity Improvement\n")

ci_c <- g20
p143_data <- energy %>%
  filter(country %in% ci_c, year %in% c(2010, latest_year),
         !is.na(carbon_intensity_elec)) %>%
  select(country, year, carbon_intensity_elec) %>%
  pivot_wider(names_from = year, values_from = carbon_intensity_elec, names_prefix = "y") %>%
  mutate(
    change = .data[[paste0("y", latest_year)]] - y2010,
    pct_change = change / y2010 * 100,
    country = fct_reorder(country, pct_change),
    direction = ifelse(pct_change < 0, "Improved", "Worsened")
  ) %>%
  filter(!is.na(pct_change))

p143 <- ggplot(p143_data, aes(x = pct_change, y = country, fill = direction)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("Improved" = accent1, "Worsened" = accent3), guide = "none") +
  geom_text(aes(label = paste0(ifelse(pct_change > 0, "+", ""), round(pct_change), "%"),
                hjust = ifelse(pct_change < 0, 1.1, -0.1)),
            colour = text_col, family = "jetbrains", size = 2.8) +
  geom_vline(xintercept = 0, colour = text_col, linewidth = 0.4) +
  labs(
    title = "Who <span style='color:#00D4AA'>Cleaned Up</span> Their Grid? Carbon Intensity Change",
    subtitle = glue("% change in gCO₂/kWh of electricity · 2010 → {latest_year} · G20"),
    caption = etx_caption("Ember / OWID"), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(axis.text.y = element_text(colour = text_col, size = 8.5, hjust = 1))
etx_save(p143, file.path(out_dir, "143_carbon_intensity_change.png"))


# ============================================================
# 144 — Energy Trilemma: GDP vs Renewables vs Emissions
# ============================================================
cat("144 — Energy Trilemma Scatter\n")

gdp_year <- energy %>% filter(!is.na(gdp)) %>% pull(year) %>% max()
latest_gdp <- energy %>% filter(year == gdp_year)

p144_data <- latest_gdp %>%
  filter(!is.na(renewables_share_energy), !is.na(gdp), !is.na(greenhouse_gas_emissions),
         !is.na(population)) %>%
  mutate(
    gdp_pc = gdp / population / 1000,
    ghg_pc = greenhouse_gas_emissions / population * 1e6
  ) %>%
  filter(gdp_pc > 0, ghg_pc > 0)

p144 <- ggplot(p144_data, aes(x = renewables_share_energy, y = ghg_pc)) +
  geom_point(aes(size = population / 1e6, colour = continent), alpha = 0.75) +
  geom_text_repel(aes(label = iso3, colour = continent),
                  size = 2.5, family = "jetbrains", segment.colour = grid_col,
                  max.overlaps = 15) +
  scale_colour_manual(values = continent_cols) +
  scale_size_continuous(range = c(2, 15), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "The Energy Trilemma: Renewables vs Per Capita Emissions",
    subtitle = glue("Bubble size = population · {gdp_year}"),
    caption = etx_caption(), x = "Renewable share of energy (%)", y = "GHG per capita (tCO₂eq)"
  ) +
  theme_etx() +
  theme(
    axis.text = element_text(colour = sub_col),
    panel.grid.major = element_line(colour = grid_col, linewidth = 0.2),
    legend.position = c(0.85, 0.85)
  )
etx_save(p144, file.path(out_dir, "144_energy_trilemma.png"))


# ============================================================
# 145 — Oil Producers vs Consumers: Who's Net Positive?
# ============================================================
cat("145 — Oil Net Balance\n")

p145_data <- latest %>%
  filter(!is.na(oil_production), !is.na(oil_consumption)) %>%
  mutate(
    net = oil_production - oil_consumption,
    country = fct_reorder(country, net),
    status = ifelse(net > 0, "Net Exporter", "Net Importer")
  ) %>%
  slice_max(abs(net), n = 20)

p145 <- ggplot(p145_data, aes(x = net, y = fct_reorder(country, net), fill = status)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("Net Exporter" = accent1, "Net Importer" = accent3)) +
  geom_vline(xintercept = 0, colour = text_col, linewidth = 0.4) +
  geom_text(aes(label = round(net), hjust = ifelse(net > 0, -0.2, 1.2)),
            colour = text_col, family = "jetbrains", size = 2.7) +
  labs(
    title = "Oil: <span style='color:#00D4AA'>Exporters</span> vs <span style='color:#FF6B6B'>Importers</span>",
    subtitle = glue("Net oil balance (production − consumption, TWh) · Top 20 by magnitude · {latest_year}"),
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 8, hjust = 1),
    legend.position = c(0.85, 0.2)
  )
etx_save(p145, file.path(out_dir, "145_oil_net_balance.png"))


# ============================================================
# 146 — Electricity Growth: Decade Comparison (Area)
# ============================================================
cat("146 — Global Electricity by Source\n")

world_data <- raw %>%
  filter(country == "World", year >= 1990) %>%
  select(year, coal_electricity, gas_electricity, oil_electricity,
         nuclear_electricity, hydro_electricity, solar_electricity,
         wind_electricity, biofuel_electricity) %>%
  pivot_longer(-year, names_to = "source", values_to = "twh") %>%
  mutate(
    source = str_remove(source, "_electricity"),
    source = str_to_title(source),
    source = recode(source, "Biofuel" = "Bioenergy"),
    source = factor(source, levels = c("Coal", "Gas", "Oil", "Nuclear",
                                        "Hydro", "Wind", "Solar", "Bioenergy"))
  ) %>%
  filter(!is.na(twh))

p146 <- ggplot(world_data, aes(x = year, y = twh, fill = source)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = fuel_cols) +
  scale_y_continuous(labels = label_comma(suffix = " TWh")) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  labs(
    title = "How the World Generates Electricity: 1990–Present",
    subtitle = "Global electricity generation by source (TWh)",
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text = element_text(colour = sub_col),
    panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.2)
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = FALSE,
         label.theme = element_text(colour = text_col, size = 8)))
etx_save(p146, file.path(out_dir, "146_global_electricity_stack.png"))


# ============================================================
# 147 — Clean Electricity Share: Bump Chart (Top 10)
# ============================================================
cat("147 — Clean Electricity Bump Chart\n")

clean_c <- c("Norway", "Brazil", "Sweden", "Switzerland", "France",
             "Canada", "New Zealand", "Denmark", "Finland", "Austria")

p147_data <- energy %>%
  filter(country %in% clean_c, year >= 2000, !is.na(low_carbon_share_elec)) %>%
  group_by(year) %>%
  mutate(rank = rank(-low_carbon_share_elec, ties.method = "first")) %>%
  ungroup()

p147_labels <- p147_data %>% filter(year == max(year))

p147 <- ggplot(p147_data, aes(x = year, y = rank, colour = country, group = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = p147_labels, size = 3) +
  geom_text(data = p147_labels, aes(label = country),
            hjust = -0.15, family = "inter", size = 3) +
  scale_y_reverse(breaks = 1:10) +
  scale_x_continuous(limits = c(2000, latest_year + 4), breaks = seq(2000, 2025, 5)) +
  scale_colour_manual(values = setNames(
    c(accent6, accent1, accent5, accent2, accent5, accent8, accent4, accent1, accent7, accent3),
    clean_c
  ), guide = "none") +
  labs(
    title = "Clean Electricity Rankings: Who Leads the Race?",
    subtitle = "Rank by low-carbon share of electricity · Top 10 cleanest grids · 2000–present",
    caption = etx_caption(), x = NULL, y = "Rank"
  ) +
  theme_etx() +
  theme(
    axis.text = element_text(colour = sub_col),
    panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.15)
  )
etx_save(p147, file.path(out_dir, "147_clean_elec_bump.png"))


# ============================================================
# 148 — Fossil Fuel Decline: Who's Actually Reducing?
# ============================================================
cat("148 — Fossil Fuel Decline\n")

p148_data <- energy %>%
  filter(country %in% g20, year %in% c(2010, latest_year),
         !is.na(fossil_share_energy)) %>%
  select(country, year, fossil_share_energy) %>%
  pivot_wider(names_from = year, values_from = fossil_share_energy, names_prefix = "y") %>%
  mutate(
    change = .data[[paste0("y", latest_year)]] - y2010,
    country = fct_reorder(country, change),
    direction = ifelse(change < 0, "Decreased", "Increased")
  ) %>%
  filter(!is.na(change))

p148 <- ggplot(p148_data, aes(x = change, y = country, fill = direction)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("Decreased" = accent1, "Increased" = accent3), guide = "none") +
  geom_vline(xintercept = 0, colour = text_col, linewidth = 0.4) +
  geom_text(aes(label = paste0(ifelse(change > 0, "+", ""), round(change, 1), " pp"),
                hjust = ifelse(change < 0, 1.1, -0.1)),
            colour = text_col, family = "jetbrains", size = 2.8) +
  labs(
    title = "Fossil Fuel Decline: Who's Actually <span style='color:#00D4AA'>Reducing</span>?",
    subtitle = glue("Change in fossil share of primary energy (pp) · 2010 → {latest_year} · G20"),
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(axis.text.y = element_text(colour = text_col, size = 8.5, hjust = 1))
etx_save(p148, file.path(out_dir, "148_fossil_decline_g20.png"))


# ============================================================
# 149 — Low-Carbon Share: Connected Dot (2000 → 2024)
# ============================================================
cat("149 — Low-Carbon Connected Dot\n")

lc_c <- c("Denmark", "United Kingdom", "Germany", "Spain", "China",
           "India", "Turkey", "Australia", "South Korea", "Poland")

p149_data <- energy %>%
  filter(country %in% lc_c, year %in% c(2000, latest_year),
         !is.na(low_carbon_share_elec)) %>%
  select(country, year, low_carbon_share_elec) %>%
  pivot_wider(names_from = year, values_from = low_carbon_share_elec, names_prefix = "y") %>%
  mutate(
    change = .data[[paste0("y", latest_year)]] - y2000,
    country = fct_reorder(country, .data[[paste0("y", latest_year)]])
  )

p149 <- ggplot(p149_data) +
  geom_segment(aes(x = y2000, xend = .data[[paste0("y", latest_year)]],
                   y = country, yend = country), colour = sub_col, linewidth = 1.2) +
  geom_point(aes(x = y2000, y = country), colour = accent3, size = 4, shape = 16) +
  geom_point(aes(x = .data[[paste0("y", latest_year)]], y = country), colour = accent1, size = 4, shape = 16) +
  geom_text(aes(x = .data[[paste0("y", latest_year)]], y = country,
                label = paste0(round(.data[[paste0("y", latest_year)]]), "%")),
            hjust = -0.4, colour = accent1, family = "jetbrains", size = 3) +
  geom_text(aes(x = y2000, y = country, label = paste0(round(y2000), "%")),
            hjust = 1.4, colour = accent3, family = "jetbrains", size = 3) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.1, 0.12))) +
  labs(
    title = "Low-Carbon Electricity: <span style='color:#FF6B6B'>2000</span> → <span style='color:#00D4AA'>{latest_year}</span>",
    subtitle = "Low-carbon share of electricity (%) · Selected transition economies",
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 9, hjust = 1),
    panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.2)
  )
etx_save(p149, file.path(out_dir, "149_lowcarbon_connected_dot.png"))


# ============================================================
# 150 — Energy Intensity: GDP per Unit of Energy
# ============================================================
cat("150 — Energy Intensity\n")

p150_data <- latest_gdp %>%
  filter(!is.na(energy_per_gdp), energy_per_gdp > 0) %>%
  slice_max(energy_per_gdp, n = 10, with_ties = FALSE) %>%
  bind_rows(
    latest_gdp %>% filter(!is.na(energy_per_gdp), energy_per_gdp > 0) %>%
      slice_min(energy_per_gdp, n = 10, with_ties = FALSE)
  ) %>%
  mutate(
    country = fct_reorder(country, energy_per_gdp),
    group = ifelse(energy_per_gdp > median(energy_per_gdp), "Most Intensive", "Most Efficient")
  )

p150 <- ggplot(p150_data, aes(x = energy_per_gdp, y = country, fill = group)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c("Most Intensive" = accent3, "Most Efficient" = accent6)) +
  geom_text(aes(label = round(energy_per_gdp, 2)),
            hjust = -0.2, colour = text_col, family = "jetbrains", size = 2.8) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Energy Intensity: <span style='color:#38BDF8'>Efficient</span> vs <span style='color:#FF6B6B'>Wasteful</span> Economies",
    subtitle = glue("Energy per unit of GDP (kWh/$) · {gdp_year}"),
    caption = etx_caption(), x = NULL, y = NULL
  ) +
  theme_etx() +
  theme(
    axis.text.y = element_text(colour = text_col, size = 8.5, hjust = 1),
    legend.position = c(0.75, 0.35)
  )
etx_save(p150, file.path(out_dir, "150_energy_intensity.png"))

cat("\n✅ All 20 infographics generated!\n")
