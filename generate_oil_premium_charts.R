# =============================================================================
# Premium Oil Crisis Infographics for energtx Bluesky
# Higher quality: gradients, better typography, diverse chart types
# =============================================================================

library(ggplot2)
library(ggtext)
library(showtext)
library(jsonlite)
library(dplyr)
library(tidyr)
library(glue)
library(scales)

# ---------- Load Data ----------
raw <- fromJSON("src/app/datasets/energy_data_combined.json")
energy <- as.data.frame(raw) %>%
  mutate(year = as.integer(year), value = as.numeric(value)) %>%
  filter(!is.na(value), value > 0)

# ---------- Fonts ----------
font_add_google("Inter", "inter")
font_add_google("JetBrains Mono", "jetmono")
showtext_auto()
showtext_opts(dpi = 300)

main_font <- "inter"
mono_font <- "jetmono"

# ---------- Output ----------
dir.create("public/bluesky", showWarnings = FALSE, recursive = TRUE)

# ---------- Premium Color Palette ----------
bg        <- "#0B1120"
panel     <- "#0F172A"
grid_col  <- "#1E293B"
grid_lite <- "#162032"
white     <- "#F8FAFC"
slate200  <- "#E2E8F0"
slate300  <- "#CBD5E1"
slate400  <- "#94A3B8"
slate500  <- "#64748B"
slate600  <- "#475569"

# Oil theme colors
oil_gold    <- "#F59E0B"
oil_amber   <- "#D97706"
oil_orange  <- "#EA580C"
oil_red     <- "#DC2626"
oil_rose    <- "#E11D48"
emerald     <- "#10B981"
cyan        <- "#06B6D4"
sky         <- "#0EA5E9"
violet      <- "#8B5CF6"
indigo      <- "#6366F1"

# ---------- Premium theme ----------
premium_theme <- function(accent = oil_gold) {
  theme_minimal(base_family = main_font, base_size = 11) +
  theme(
    plot.background    = element_rect(fill = bg, color = NA),
    panel.background   = element_rect(fill = bg, color = NA),
    plot.title         = element_markdown(color = white, size = 16, face = "bold",
                                          margin = margin(b = 4), lineheight = 1.15),
    plot.subtitle      = element_text(color = slate400, size = 9.5,
                                       margin = margin(b = 16)),
    plot.caption       = element_markdown(color = slate500, size = 7.5, hjust = 1,
                                           margin = margin(t = 14)),
    plot.margin        = margin(28, 28, 18, 18),
    axis.text.y        = element_text(color = slate300, size = 9, hjust = 1,
                                       family = main_font),
    axis.text.x        = element_text(color = slate500, size = 8),
    axis.title         = element_blank(),
    panel.grid.major.x = element_line(color = grid_col, linewidth = 0.25),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none"
  )
}

save_chart <- function(filename, plot) {
  ggsave(filename, plot, width = 10, height = 5.625, dpi = 300,
         bg = bg, units = "in")
  cat(glue("  Saved: {filename}\n\n"))
}

caption_text <- "<span style='color:#F59E0B'>energtx.com/subscribe</span>
<span style='color:#475569'>|</span>
<span style='color:#64748B'>@energtx.bsky.social</span>"

# =============================================================================
# 1: Oil Production vs Consumption — Dumbbell Chart (Net Exporters/Importers)
# =============================================================================
cat("1/6 Oil Net Exporters vs Importers (Dumbbell)...\n")

d_prod <- energy %>%
  filter(indicator_name == "Crude oil production — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>% filter(year == max(year)) %>% ungroup() %>%
  select(country_name, production = value)

d_dem <- energy %>%
  filter(indicator_name == "Total oil products demand — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>% filter(year == max(year)) %>% ungroup() %>%
  select(country_name, demand = value)

d_net <- inner_join(d_prod, d_dem, by = "country_name") %>%
  mutate(net = production - demand,
         type = ifelse(net >= 0, "Net Exporter", "Net Importer")) %>%
  arrange(net) %>%
  tail(20) %>%
  bind_rows(
    inner_join(d_prod, d_dem, by = "country_name") %>%
      mutate(net = production - demand, type = ifelse(net >= 0, "Net Exporter", "Net Importer")) %>%
      arrange(net) %>% head(5)
  ) %>%
  distinct(country_name, .keep_all = TRUE) %>%
  arrange(net) %>%
  mutate(country_name = factor(country_name, levels = country_name))

p1 <- ggplot(d_net) +
  geom_segment(aes(x = country_name, xend = country_name,
                   y = demand, yend = production),
               color = slate600, linewidth = 1.2) +
  geom_point(aes(x = country_name, y = production), color = emerald, size = 3.5) +
  geom_point(aes(x = country_name, y = demand), color = oil_red, size = 3.5) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Oil: Production vs Demand by Country",
    subtitle = "Green = production (KB/D)  |  Red = demand (KB/D)  |  Gap = net export or import  |  Source: JODI",
    caption = caption_text
  ) +
  premium_theme() +
  theme(
    panel.grid.major.x = element_line(color = grid_col, linewidth = 0.2),
    panel.grid.major.y = element_line(color = grid_lite, linewidth = 0.15)
  )

save_chart("public/bluesky/bsky-oil-net-balance.png", p1)

# =============================================================================
# 2: Gasoline vs Diesel Demand — Paired Bar Chart
# =============================================================================
cat("2/6 Gasoline vs Diesel Demand...\n")

d_gas <- energy %>%
  filter(indicator_name == "Gasoline demand — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>% filter(year == max(year)) %>% ungroup() %>%
  select(country_name, gasoline = value)

d_dies <- energy %>%
  filter(indicator_name == "Diesel demand — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>% filter(year == max(year)) %>% ungroup() %>%
  select(country_name, diesel = value)

d_fuel <- inner_join(d_gas, d_dies, by = "country_name") %>%
  mutate(total = gasoline + diesel) %>%
  arrange(desc(total)) %>% head(12) %>%
  pivot_longer(cols = c(gasoline, diesel), names_to = "fuel", values_to = "value") %>%
  mutate(
    country_name = factor(country_name, levels = rev(unique(country_name))),
    fuel = factor(fuel, levels = c("diesel", "gasoline"))
  )

p2 <- ggplot(d_fuel, aes(x = country_name, y = value, fill = fuel)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(value)),
            position = position_dodge(width = 0.7),
            hjust = -0.15, size = 2.8, color = slate300, family = main_font) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("gasoline" = oil_amber, "diesel" = sky),
                    labels = c("Gasoline", "Diesel")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "<span style='color:#D97706'>Gasoline</span> vs <span style='color:#0EA5E9'>Diesel</span> Demand — Top 12 Countries",
    subtitle = "Thousand barrels per day (KB/D)  |  Source: JODI Oil World Database",
    caption = caption_text
  ) +
  premium_theme() +
  theme(legend.position = "none")

save_chart("public/bluesky/bsky-gasoline-vs-diesel.png", p2)

# =============================================================================
# 3: Crude Oil Stocks — Top Holders (Lollipop Chart)
# =============================================================================
cat("3/6 Crude Oil Stocks (Lollipop)...\n")

d_stocks <- energy %>%
  filter(indicator_name == "Crude oil closing stocks — JODI (thousand barrels)") %>%
  group_by(country_name) %>% filter(year == max(year)) %>% ungroup() %>%
  arrange(desc(value)) %>% head(15) %>%
  mutate(
    country_name = factor(country_name, levels = rev(country_name)),
    value_m = value / 1000,  # million barrels
    label = paste0(round(value_m), "M bbl")
  )

p3 <- ggplot(d_stocks, aes(x = country_name, y = value_m)) +
  geom_segment(aes(xend = country_name, y = 0, yend = value_m),
               color = oil_gold, linewidth = 1.3) +
  geom_point(color = oil_gold, size = 4.5) +
  geom_point(color = bg, size = 2) +
  geom_text(aes(label = label), hjust = -0.2, size = 3, color = slate300,
            family = mono_font) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22)),
                     labels = function(x) paste0(x, "M")) +
  labs(
    title = "Crude Oil Stocks by Country",
    subtitle = "Million barrels (closing stocks)  |  Strategic + commercial reserves  |  Source: JODI",
    caption = caption_text
  ) +
  premium_theme()

save_chart("public/bluesky/bsky-crude-oil-stocks.png", p3)

# =============================================================================
# 4: Refinery Output — Top 15 (Gradient Bar)
# =============================================================================
cat("4/6 Refinery Output...\n")

d_ref <- energy %>%
  filter(indicator_name == "Refinery output — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>% filter(year == max(year)) %>% ungroup() %>%
  arrange(desc(value)) %>% head(15) %>%
  mutate(
    country_name = factor(country_name, levels = rev(country_name)),
    label = format(round(value), big.mark = ","),
    rank = row_number()
  )

p4 <- ggplot(d_ref, aes(x = country_name, y = value, fill = value)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = label), hjust = -0.1, size = 3, color = slate300,
            family = mono_font) +
  scale_fill_gradient(low = indigo, high = oil_rose) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Top 15 Oil Refinery Output Countries",
    subtitle = "Thousand barrels per day (KB/D)  |  Refinery output = downstream capacity  |  Source: JODI",
    caption = caption_text
  ) +
  premium_theme()

save_chart("public/bluesky/bsky-refinery-output.png", p4)

# =============================================================================
# 5: Oil Import Trend — Top 5 Importers Over Time
# =============================================================================
cat("5/6 Oil Import Trend — Top 5...\n")

top5_imp <- energy %>%
  filter(indicator_name == "Crude oil imports — JODI (thousand barrels/day)") %>%
  group_by(country_name) %>% filter(year == max(year)) %>% ungroup() %>%
  arrange(desc(value)) %>% head(5) %>% pull(country_name)

d_imp_trend <- energy %>%
  filter(
    indicator_name == "Crude oil imports — JODI (thousand barrels/day)",
    country_name %in% top5_imp,
    year >= 2005
  )

imp_colors <- c(oil_red, oil_amber, emerald, sky, violet)
names(imp_colors) <- top5_imp

p5 <- ggplot(d_imp_trend, aes(x = year, y = value, color = country_name)) +
  geom_line(linewidth = 1.3, alpha = 0.9) +
  geom_point(
    data = d_imp_trend %>% group_by(country_name) %>% filter(year == max(year)),
    size = 3.5
  ) +
  geom_text(
    data = d_imp_trend %>% group_by(country_name) %>% filter(year == max(year)),
    aes(label = paste0(country_name, "\n", format(round(value), big.mark = ","))),
    hjust = -0.1, size = 2.8, family = main_font, lineheight = 0.9,
    show.legend = FALSE
  ) +
  scale_color_manual(values = imp_colors) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.22)),
                     breaks = seq(2005, 2025, 5)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Crude Oil Import Trend — Top 5 Importers",
    subtitle = "Thousand barrels per day (2005-present)  |  Source: JODI  |  All exposed to Hormuz",
    caption = caption_text
  ) +
  premium_theme() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = grid_col, linewidth = 0.2)
  )

save_chart("public/bluesky/bsky-oil-import-trend.png", p5)

# =============================================================================
# 6: Oil Products Breakdown — Stacked Bar for Top 8
# =============================================================================
cat("6/6 Oil Products Breakdown (Stacked)...\n")

products <- c(
  "Gasoline demand — JODI (thousand barrels/day)",
  "Diesel demand — JODI (thousand barrels/day)",
  "Kerosene demand — JODI (thousand barrels/day)",
  "LPG demand — JODI (thousand barrels/day)",
  "Fuel oil demand — JODI (thousand barrels/day)"
)

short_names <- c("Gasoline", "Diesel", "Kerosene", "LPG", "Fuel Oil")
names(short_names) <- products

d_stack <- energy %>%
  filter(indicator_name %in% products) %>%
  group_by(country_name, indicator_name) %>%
  filter(year == max(year)) %>% ungroup() %>%
  mutate(product = short_names[indicator_name])

# Top 10 by total
top10_total <- d_stack %>%
  group_by(country_name) %>%
  summarise(total = sum(value), .groups = "drop") %>%
  arrange(desc(total)) %>% head(10) %>% pull(country_name)

d_stack <- d_stack %>%
  filter(country_name %in% top10_total) %>%
  mutate(
    country_name = factor(country_name, levels = rev(top10_total)),
    product = factor(product, levels = rev(c("Gasoline", "Diesel", "Kerosene", "LPG", "Fuel Oil")))
  )

prod_colors <- c(
  "Gasoline" = oil_amber,
  "Diesel" = sky,
  "Kerosene" = violet,
  "LPG" = emerald,
  "Fuel Oil" = oil_red
)

p6 <- ggplot(d_stack, aes(x = country_name, y = value, fill = product)) +
  geom_col(width = 0.65, position = "stack") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = prod_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = comma) +
  labs(
    title = "Oil Products Demand Breakdown — Top 10 Countries",
    subtitle = "KB/D by product type  |  Source: JODI  |  Hormuz disrupts ALL product flows",
    caption = caption_text
  ) +
  premium_theme() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(color = slate300, size = 8),
    legend.title = element_blank(),
    legend.key.size = unit(0.4, "cm"),
    legend.margin = margin(b = -5),
    legend.spacing.x = unit(0.3, "cm")
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1))

save_chart("public/bluesky/bsky-oil-products-breakdown.png", p6)

cat("\n=== All 6 premium charts generated! ===\n")
