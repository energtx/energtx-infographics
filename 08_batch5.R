#!/usr/bin/env Rscript
# energtx batch 5 — charts 171-175 (2026-08-08)
# Style follows 07_spec_generate.R (dark #0a1628, Inter, OWID data).
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(forcats)
  library(scales); library(ragg); library(showtext); library(sysfonts); library(ggtext)
})

root <- "/home/fbartuyurdacan/energtx-infographics"
setwd(root)
sysfonts::font_add_google("Inter", "Inter")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 150)

pal <- list(bg = "#0a1628", grid = "#16263f", text_main = "#e8f1ff", text_sub = "#8aa0bf",
            teal = "#34d7c2", coral = "#f5617a", amber = "#fbbf24", violet = "#c084fc",
            green = "#4ade80", orange = "#fb923c", blue = "#60a5fa")

energtx_theme <- function(base = 34) {
  theme_minimal(base_family = "Inter", base_size = base) +
    theme(
      plot.background = element_rect(fill = pal$bg, color = NA),
      panel.background = element_rect(fill = pal$bg, color = NA),
      panel.grid.major = element_line(color = pal$grid, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.title = element_markdown(color = pal$text_main, face = "bold",
                                    size = base * 2.1, margin = margin(b = 8), lineheight = 1.1),
      plot.subtitle = element_text(color = pal$text_sub, size = base * 1.05,
                                   margin = margin(b = 22), lineheight = 1.2),
      plot.caption = element_markdown(color = pal$text_sub, size = base * 0.78,
                                      hjust = 0, margin = margin(t = 18)),
      plot.title.position = "plot", plot.caption.position = "plot",
      axis.text = element_text(color = pal$text_sub, size = base * 0.92),
      axis.title = element_text(color = pal$text_sub, size = base * 0.95),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.margin = margin(34, 44, 26, 44)
    )
}
caption_txt <- "Source: Our World in Data · <span style='color:#34d7c2;'>energtx.com</span> · @energtx.bsky.social"
save_png <- function(p, file) {
  ggsave(file.path(root, "png", file), p, device = ragg::agg_png,
         width = 3600, height = 2025, units = "px", dpi = 150)
  cat("yazildi:", file, "\n")
}

raw <- readr::read_csv("owid-energy-data.csv", show_col_types = FALSE)
owid  <- raw |> filter(!is.na(iso_code), nchar(iso_code) == 3)
world <- raw |> filter(country == "World")

# ---- 171: The electricity gap — Africa vs the rich world ----
afr <- c("Nigeria", "Kenya", "Morocco", "Egypt", "Algeria", "South Africa")
ref <- c("Germany", "United States")
d171 <- owid |> filter(country %in% c(afr, ref), year == 2023) |>
  select(country, v = per_capita_electricity) |>
  bind_rows(world |> filter(year == 2023) |>
              transmute(country = "World average", v = per_capita_electricity)) |>
  filter(!is.na(v)) |>
  mutate(grp = case_when(country %in% afr ~ "afr", country == "World average" ~ "wrl", TRUE ~ "ref"),
         country = fct_reorder(country, v))
p171 <- ggplot(d171, aes(v, country, fill = grp)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = comma(round(v)), color = grp), hjust = -0.15, size = 10.5, family = "Inter") +
  scale_fill_manual(values = c(afr = pal$teal, ref = "#3b4d68", wrl = pal$coral)) +
  scale_color_manual(values = c(afr = pal$teal, ref = pal$text_sub, wrl = pal$coral)) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.14))) +
  labs(title = "The electricity gap",
       subtitle = "Electricity use per person (kWh), 2023 — six African economies vs world, Germany, US",
       x = NULL, y = NULL, caption = caption_txt) +
  energtx_theme()
save_png(p171, "171_africa_electricity_gap.png")

# ---- 172: Vietnam's solar boom ----
d172 <- owid |> filter(country == "Vietnam", year >= 2012, !is.na(solar_share_elec)) |>
  select(year, v = solar_share_elec)
lab172 <- d172 |> slice_max(year)
p172 <- ggplot(d172, aes(year, v)) +
  geom_area(fill = pal$amber, alpha = 0.25) +
  geom_line(color = pal$amber, linewidth = 2) +
  geom_point(data = lab172, color = pal$amber, size = 7) +
  geom_text(data = lab172, aes(label = paste0(round(v, 1), "%")),
            color = pal$amber, vjust = -1.1, size = 12, family = "Inter", fontface = "bold") +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  scale_x_continuous(breaks = seq(2012, 2024, 2)) +
  labs(title = "Vietnam's solar explosion",
       subtitle = "Solar share of electricity generation - from almost nothing in 2018 to double digits two years later",
       x = NULL, y = NULL, caption = caption_txt) +
  energtx_theme()
save_png(p172, "172_vietnam_solar_boom.png")

# ---- 173: Wind + solar overtake nuclear (world) ----
d173 <- world |> filter(year >= 1990) |>
  transmute(year, `Wind + solar` = wind_electricity + solar_electricity,
            Nuclear = nuclear_electricity) |>
  pivot_longer(-year) |> filter(!is.na(value))
lab173 <- d173 |> group_by(name) |> slice_max(year) |>
  mutate(vj = if_else(name == "Wind + solar", 1.8, -1.0))
p173 <- ggplot(d173, aes(year, value, color = name)) +
  geom_line(linewidth = 2) +
  geom_text(data = lab173, aes(label = name, vjust = vj), hjust = 1,
            size = 11, family = "Inter", fontface = "bold") +
  scale_color_manual(values = c(`Wind + solar` = pal$teal, Nuclear = pal$violet)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.12))) +
  labs(title = "The crossover: wind + solar pass nuclear",
       subtitle = "Global electricity generation (TWh), 1990-2024 - around 2021 the lines crossed for good",
       x = NULL, y = "TWh", caption = caption_txt) +
  energtx_theme()
save_png(p173, "173_wind_solar_vs_nuclear.png")

# ---- 174: Germany's nuclear exit ----
d174 <- owid |> filter(country == "Germany", year >= 2000) |>
  transmute(year, Nuclear = nuclear_electricity,
            `Wind + solar` = wind_electricity + solar_electricity) |>
  pivot_longer(-year) |> filter(!is.na(value))
lab174 <- d174 |> group_by(name) |> slice_max(year) |>
  mutate(vj = if_else(name == "Wind + solar", 1.8, -1.0))
p174 <- ggplot(d174, aes(year, value, color = name)) +
  geom_line(linewidth = 2) +
  geom_text(data = lab174, aes(label = name, vjust = vj), hjust = 1,
            size = 11, family = "Inter", fontface = "bold") +
  scale_color_manual(values = c(`Wind + solar` = pal$teal, Nuclear = pal$violet)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.12))) +
  labs(title = "Energiewende: atoms out, turbines in",
       subtitle = "Germany, electricity generation (TWh) - the last reactor closed in April 2023",
       x = NULL, y = "TWh", caption = caption_txt) +
  energtx_theme()
save_png(p174, "174_germany_nuclear_exit.png")

# ---- 175: South Asia's rising demand ----
d175 <- owid |> filter(country %in% c("India", "Pakistan", "Bangladesh"), year >= 2000,
                       !is.na(per_capita_electricity)) |>
  select(country, year, v = per_capita_electricity)
lab175 <- d175 |> group_by(country) |> slice_max(year) |>
  mutate(vj = if_else(country == "India", 1.8, -1.0))
p175 <- ggplot(d175, aes(year, v, color = country)) +
  geom_line(linewidth = 2) +
  geom_text(data = lab175, aes(label = country, vjust = vj), hjust = 1,
            size = 11, family = "Inter", fontface = "bold") +
  scale_color_manual(values = c(India = pal$orange, Pakistan = pal$blue, Bangladesh = pal$green)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.12))) +
  labs(title = "South Asia plugs in",
       subtitle = "Electricity use per person (kWh), 2000-2024 - 1.9 billion people, demand still climbing fast",
       x = NULL, y = "kWh per person", caption = caption_txt) +
  energtx_theme()
save_png(p175, "175_south_asia_rising.png")

cat("BATCH 5 TAMAM\n")
