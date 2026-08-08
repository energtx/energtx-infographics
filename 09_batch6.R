#!/usr/bin/env Rscript
# energtx batch 6 — charts 176-185 (2026-08-08)
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(forcats)
  library(scales); library(ragg); library(ggrepel); library(showtext); library(sysfonts); library(ggtext)
})
root <- "/home/fbartuyurdacan/energtx-infographics"; setwd(root)
sysfonts::font_add_google("Inter", "Inter"); showtext::showtext_auto(); showtext::showtext_opts(dpi = 150)

pal <- list(bg = "#0a1628", grid = "#16263f", text_main = "#e8f1ff", text_sub = "#8aa0bf",
            teal = "#34d7c2", coral = "#f5617a", amber = "#fbbf24", violet = "#c084fc",
            green = "#4ade80", orange = "#fb923c", blue = "#60a5fa")
energtx_theme <- function(base = 34) {
  theme_minimal(base_family = "Inter", base_size = base) +
    theme(plot.background = element_rect(fill = pal$bg, color = NA),
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
          axis.ticks = element_blank(), legend.position = "none",
          plot.margin = margin(34, 44, 26, 44))
}
caption_txt <- "Source: Our World in Data · <span style='color:#34d7c2;'>energtx.com</span> · @energtx.bsky.social"
save_png <- function(p, file) {
  ggsave(file.path(root, "png", file), p, device = ragg::agg_png,
         width = 3600, height = 2025, units = "px", dpi = 150)
  cat("yazildi:", file, "\n")
}
line_chart <- function(d, cols, title, subtitle, ylab, top_series, repel = FALSE) {
  lab <- d |> group_by(name) |> slice_max(year) |>
    mutate(vj = if_else(name == top_series, 1.8, -1.0))
  txt <- if (repel) {
    ggrepel::geom_text_repel(data = lab, aes(label = name), direction = "y", hjust = 1,
                             nudge_x = 0.8, min.segment.length = Inf, seed = 7,
                             box.padding = 0.5, size = 11, family = "Inter", fontface = "bold")
  } else {
    geom_text(data = lab, aes(label = name, vjust = vj), hjust = 1,
              size = 11, family = "Inter", fontface = "bold")
  }
  ggplot(d, aes(year, value, color = name)) +
    geom_line(linewidth = 2) +
    txt +
    scale_color_manual(values = cols) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.12))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylab, caption = caption_txt) +
    energtx_theme()
}

raw <- readr::read_csv("owid-energy-data.csv", show_col_types = FALSE)
owid  <- raw |> filter(!is.na(iso_code), nchar(iso_code) == 3)
world <- raw |> filter(country == "World")

# ---- 176: Gulf power demand ----
d <- owid |> filter(country %in% c("United Arab Emirates", "Saudi Arabia"), year >= 1990,
                    !is.na(per_capita_electricity)) |>
  select(name = country, year, value = per_capita_electricity) |>
  bind_rows(world |> filter(year >= 1990, !is.na(per_capita_electricity)) |>
              transmute(name = "World", year, value = per_capita_electricity))
p <- line_chart(d, c(`United Arab Emirates` = pal$teal, `Saudi Arabia` = pal$orange, World = "#8aa0bf"),
                "The Gulf runs on full power",
                "Electricity use per person (kWh), 1990-2024 - air conditioning, desalination and industry",
                "kWh per person", "United Arab Emirates")
save_png(p, "176_gulf_power_demand.png")

# ---- 177: China vs rest of world - solar ----
chn <- owid |> filter(country == "China", !is.na(solar_electricity)) |> select(year, chn = solar_electricity)
d <- world |> filter(!is.na(solar_electricity)) |> select(year, wrl = solar_electricity) |>
  inner_join(chn, by = "year") |> filter(year >= 2010) |>
  transmute(year, China = chn, `Rest of world` = wrl - chn) |>
  pivot_longer(-year)
p <- line_chart(d, c(China = pal$coral, `Rest of world` = pal$blue),
                "Solar has a capital, and it is Beijing",
                "Solar electricity generation (TWh), 2010-2024 - China now rivals the rest of the world combined",
                "TWh", "Rest of world")
save_png(p, "177_china_solar_scale.png")

# ---- 178: The West passed peak energy per person ----
d <- owid |> filter(country %in% c("United States", "Germany", "United Kingdom", "Japan"),
                    year >= 1965, !is.na(energy_per_capita)) |>
  select(name = country, year, value = energy_per_capita)
p <- line_chart(d, c(`United States` = pal$orange, Germany = pal$teal,
                     `United Kingdom` = pal$blue, Japan = pal$violet),
                "The West is past peak energy",
                "Primary energy use per person (kWh), 1965-2024 - all four peaked decades ago",
                "kWh per person", "United States", repel = TRUE)
save_png(p, "178_west_peak_energy.png")

# ---- 179: ASEAN's energy rise ----
d <- owid |> filter(country %in% c("Indonesia", "Vietnam", "Thailand", "Malaysia", "Philippines"),
                    year >= 2000, !is.na(primary_energy_consumption)) |>
  select(name = country, year, value = primary_energy_consumption)
p <- line_chart(d, c(Indonesia = pal$orange, Vietnam = pal$blue, Thailand = pal$green,
                     Malaysia = pal$violet, Philippines = pal$teal),
                "Southeast Asia switches on",
                "Primary energy consumption (TWh), 2000-2024 - five ASEAN economies, one relentless climb",
                "TWh", "Indonesia", repel = TRUE)
save_png(p, "179_asean_energy_rise.png")

# ---- 180: Oil's long slide ----
d <- world |> filter(year >= 1965, !is.na(oil_share_energy)) |> select(year, v = oil_share_energy)
lab <- bind_rows(d |> slice_max(v), d |> slice_max(year))
p <- ggplot(d, aes(year, v)) +
  geom_area(fill = "#92400E", alpha = 0.35) +
  geom_line(color = pal$orange, linewidth = 2) +
  geom_point(data = lab, color = pal$orange, size = 7) +
  geom_text(data = lab, aes(label = paste0(round(v), "%")), color = pal$orange,
            vjust = -1.1, size = 11, family = "Inter", fontface = "bold") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Oil's long slide",
       subtitle = "Oil's share of global primary energy, 1965-2024 - still the biggest fuel, but far from its peak",
       x = NULL, y = NULL, caption = caption_txt) +
  energtx_theme()
save_png(p, "180_oil_share_decline.png")

# ---- 181: Wind + solar leaders ----
d <- owid |> filter(year == 2024, !is.na(wind_share_elec), !is.na(solar_share_elec),
                    population > 1e6) |>
  transmute(country, v = wind_share_elec + solar_share_elec) |>
  slice_max(v, n = 10) |> mutate(country = fct_reorder(country, v))
p <- ggplot(d, aes(v, country)) +
  geom_col(fill = pal$teal, width = 0.62) +
  geom_text(aes(label = paste0(round(v), "%")), color = pal$teal, hjust = -0.15,
            size = 10.5, family = "Inter") +
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "The variable-renewables top 10",
       subtitle = "Wind + solar share of electricity generation, 2024 - countries above 1 million people",
       x = NULL, y = NULL, caption = caption_txt) +
  energtx_theme()
save_png(p, "181_wind_solar_leaders.png")

# ---- 182: Türkiye's wind & solar rise ----
d <- owid |> filter(country == "Turkey", year >= 2000) |>
  transmute(year, v = wind_share_elec + solar_share_elec) |> filter(!is.na(v))
lab <- d |> slice_max(year)
p <- ggplot(d, aes(year, v)) +
  geom_area(fill = pal$teal, alpha = 0.25) +
  geom_line(color = pal$teal, linewidth = 2) +
  geom_point(data = lab, color = pal$teal, size = 7) +
  geom_text(data = lab, aes(label = paste0(round(v, 1), "%")), color = pal$teal,
            vjust = -1.1, size = 12, family = "Inter", fontface = "bold") +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Türkiye catches the wind (and the sun)",
       subtitle = "Wind + solar share of electricity generation, 2000-2024",
       x = NULL, y = NULL, caption = caption_txt) +
  energtx_theme()
save_png(p, "182_turkiye_wind_solar.png")

# ---- 183: Brazil's hydro rollercoaster ----
d <- owid |> filter(country == "Brazil", year >= 1990, !is.na(hydro_share_elec)) |>
  select(year, v = hydro_share_elec)
lab <- bind_rows(d |> slice_max(v), d |> slice_min(v), d |> slice_max(year)) |>
  mutate(hj = if_else(year == max(d$year), 1.2, 0.5))
p <- ggplot(d, aes(year, v)) +
  geom_line(color = pal$blue, linewidth = 2) +
  geom_point(data = lab, color = pal$blue, size = 7) +
  geom_text(data = lab, aes(label = paste0(round(v), "%"), hjust = hj), color = pal$blue,
            vjust = -1.1, size = 10.5, family = "Inter", fontface = "bold") +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Brazil's hydro rollercoaster",
       subtitle = "Hydropower share of electricity, 1990-2024 - droughts in 2001, 2014 and 2021 carved the dips",
       x = NULL, y = NULL, caption = caption_txt) +
  energtx_theme()
save_png(p, "183_brazil_hydro_swings.png")

# ---- 184: Morocco's renewable bet ----
d <- owid |> filter(country == "Morocco", year >= 2000, !is.na(renewables_share_elec)) |>
  select(year, v = renewables_share_elec)
lab <- d |> slice_max(year)
p <- ggplot(d, aes(year, v)) +
  geom_area(fill = pal$green, alpha = 0.25) +
  geom_line(color = pal$green, linewidth = 2) +
  geom_point(data = lab, color = pal$green, size = 7) +
  geom_text(data = lab, aes(label = paste0(round(v), "%")), color = pal$green,
            vjust = -1.1, size = 12, family = "Inter", fontface = "bold") +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Morocco's renewable bet",
       subtitle = "Renewables share of electricity, 2000-2024 - Noor solar, Atlantic wind, and a grid in transition",
       x = NULL, y = NULL, caption = caption_txt) +
  energtx_theme()
save_png(p, "184_morocco_renewables.png")

# ---- 185: China vs US, per person ----
d <- owid |> filter(country %in% c("China", "United States"), year >= 1990,
                    !is.na(per_capita_electricity)) |>
  select(name = country, year, value = per_capita_electricity)
p <- line_chart(d, c(China = pal$coral, `United States` = pal$blue),
                "Per person, China is closing in",
                "Electricity use per person (kWh), 1990-2024 - the gap is closing fast",
                "kWh per person", "zzz")
save_png(p, "185_china_us_percapita.png")

cat("BATCH 6 TAMAM\n")
