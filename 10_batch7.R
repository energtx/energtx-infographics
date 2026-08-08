#!/usr/bin/env Rscript
# energtx batch 7 — charts 186-190 (2026-08-08) — premium editorial set
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
          strip.text = element_text(color = pal$text_main, size = base * 0.95, face = "bold",
                                    margin = margin(b = 6)),
          strip.background = element_rect(fill = pal$bg, color = NA),
          plot.margin = margin(34, 44, 26, 44))
}
caption_txt <- "Source: Our World in Data · <span style='color:#34d7c2;'>energtx.com</span> · @energtx.bsky.social"
save_png <- function(p, file) {
  ggsave(file.path(root, "png", file), p, device = ragg::agg_png,
         width = 3600, height = 2025, units = "px", dpi = 150)
  cat("yazildi:", file, "\n")
}

raw <- readr::read_csv("owid-energy-data.csv", show_col_types = FALSE)
owid  <- raw |> filter(!is.na(iso_code), nchar(iso_code) == 3)
pop22 <- owid |> filter(year == 2022) |> select(country, pop = population)

# ---- 186: Twelve solar take-offs (small multiples) ----
cs <- c("Chile", "Australia", "Netherlands", "Spain", "Greece", "Hungary",
        "Vietnam", "India", "China", "Germany", "Japan", "Turkey")
d <- owid |> filter(country %in% cs, year >= 2010, !is.na(solar_share_elec)) |>
  mutate(country = if_else(country == "Turkey", "Türkiye", country)) |>
  select(country, year, v = solar_share_elec)
ord <- d |> filter(year == 2024) |> arrange(desc(v)) |> pull(country)
d <- d |> mutate(country = factor(country, levels = ord))
lab <- d |> group_by(country) |> slice_max(year)
p <- ggplot(d, aes(year, v)) +
  geom_area(fill = pal$amber, alpha = 0.25) +
  geom_line(color = pal$amber, linewidth = 1.4) +
  geom_text(data = lab, aes(label = paste0(round(v), "%")), color = pal$amber,
            hjust = 1.1, vjust = -0.6, size = 8.5, family = "Inter", fontface = "bold") +
  facet_wrap(~country, nrow = 3) +
  scale_y_continuous(labels = label_percent(scale = 1), breaks = c(0, 10, 20),
                     expand = expansion(mult = c(0, 0.28))) +
  scale_x_continuous(breaks = c(2012, 2022)) +
  labs(title = "Twelve solar take-offs",
       subtitle = "Solar share of electricity generation, 2010-2024 - same scale, very different launch dates",
       x = NULL, y = NULL, caption = caption_txt) +
  energtx_theme(base = 30)
save_png(p, "186_solar_takeoffs_grid.png")

# ---- 187: Many roads to clean power (composition stack) ----
d <- owid |> filter(year == 2024) |> left_join(pop22, by = "country") |>
  filter(pop > 1e6, !is.na(low_carbon_share_elec), low_carbon_share_elec >= 90) |>
  transmute(country, total = low_carbon_share_elec,
            Hydro = hydro_share_elec, Wind = wind_share_elec,
            Nuclear = nuclear_share_elec, Solar = solar_share_elec,
            Other = pmax(low_carbon_share_elec - hydro_share_elec - wind_share_elec -
                           nuclear_share_elec - solar_share_elec, 0)) |>
  arrange(desc(total))
dl <- d |> pivot_longer(-c(country, total)) |>
  mutate(name = factor(name, levels = c("Hydro", "Wind", "Nuclear", "Solar", "Other")),
         country = fct_reorder(country, total))
p <- ggplot(dl, aes(value, country, fill = name)) +
  geom_col(width = 0.7, color = pal$bg, linewidth = 0.8) +
  geom_text(data = d |> mutate(country = fct_reorder(country, total)),
            aes(x = total, y = country, label = paste0(round(total), "%")),
            inherit.aes = FALSE, color = pal$text_main, hjust = -0.2,
            size = 10, family = "Inter", fontface = "bold") +
  scale_fill_manual(values = c(Hydro = pal$blue, Wind = pal$teal, Nuclear = pal$violet,
                               Solar = pal$amber, Other = pal$green), name = NULL) +
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Many roads to clean power",
       subtitle = "Countries with 90%+ low-carbon electricity in 2024 - dams, reactors, turbines or sun",
       x = NULL, y = NULL, caption = caption_txt) +
  energtx_theme() +
  theme(legend.position = "top", legend.justification = "left",
        legend.text = element_text(color = pal$text_sub, size = 30))
save_png(p, "187_roads_to_clean_power.png")

# ---- 188: When did fossil fuels peak? ----
majors <- c("United Kingdom", "Germany", "France", "Italy", "Japan", "United States",
            "Spain", "Poland", "Canada", "Australia", "South Korea", "Turkey",
            "Brazil", "China", "India", "Indonesia", "Saudi Arabia", "Mexico")
pk <- owid |> filter(country %in% majors, !is.na(fossil_fuel_consumption), year <= 2024) |>
  group_by(country) |> slice_max(fossil_fuel_consumption, with_ties = FALSE) |>
  ungroup() |>
  mutate(country = if_else(country == "Turkey", "Türkiye", country),
         status = if_else(year >= 2022, "Not yet", "Peaked"),
         country = fct_reorder(country, year))
p <- ggplot(pk, aes(year, country, color = status)) +
  geom_segment(aes(x = 1965, xend = year, yend = country), color = pal$grid, linewidth = 2) +
  geom_point(size = 8) +
  geom_text(aes(label = if_else(status == "Not yet", "still rising", as.character(year))),
            hjust = -0.35, size = 9.5, family = "Inter", fontface = "bold") +
  scale_color_manual(values = c(Peaked = pal$teal, `Not yet` = pal$coral)) +
  scale_x_continuous(limits = c(1965, 2036), breaks = seq(1970, 2020, 10)) +
  labs(title = "When did fossil fuels peak?",
       subtitle = "Year of each country's highest-ever fossil fuel consumption - teal has peaked, coral is still climbing",
       x = NULL, y = NULL, caption = caption_txt) +
  energtx_theme()
save_png(p, "188_fossil_peak_years.png")

# ---- 189: Wind country, sun country (quadrant scatter) ----
d <- owid |> filter(year == 2024) |> left_join(pop22, by = "country") |>
  filter(pop > 5e6, !is.na(wind_share_elec), !is.na(solar_share_elec)) |>
  transmute(country = if_else(country == "Turkey", "Türkiye", country),
            w = wind_share_elec, s = solar_share_elec)
hl <- c("Denmark", "Netherlands", "Spain", "Germany", "Greece", "Chile", "Australia",
        "United Kingdom", "Portugal", "Hungary", "India", "China", "United States",
        "Japan", "Brazil", "Vietnam", "Türkiye", "Poland", "Morocco", "Sweden", "Ireland")
p <- ggplot(d, aes(w, s)) +
  geom_hline(yintercept = 10, linetype = 2, color = pal$text_sub, linewidth = 0.6) +
  geom_vline(xintercept = 10, linetype = 2, color = pal$text_sub, linewidth = 0.6) +
  annotate("text", x = 52, y = 2,  label = "WIND COUNTRY", color = pal$text_sub,
           size = 9, family = "Inter", fontface = "bold", hjust = 1) +
  annotate("text", x = 2, y = 27, label = "SUN COUNTRY", color = pal$text_sub,
           size = 9, family = "Inter", fontface = "bold", hjust = 0) +
  annotate("text", x = 52, y = 27, label = "ALL-IN", color = pal$text_sub,
           size = 9, family = "Inter", fontface = "bold", hjust = 1) +
  geom_point(color = pal$teal, size = 5, alpha = 0.85) +
  ggrepel::geom_text_repel(data = d |> filter(country %in% hl), aes(label = country),
                           color = pal$text_main, size = 8.5, family = "Inter",
                           seed = 11, box.padding = 0.35, max.overlaps = 30,
                           segment.color = pal$grid) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Wind country, sun country",
       subtitle = "Wind vs solar share of electricity, 2024 - countries above 5 million people",
       x = "Wind share of electricity", y = "Solar share of electricity", caption = caption_txt) +
  energtx_theme()
save_png(p, "189_wind_sun_quadrant.png")

# ---- 190: The wind & solar club, from zero ----
# NOT: low_carbon_share_elec kapsami 2000 oncesi dar (Ember 2000'de basliyor) ->
# donem 2000-2024 ile sinirlandi; metrik ruzgar+gunes >%25.
d <- owid |> left_join(pop22, by = "country") |>
  filter(pop > 1e6, year >= 2000, year <= 2024,
         !is.na(wind_share_elec), !is.na(solar_share_elec)) |>
  mutate(ws = wind_share_elec + solar_share_elec) |>
  group_by(year) |> summarise(n = sum(ws > 25), .groups = "drop")
last <- d |> slice_max(year)
p <- ggplot(d, aes(year, n)) +
  geom_area(fill = pal$teal, alpha = 0.2) +
  geom_line(color = pal$teal, linewidth = 2) +
  geom_point(data = last, color = pal$teal, size = 7) +
  geom_text(data = last, aes(label = paste(n, "countries")), color = pal$teal,
            hjust = 1.05, vjust = -1.1, size = 12, family = "Inter", fontface = "bold") +
  annotate("text", x = 2001, y = max(d$n) * 0.85,
           label = "in 2000: zero", color = pal$text_sub,
           size = 10.5, family = "Inter", fontface = "italic", hjust = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "The 25% wind-and-solar club, from zero",
       subtitle = "Countries (1M+ people) getting over a quarter of their electricity from wind + solar, 2000-2024",
       x = NULL, y = "countries", caption = caption_txt) +
  energtx_theme()
save_png(p, "190_wind_solar_club.png")

cat("BATCH 7 TAMAM\n")
