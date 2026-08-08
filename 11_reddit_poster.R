#!/usr/bin/env Rscript
# energtx — Reddit poster: "When did fossil fuels peak?" (191)
# Portrait 2600x3600, 40 countries, two aligned panels (peak year + decline since peak).
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(forcats)
  library(scales); library(ragg); library(showtext); library(sysfonts); library(ggtext); library(patchwork)
})
root <- "/home/fbartuyurdacan/energtx-infographics"; setwd(root)
sysfonts::font_add_google("Inter", "Inter"); showtext::showtext_auto(); showtext::showtext_opts(dpi = 150)

pal <- list(bg = "#0a1628", grid = "#16263f", text_main = "#e8f1ff", text_sub = "#8aa0bf",
            coral = "#f5617a", t_old = "#0d9488", t_mid = "#34d7c2", t_new = "#99f6e4")

base_theme <- function(base = 26) {
  theme_minimal(base_family = "Inter", base_size = base) +
    theme(plot.background = element_rect(fill = pal$bg, color = NA),
          panel.background = element_rect(fill = pal$bg, color = NA),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = pal$grid, linewidth = 0.5),
          axis.text = element_text(color = pal$text_sub, size = base * 0.95),
          axis.title = element_blank(), axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin = margin(6, 10, 6, 10))
}

raw <- readr::read_csv("owid-energy-data.csv", show_col_types = FALSE)
owid <- raw |> filter(!is.na(iso_code), nchar(iso_code) == 3)

top40 <- owid |> filter(year == 2023, !is.na(fossil_fuel_consumption)) |>
  slice_max(fossil_fuel_consumption, n = 40) |> pull(country)

pk <- owid |> filter(country %in% top40, !is.na(fossil_fuel_consumption)) |>
  group_by(country) |>
  summarise(peak_year = year[which.max(fossil_fuel_consumption)],
            peak_val  = max(fossil_fuel_consumption),
            last_val  = fossil_fuel_consumption[which.max(year)],
            .groups = "drop") |>
  mutate(country = recode(country, "Turkey" = "Türkiye",
                          "United States" = "United States", "United Kingdom" = "United Kingdom"),
         status = if_else(peak_year >= 2022, "rising", "peaked"),
         decline = if_else(status == "peaked", (1 - last_val / peak_val) * 100, NA_real_),
         cls = case_when(status == "rising" ~ "rising",
                         peak_year < 1990 ~ "old",
                         peak_year < 2010 ~ "mid",
                         TRUE ~ "new"),
         country = fct_reorder(country, peak_year, .desc = TRUE))

cols <- c(old = pal$t_old, mid = pal$t_mid, new = pal$t_new, rising = pal$coral)

# ---- sol panel: zirve yili zaman cizelgesi ----
p1 <- ggplot(pk, aes(y = country)) +
  geom_vline(xintercept = c(1973, 2008), color = pal$grid, linewidth = 1.4) +
  annotate("text", x = 1973, y = 41.4, label = "1973 oil shock", color = pal$text_sub,
           size = 7.5, family = "Inter", hjust = 0.5, fontface = "italic") +
  annotate("text", x = 2008, y = 41.4, label = "2008 crisis", color = pal$text_sub,
           size = 7.5, family = "Inter", hjust = 0.5, fontface = "italic") +
  geom_segment(aes(x = 1965, xend = peak_year, yend = country),
               color = pal$grid, linewidth = 2.6) +
  geom_point(aes(x = peak_year, color = cls), size = 6.5) +
  geom_text(aes(x = peak_year, color = cls,
                label = if_else(status == "rising", "▲ rising", as.character(peak_year))),
            hjust = -0.28, size = 7.8, family = "Inter", fontface = "bold") +
  scale_color_manual(values = cols) +
  scale_x_continuous(limits = c(1965, 2040), breaks = seq(1970, 2020, 10),
                     expand = expansion(mult = c(0.01, 0))) +
  scale_y_discrete(expand = expansion(add = c(0.6, 2.6))) +
  base_theme() +
  theme(axis.text.y = element_text(color = pal$text_main, size = 26, family = "Inter"))

# ---- sag panel: zirveden bugune dusus ----
p2 <- ggplot(pk, aes(y = country)) +
  geom_col(aes(x = decline, fill = cls), width = 0.62, na.rm = TRUE) +
  geom_text(data = pk |> filter(status == "peaked"),
            aes(x = decline, label = paste0("-", round(decline), "%"), color = cls),
            hjust = -0.18, size = 7.4, family = "Inter", fontface = "bold") +
  geom_text(data = pk |> filter(status == "rising"),
            aes(x = 0, label = "▲"), color = pal$coral,
            hjust = -0.4, size = 7.4, family = "Inter") +
  scale_fill_manual(values = cols) + scale_color_manual(values = cols) +
  scale_x_continuous(limits = c(0, 78), breaks = c(0, 25, 50),
                     labels = function(x) if_else(x == 0, "0", paste0("-", x, "%")),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_discrete(expand = expansion(add = c(0.6, 2.6))) +
  annotate("text", x = 39, y = 41.4, label = "below peak today", color = pal$text_sub,
           size = 7.5, family = "Inter", hjust = 0.5, fontface = "italic") +
  base_theme() +
  theme(axis.text.y = element_blank())

title_md <- paste0(
  "<span style='font-size:64pt;'>**When did fossil fuels peak?**</span><br><br>",
  "<span style='font-size:23pt;color:#8aa0bf;'>Year of the highest-ever fossil fuel consumption in the 40 largest fossil-fuel-consuming countries —<br>",
  "and how far below that peak each one is today.<br>",
  "<span style='color:#0d9488;'>**● pre-1990**</span> · ",
  "<span style='color:#34d7c2;'>**● 1990-2009**</span> · ",
  "<span style='color:#99f6e4;'>**● 2010+**</span> · ",
  "<span style='color:#f5617a;'>**▲ no peak yet — still rising**</span></span>")

poster <- p1 + p2 + plot_layout(widths = c(2.7, 1)) +
  plot_annotation(
    title = title_md,
    caption = "Data: Our World in Data energy dataset (2024) · Chart: R + ggplot2 · energtx.com",
    theme = theme(
      plot.background = element_rect(fill = pal$bg, color = NA),
      plot.title = element_markdown(color = pal$text_main, family = "Inter",
                                    lineheight = 1.25, margin = margin(t = 10, b = 26)),
      plot.caption = element_text(color = pal$text_sub, family = "Inter", size = 22,
                                  hjust = 0, margin = margin(t = 16)),
      plot.margin = margin(40, 48, 30, 48)))

ggsave(file.path(root, "png", "191_fossil_peak_poster.png"), poster,
       device = ragg::agg_png, width = 2600, height = 3600, units = "px", dpi = 150)
cat("yazildi: 191_fossil_peak_poster.png\n")
