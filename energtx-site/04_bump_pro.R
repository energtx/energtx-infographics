#!/usr/bin/env Rscript
# ============================================================
# "The wind-and-solar league table" — gelistirilmis bump chart
#   * duz kirik cizgi yerine SIGMOID bump egrileri
#   * nokta buyuklugu = o yilki ruzgar+gunes payi (cift kodlama)
#   * 2010 VE 2024 ilk 15'in birlesimi (19 ulke) -> dusenler de gorunuyor
#   * sag tarafta pay + sira degisimi rozeti
# Veri: energtx_country_data.csv (energtx.com ulke sayfalari)
# Cikti: 05_windsolar_league.png
# ============================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(readr); library(purrr)
  library(ggplot2); library(scales); library(ragg); library(showtext); library(sysfonts); library(ggtext)
})
root <- path.expand("~/Desktop/8_10_2026_energtx_radial"); setwd(root)
sysfonts::font_add_google("Inter", "Inter"); showtext::showtext_auto(); showtext::showtext_opts(dpi = 150)

pal <- list(bg = "#070f1e", band = "#0e1a2e", grid = "#1b2c47", text_main = "#eef4ff",
            text_sub = "#8fa5c4", teal = "#34d7c2", coral = "#f5617a", slate = "#7d90ad",
            amber = "#fbbf24")
Y0 <- 2010; Y1 <- 2024

d <- read_csv("energtx_country_data.csv", show_col_types = FALSE) |>
  mutate(country = str_trim(str_remove(country, "Energy Data$")),
         country = recode(country, "Turkey" = "Türkiye", "United Kingdom" = "UK",
                          "United States" = "USA"))
mix <- d |> filter(str_detect(indicator, "^Share of electricity from"), year >= Y0, year <= Y1) |>
  mutate(src = indicator |> str_remove("^Share of electricity from ") |> str_remove(" \\(%\\)$")) |>
  select(slug, country, year, src, value) |>
  pivot_wider(names_from = src, values_from = value, values_fill = 0)
names(mix) <- make.names(names(mix))
mix <- mix |> mutate(ws = wind + solar)

pool <- d |> filter(indicator == "Total electricity generation (TWh)", year == 2024) |>
  slice_max(value, n = 30) |> pull(slug)
bp <- mix |> filter(slug %in% pool) |> group_by(year) |>
  mutate(rank = rank(-ws, ties.method = "first")) |> ungroup()

keep <- union(bp |> filter(year == Y0, rank <= 15) |> pull(slug),
              bp |> filter(year == Y1, rank <= 15) |> pull(slug))
bp <- bp |> filter(slug %in% keep)

meta <- bp |> group_by(slug, country) |>
  summarise(r0 = rank[year == Y0], r1 = rank[year == Y1],
            v0 = ws[year == Y0], v1 = ws[year == Y1], .groups = "drop") |>
  mutate(d_rank = r0 - r1,
         cls = case_when(d_rank >= 4 ~ "up", d_rank <= -4 ~ "down", TRUE ~ "hold"),
         chip = case_when(d_rank > 0 ~ paste0("▲", d_rank),
                          d_rank < 0 ~ paste0("▼", abs(d_rank)), TRUE ~ "–"))
bp <- bp |> left_join(meta |> select(slug, cls), by = "slug")

# ---- sigmoid bump egrileri ----
sig <- function(x1, x2, y1, y2, n = 70, k = 9) {
  t <- seq(0, 1, length.out = n)
  s <- 1 / (1 + exp(-k * (t - 0.5))); s <- (s - min(s)) / (max(s) - min(s))
  tibble(x = x1 + t * (x2 - x1), y = y1 + s * (y2 - y1))
}
curve <- bp |> arrange(slug, year) |> group_by(slug, cls) |>
  group_modify(function(g, key) {
    map_dfr(seq_len(nrow(g) - 1), function(i)
      sig(g$year[i], g$year[i + 1], g$rank[i], g$rank[i + 1]) |> mutate(seg = i))
  }) |> ungroup()

rmax <- max(bp$rank)
bands <- tibble(r = seq(1, rmax, by = 2))
cls_col <- c(up = pal$teal, down = pal$coral, hold = pal$slate)
lab_l <- bp |> filter(year == Y0) |> left_join(meta |> select(slug, chip), by = "slug")
lab_r <- bp |> filter(year == Y1) |> left_join(meta |> select(slug, chip, d_rank), by = "slug")

p <- ggplot() +
  geom_rect(data = bands, aes(xmin = -Inf, xmax = Inf, ymin = r - 0.5, ymax = r + 0.5),
            fill = pal$band) +
  geom_hline(yintercept = 15.5, linetype = "22", color = pal$slate, linewidth = 1) +
  geom_path(data = curve, aes(x, y, group = slug, color = cls),
            linewidth = 3, lineend = "round", alpha = 0.95) +
  geom_point(data = bp, aes(year, rank, color = cls, size = ws)) +
  geom_point(data = bp, aes(year, rank), color = pal$bg, size = 1.4) +
  # sol etiketler
  geom_text(data = lab_l, aes(year - 0.35, rank, label = country, color = cls),
            hjust = 1, family = "Inter", fontface = "bold", size = 8.6) +
  # sag etiketler: ulke + pay + rozet
  geom_text(data = lab_r, aes(year + 0.35, rank, label = country, color = cls),
            hjust = 0, family = "Inter", fontface = "bold", size = 8.6) +
  geom_text(data = lab_r, aes(year + 3.75, rank, label = sprintf("%.0f%%", ws), color = cls),
            hjust = 1, family = "Inter", fontface = "bold", size = 8.6) +
  geom_text(data = lab_r, aes(year + 5.4, rank, label = chip,
                              color = if_else(d_rank > 0, "up", if_else(d_rank < 0, "down", "hold"))),
            hjust = 1, family = "Inter", size = 7.8) +
  annotate("text", x = Y1 + 3.75, y = 0.15, label = "2024", color = pal$text_sub,
           family = "Inter", fontface = "bold.italic", size = 7.6, hjust = 1) +
  annotate("text", x = Y1 + 5.4, y = 0.15, label = "rank", color = pal$text_sub,
           family = "Inter", fontface = "bold.italic", size = 7.6, hjust = 1) +
  annotate("richtext", x = Y0 - 3.0, y = 29.4, hjust = 0, vjust = 1, fill = NA, label.color = NA,
           family = "Inter", size = 9.4, lineheight = 1.35, text.color = pal$text_sub,
           label = paste0("**Every one of these 19 countries raised its wind-and-solar share.**<br>",
                          "India went from 2.1% to 11.0% and still lost nine places — the table<br>",
                          "measures speed against the field, not effort.")) +
  scale_color_manual(values = cls_col) +
  scale_size(range = c(3.4, 16), guide = "none") +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25), expand = expansion(add = c(4.6, 1.4))) +
  scale_x_continuous(breaks = seq(Y0, Y1, 2), limits = c(Y0 - 3.2, Y1 + 5.6),
                     expand = expansion(add = 0)) +
  labs(title = "The wind-and-solar league table",
       subtitle = paste0("Where each country ranks among the world's <b>30 largest electricity producers</b> on the<br>",
                         "share of its power that comes from wind and solar, ", Y0, " → ", Y1,
                         ". Drawn for the 19 countries that<br>made the top 15 in either year; the dashed line is the top-15 cut. ",
                         "Dot size = that year's share.<br>",
                         "<span style='color:#34d7c2;'>**Climbers**</span> gained four places or more, ",
                         "<span style='color:#f5617a;'>**slippers**</span> lost four or more."),
       caption = paste0("Data: <span style='color:#34d7c2;'>energtx.com</span> country pages ",
                        "(Ember / Energy Institute) · Chart: R + ggplot2 · @energtx.bsky.social"),
       x = NULL, y = "Rank") +
  theme_minimal(base_family = "Inter", base_size = 32) +
  theme(plot.background = element_rect(fill = pal$bg, color = NA),
        panel.background = element_rect(fill = pal$bg, color = NA),
        panel.grid = element_blank(),
        plot.title = element_markdown(color = pal$text_main, face = "bold", size = 70,
                                      margin = margin(b = 12)),
        plot.subtitle = element_markdown(color = pal$text_sub, size = 29,
                                         lineheight = 1.42, margin = margin(b = 26)),
        plot.caption = element_markdown(color = pal$text_sub, size = 23, hjust = 0,
                                        margin = margin(t = 20)),
        plot.title.position = "plot", plot.caption.position = "plot",
        axis.text.x = element_text(color = pal$text_sub, size = 29),
        axis.text.y = element_text(color = pal$text_sub, size = 26),
        axis.title.y = element_text(color = pal$text_sub, size = 26),
        axis.ticks = element_blank(), legend.position = "none",
        plot.margin = margin(40, 48, 32, 48))

ggsave("05_windsolar_league.png", p, device = ragg::agg_png,
       width = 3600, height = 2700, units = "px", dpi = 150)
cat("yazildi: 05_windsolar_league.png\n")
print(as.data.frame(meta |> arrange(r1) |> select(country, r0, r1, d_rank, v0, v1)), digits = 3)
