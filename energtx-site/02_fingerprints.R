#!/usr/bin/env Rscript
# ============================================================
# "Fingerprints of the Grid" — energtx.com verisiyle radyal deneme
# Veri : energtx_country_data.csv (01_scrape.R ile energtx.com'dan)
# Cikti: 01_grid_fingerprints.png
# ============================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(readr); library(forcats)
  library(ggplot2); library(scales); library(ragg); library(showtext); library(sysfonts); library(ggtext)
})
root <- path.expand("~/Desktop/8_10_2026_energtx_radial"); setwd(root)
sysfonts::font_add_google("Inter", "Inter"); showtext::showtext_auto(); showtext::showtext_opts(dpi = 150)

pal <- list(bg = "#070f1e", ring = "#1c2e4a", text_main = "#eef4ff", text_sub = "#8fa5c4")
YR <- 2024; NTOP <- 24

d <- read_csv("energtx_country_data.csv", show_col_types = FALSE) |>
  mutate(country = str_trim(str_remove(country, "Energy Data$")),
         country = recode(country, "Turkey" = "Türkiye", "United Kingdom" = "UK"))

# ---- kaynak sirasi: sol yari fosil, sag yari dusuk karbon ----
src_lv <- c("Coal", "Gas", "Other fossil", "Nuclear", "Hydro",
            "Bioenergy", "Other renew.", "Wind", "Solar")
src_col <- c(Coal = "#c2455f", Gas = "#fb923c", `Other fossil` = "#a97452",
             Nuclear = "#c084fc", Hydro = "#60a5fa", Bioenergy = "#4ade80",
             `Other renew.` = "#22c55e", Wind = "#34d7c2", Solar = "#fbbf24")
clean_src <- c("Nuclear", "Hydro", "Bioenergy", "Other renew.", "Wind", "Solar")

mix <- d |>
  filter(str_detect(indicator, "^Share of electricity from"), year == YR) |>
  mutate(src = indicator |> str_remove("^Share of electricity from ") |> str_remove(" \\(%\\)$") |>
           recode("other fossil" = "Other fossil", "other renewables" = "Other renew.",
                  "coal" = "Coal", "gas" = "Gas", "hydro" = "Hydro", "nuclear" = "Nuclear",
                  "solar" = "Solar", "wind" = "Wind", "bioenergy" = "Bioenergy")) |>
  select(slug, country, src, value) |>
  complete(nesting(slug, country), src = src_lv, fill = list(value = 0)) |>
  mutate(src = factor(src, levels = src_lv))

gen <- d |> filter(indicator == "Total electricity generation (TWh)", year == YR) |>
  select(slug, gen = value)

pick <- mix |> group_by(slug, country) |>
  summarise(clean = sum(value[src %in% clean_src]), tot = sum(value), .groups = "drop") |>
  filter(between(tot, 99, 101)) |>
  inner_join(gen, by = "slug") |>
  slice_max(gen, n = NTOP) |>
  mutate(lab = paste0("**", country, "**<br><span style='font-size:22pt;color:#8fa5c4;'>",
                      if_else(gen >= 1000, sprintf("%.1fk", gen / 1000), sprintf("%.0f", gen)),
                      " TWh · ", round(clean), "%</span>"),
         lab = fct_reorder(lab, -clean))

pf <- mix |> inner_join(pick |> select(slug, lab), by = "slug")
rings <- tibble(r = sqrt(c(25, 50, 75, 100)))

p <- ggplot(pf, aes(x = src, y = sqrt(value), fill = src)) +
  geom_hline(data = rings, aes(yintercept = r), color = pal$ring, linewidth = 0.5) +
  geom_col(width = 1, color = pal$bg, linewidth = 0.6) +
  coord_polar(start = -pi / 9) +
  facet_wrap(~lab, ncol = 6) +
  scale_fill_manual(values = src_col, drop = FALSE, name = NULL) +
  scale_y_continuous(limits = c(0, sqrt(100)), expand = expansion(mult = c(0, 0.03))) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(color = NA),
                             keywidth = unit(24, "pt"), keyheight = unit(24, "pt"))) +
  labs(title = "Fingerprints of the grid",
       subtitle = paste0(
         "Every country's electricity mix drawn as a rose. Each petal is one source and its<br>",
         "<b>area</b> is that source's share of generation in ", YR, ". Faint rings mark 25, 50, 75, 100%.<br>",
         "<span style='color:#f5617a;'>**Fossil petals**</span> open to the left, ",
         "<span style='color:#34d7c2;'>**low-carbon petals**</span> to the right, so the silhouette<br>",
         "alone tells you which way a grid leans. The ", NTOP,
         " largest producers, cleanest grid first."),
       caption = paste0("Data scraped from <span style='color:#34d7c2;'>energtx.com</span> country pages ",
                        "· 106 countries · R + ggplot2 · @energtx.bsky.social")) +
  theme_minimal(base_family = "Inter", base_size = 30) +
  theme(plot.background = element_rect(fill = pal$bg, color = NA),
        panel.background = element_rect(fill = pal$bg, color = NA),
        panel.grid = element_blank(), panel.spacing = unit(2, "pt"),
        axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        strip.text = element_markdown(color = pal$text_main, size = 25, lineheight = 1.3,
                                      margin = margin(t = 14, b = 0)),
        plot.title = element_markdown(color = pal$text_main, face = "bold", size = 78,
                                      margin = margin(b = 14)),
        plot.subtitle = element_markdown(color = pal$text_sub, size = 29,
                                         lineheight = 1.45, margin = margin(b = 10)),
        plot.caption = element_markdown(color = pal$text_sub, size = 23, hjust = 0,
                                        margin = margin(t = 20)),
        plot.title.position = "plot", plot.caption.position = "plot",
        legend.position = "bottom", legend.justification = "left",
        legend.text = element_text(color = pal$text_sub, size = 26),
        legend.margin = margin(t = 16),
        plot.margin = margin(40, 48, 30, 48))

ggsave("01_grid_fingerprints.png", p, device = ragg::agg_png,
       width = 3400, height = 3000, units = "px", dpi = 150)
cat("yazildi: 01_grid_fingerprints.png\n")
