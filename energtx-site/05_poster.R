#!/usr/bin/env Rscript
# ============================================================
# "Twenty-five years, twenty-four grids" — kompoze infografik poster
#   baslik blogu + 4'lu KPI seridi + 24 panel %100 yigilmis alan + lejant + kunye
# Veri: energtx_country_data.csv (energtx.com ulke sayfalari, kaynak: Ember)
# Cikti: 06_grid_poster.png
# ============================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(readr); library(forcats)
  library(ggplot2); library(scales); library(ragg); library(showtext); library(sysfonts)
  library(ggtext); library(patchwork)
})
root <- path.expand("~/Desktop/8_10_2026_energtx_radial"); setwd(root)
sysfonts::font_add_google("Inter", "Inter"); showtext::showtext_auto(); showtext::showtext_opts(dpi = 150)

pal <- list(bg = "#070f1e", panel = "#0c1729", rule = "#20304c", text_main = "#eef4ff",
            text_sub = "#8fa5c4", teal = "#34d7c2", amber = "#fbbf24", coral = "#f5617a")

d <- read_csv("energtx_country_data.csv", show_col_types = FALSE) |>
  mutate(country = str_trim(str_remove(country, "Energy Data$")),
         country = recode(country, "Turkey" = "Türkiye", "United Kingdom" = "UK",
                          "United States" = "United States", "United Arab Emirates" = "UAE"))

src_lv <- c("Coal", "Other fossil", "Gas", "Nuclear", "Hydro", "Bioenergy",
            "Other renew.", "Wind", "Solar")
src_col <- c(Coal = "#c2455f", `Other fossil` = "#a97452", Gas = "#fb923c",
             Nuclear = "#c084fc", Hydro = "#60a5fa", Bioenergy = "#4ade80",
             `Other renew.` = "#22c55e", Wind = "#34d7c2", Solar = "#fbbf24")

mix <- d |> filter(str_detect(indicator, "^Share of electricity from"), year >= 2000, year <= 2024) |>
  mutate(src = indicator |> str_remove("^Share of electricity from ") |> str_remove(" \\(%\\)$") |>
           recode("other fossil" = "Other fossil", "other renewables" = "Other renew.",
                  "coal" = "Coal", "gas" = "Gas", "hydro" = "Hydro", "nuclear" = "Nuclear",
                  "solar" = "Solar", "wind" = "Wind", "bioenergy" = "Bioenergy")) |>
  select(slug, country, year, src, value) |>
  complete(nesting(slug, country, year), src = src_lv, fill = list(value = 0)) |>
  mutate(src = factor(src, levels = src_lv))

wide <- mix |> pivot_wider(names_from = src, values_from = value, values_fill = 0)
names(wide) <- make.names(names(wide))
wide <- wide |> mutate(fossil = Coal + Gas + Other.fossil, ws = Wind + Solar, lowc = 100 - fossil)

# ---------- KPI'lar (2000 ve 2024'te verisi olan 105 ulke) ----------
both <- wide |> filter(year %in% c(2000, 2024)) |> group_by(slug) |> filter(n() == 2) |> ungroup()
k <- both |> group_by(year) |> summarise(n = n(), medfos = median(fossil),
                                         ws25 = sum(ws >= 25), sol10 = sum(Solar >= 10),
                                         lc90 = sum(lowc >= 90), .groups = "drop")
g <- function(y, col) k[[col]][k$year == y]

kpi <- tibble(
  i    = 1:4,
  big  = c(sprintf("0 → %d", g(2024, "ws25")),
           sprintf("0 → %d", g(2024, "sol10")),
           sprintf("%.0f%% → %.0f%%", g(2000, "medfos"), g(2024, "medfos")),
           sprintf("%d → %d", g(2000, "lc90"), g(2024, "lc90"))),
  head = c("A QUARTER FROM WIND & SUN", "A TENTH FROM SOLAR ALONE",
           "MEDIAN GRID'S FOSSIL SHARE", "ALREADY 90% LOW-CARBON"),
  sub  = c("countries, 2000 → 2024", "countries, 2000 → 2024",
           "still more than half", "same count: four left, four joined"),
  col  = c(pal$teal, pal$amber, pal$coral, pal$text_main))

p_kpi <- ggplot(kpi) +
  annotate("segment", x = 0.44, xend = 4.56, y = 1.0, yend = 1.0, color = pal$rule, linewidth = 1.1) +
  annotate("segment", x = 0.44, xend = 4.56, y = 0.0, yend = 0.0, color = pal$rule, linewidth = 1.1) +
  geom_segment(aes(x = i - 0.5, xend = i - 0.5, y = 0.06, yend = 0.94),
               data = kpi |> filter(i > 1), color = pal$rule, linewidth = 0.8) +
  geom_text(aes(i - 0.44, 0.86, label = head), hjust = 0, color = pal$text_sub,
            family = "Inter", fontface = "bold", size = 6.6) +
  geom_text(aes(i - 0.44, 0.50, label = big, color = col), hjust = 0,
            family = "Inter", fontface = "bold", size = 16.5) +
  geom_text(aes(i - 0.44, 0.13, label = sub), hjust = 0, color = pal$text_sub,
            family = "Inter", size = 6.6) +
  scale_color_identity() +
  scale_x_continuous(limits = c(0.44, 4.56), expand = expansion(add = 0)) +
  scale_y_continuous(limits = c(-0.02, 1.02), expand = expansion(add = 0)) +
  theme_void() + theme(plot.background = element_rect(fill = pal$bg, color = NA),
                       plot.margin = margin(6, 0, 18, 0))

# ---------- ana panel: 24 ulke, %100 yigilmis alan ----------
gen <- d |> filter(indicator == "Total electricity generation (TWh)", year == 2024) |> select(slug, gen = value)
pick <- wide |> filter(year == 2024) |> inner_join(gen, by = "slug") |>
  slice_max(gen, n = 24) |>
  mutate(lab = paste0("**", country, "**  <span style='color:#8fa5c4;'>", round(lowc), "%</span>"),
         lab = fct_reorder(lab, -lowc)) |>
  select(slug, lab, lowc)

pf <- mix |> inner_join(pick, by = "slug")

p_main <- ggplot(pf, aes(year, value, fill = src)) +
  geom_area(position = position_stack(reverse = TRUE), color = NA) +
  geom_hline(yintercept = 50, color = "#0a1424", linewidth = 0.7, linetype = "22") +
  facet_wrap(~lab, ncol = 4) +
  scale_fill_manual(values = src_col, drop = FALSE, name = NULL) +
  scale_x_continuous(breaks = c(2000, 2012, 2024), labels = c("'00", "'12", "'24"),
                     expand = expansion(add = 0)) +
  scale_y_continuous(breaks = c(0, 50, 100), labels = c("0", "50", "100%"),
                     expand = expansion(add = 0)) +
  guides(fill = guide_legend(nrow = 1, keywidth = unit(30, "pt"), keyheight = unit(22, "pt"))) +
  theme_minimal(base_family = "Inter", base_size = 28) +
  theme(plot.background = element_rect(fill = pal$bg, color = NA),
        panel.background = element_rect(fill = pal$panel, color = NA),
        panel.grid = element_blank(), panel.spacing.x = unit(38, "pt"), panel.spacing.y = unit(18, "pt"),
        strip.text = element_markdown(color = pal$text_main, size = 27, hjust = 0,
                                      margin = margin(t = 14, b = 6)),
        axis.text = element_text(color = pal$text_sub, size = 20),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", legend.justification = "left",
        legend.text = element_text(color = pal$text_sub, size = 25),
        legend.margin = margin(t = 26, b = 0),
        plot.margin = margin(0, 0, 0, 0))

poster <- p_kpi / p_main +
  plot_layout(heights = c(1.05, 9)) +
  plot_annotation(
    title = "Twenty-five years, twenty-four grids",
    subtitle = paste0(
      "How the world's 24 largest electricity producers actually make their power, **2000 → 2024**.<br>",
      "Each band is one source; the number beside each name is its 2024 low-carbon share.<br>",
      "Panels run from the cleanest grid to the most fossil-fuelled; the dashed line is halfway.<br>",
      "The <span style='color:#fbbf24;'>**solar**</span> and ",
      "<span style='color:#34d7c2;'>**wind**</span> wedges on top of each panel did not exist in 2000."),
    caption = paste0("Source: Ember, via the country pages on ",
                     "<span style='color:#34d7c2;'>energtx.com</span> · 235,061 records, 106 countries · ",
                     "Chart: R + ggplot2 · @energtx.bsky.social"),
    theme = theme(
      plot.background = element_rect(fill = pal$bg, color = NA),
      plot.title = element_markdown(color = pal$text_main, face = "bold", family = "Inter",
                                    size = 78, margin = margin(b = 16)),
      plot.subtitle = element_markdown(color = pal$text_sub, family = "Inter", size = 30,
                                       lineheight = 1.45, margin = margin(b = 34)),
      plot.caption = element_markdown(color = pal$text_sub, family = "Inter", size = 22,
                                      hjust = 0, margin = margin(t = 26)),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.margin = margin(46, 54, 34, 54)))

ggsave("06_grid_poster.png", poster, device = ragg::agg_png,
       width = 3400, height = 4500, units = "px", dpi = 150)
cat("yazildi: 06_grid_poster.png\n")
print(as.data.frame(k), digits = 3)
