#!/usr/bin/env Rscript
# ============================================================
# energtx veritabanindan 3 ozgun formlu elektrik gorseli
#   02_transition_plane.png  — baglantili sacilim (trajectory)
#   03_windsolar_bump.png    — siralama akisi (bump chart)
#   04_windsolar_ridgeline.png — yillara gore dagilim sirti (ridgeline)
# Veri: energtx_country_data.csv  (energtx.com ulke sayfalari)
# ============================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(readr); library(forcats); library(purrr)
  library(ggplot2); library(scales); library(ragg); library(showtext); library(sysfonts)
  library(ggtext); library(ggrepel)
})
root <- path.expand("~/Desktop/8_10_2026_energtx_radial"); setwd(root)
sysfonts::font_add_google("Inter", "Inter"); showtext::showtext_auto(); showtext::showtext_opts(dpi = 150)

pal <- list(bg = "#070f1e", grid = "#182a45", text_main = "#eef4ff", text_sub = "#8fa5c4",
            teal = "#34d7c2", coral = "#f5617a", amber = "#fbbf24", violet = "#c084fc",
            blue = "#60a5fa", slate = "#4a5c78", green = "#4ade80")

etheme <- function(base = 32) {
  theme_minimal(base_family = "Inter", base_size = base) +
    theme(plot.background = element_rect(fill = pal$bg, color = NA),
          panel.background = element_rect(fill = pal$bg, color = NA),
          panel.grid.major = element_line(color = pal$grid, linewidth = 0.5),
          panel.grid.minor = element_blank(),
          plot.title = element_markdown(color = pal$text_main, face = "bold",
                                        size = base * 2.1, margin = margin(b = 10), lineheight = 1.1),
          plot.subtitle = element_markdown(color = pal$text_sub, size = base * 0.92,
                                           margin = margin(b = 24), lineheight = 1.4),
          plot.caption = element_markdown(color = pal$text_sub, size = base * 0.72,
                                          hjust = 0, margin = margin(t = 18)),
          plot.title.position = "plot", plot.caption.position = "plot",
          axis.text = element_text(color = pal$text_sub, size = base * 0.9),
          axis.title = element_text(color = pal$text_sub, size = base * 0.9),
          axis.ticks = element_blank(), legend.position = "none",
          plot.margin = margin(38, 48, 30, 48))
}
cap <- "Data: <span style='color:#34d7c2;'>energtx.com</span> country pages (235,061 records · 106 countries) · Chart: R + ggplot2 · @energtx.bsky.social"
sv <- function(p, f, w = 3600, h = 2400) {
  ggsave(f, p, device = ragg::agg_png, width = w, height = h, units = "px", dpi = 150)
  cat("yazildi:", f, "\n")
}

# ---- veri hazirligi ----
d <- read_csv("energtx_country_data.csv", show_col_types = FALSE) |>
  mutate(country = str_trim(str_remove(country, "Energy Data$")),
         country = recode(country, "Turkey" = "Türkiye", "United Kingdom" = "UK",
                          "United States" = "USA", "United Arab Emirates" = "UAE"))

mix <- d |> filter(str_detect(indicator, "^Share of electricity from"), year >= 2000, year <= 2024) |>
  mutate(src = indicator |> str_remove("^Share of electricity from ") |> str_remove(" \\(%\\)$")) |>
  select(slug, country, year, src, value) |>
  pivot_wider(names_from = src, values_from = value, values_fill = 0)
names(mix) <- make.names(names(mix))
mix <- mix |> mutate(fossil = coal + gas + other.fossil, ws = wind + solar)

gen24 <- d |> filter(indicator == "Total electricity generation (TWh)", year == 2024) |> select(slug, gen = value)
big30 <- gen24 |> slice_max(gen, n = 30) |> pull(slug)

# ============================================================
# 02 — THE TRANSITION PLANE (connected scatter)
# ============================================================
sel <- c("germany", "spain", "united-kingdom", "australia", "poland", "brazil", "italy",
         "turkiye", "china", "united-states", "france", "india", "japan", "vietnam",
         "south-africa", "saudi-arabia")
sel <- intersect(sel, unique(mix$slug))
tr <- mix |> filter(slug %in% sel) |> arrange(slug, year)
tr_end <- tr |> group_by(slug, country) |> slice_max(year, n = 1) |> ungroup()
tr_st  <- tr |> group_by(slug) |> slice_min(year, n = 1) |> ungroup()
tr_arw <- tr |> group_by(slug) |> slice_max(year, n = 2) |> ungroup() |>
  arrange(slug, year) |> group_by(slug) |>
  summarise(x = first(fossil), y = first(ws), xe = last(fossil), ye = last(ws), .groups = "drop")

p2 <- ggplot(tr, aes(fossil, ws, group = slug)) +
  annotate("rect", xmin = -Inf, xmax = 50, ymin = 20, ymax = Inf, fill = pal$teal, alpha = 0.05) +
  geom_path(aes(color = year), linewidth = 2.4, lineend = "round") +
  geom_segment(data = tr_arw, aes(x = x, y = y, xend = xe, yend = ye), inherit.aes = FALSE,
               color = pal$teal, linewidth = 2.4,
               arrow = arrow(length = unit(15, "pt"), type = "closed")) +
  geom_point(data = tr_st, aes(fossil, ws), inherit.aes = FALSE,
             shape = 21, fill = pal$bg, color = pal$slate, size = 5, stroke = 1.6) +
  geom_text_repel(data = tr_end, aes(fossil, ws, label = country), inherit.aes = FALSE,
                  color = pal$text_main, family = "Inter", fontface = "bold", size = 9.2,
                  seed = 7, min.segment.length = 0.4, segment.color = pal$slate,
                  box.padding = 0.7, point.padding = 0.5, max.overlaps = 30) +
  annotate("richtext", x = 4, y = 47.5, hjust = 0, fill = NA, label.color = NA,
           family = "Inter", size = 9.5, lineheight = 1.3, text.color = pal$teal,
           label = "**Wind & solar did the work**<br><span style='font-size:25pt;'>less fossil, more turbines and panels</span>") +
  annotate("richtext", x = 26, y = 10, hjust = 0, vjust = 1, fill = NA, label.color = NA,
           family = "Inter", size = 9, lineheight = 1.35, text.color = pal$blue,
           label = "**Clean without<br>wind or sun**<br><span style='font-size:24pt;'>hydro and nuclear<br>already did it</span>") +
  annotate("richtext", x = 99, y = 20, hjust = 1, fill = NA, label.color = NA,
           family = "Inter", size = 9.5, lineheight = 1.3, text.color = pal$coral,
           label = "**Still burning**<br><span style='font-size:25pt;'>fossil grids that barely moved</span>") +
  scale_color_gradient(low = "#22384f", high = pal$teal) +
  scale_x_continuous(labels = label_percent(scale = 1), breaks = seq(0, 100, 20), limits = c(0, 100)) +
  scale_y_continuous(labels = label_percent(scale = 1), breaks = seq(0, 50, 10), limits = c(-1, 52)) +
  labs(title = "The transition plane",
       subtitle = paste0("Each line is one country's path from <b>2000</b> (hollow dot) to <b>2024</b> (arrow head).<br>",
                         "Moving <b>left</b> means burning less fossil fuel; moving <b>up</b> means wind and solar<br>",
                         "took over. Lines brighten as time passes."),
       caption = cap, x = "Fossil share of electricity", y = "Wind + solar share of electricity") +
  etheme()
sv(p2, "02_transition_plane.png")

# ============================================================
# 03 — THE WIND-AND-SOLAR LEAGUE (bump chart)
# ============================================================
bp <- mix |> filter(slug %in% big30, year >= 2010) |>
  group_by(year) |> mutate(rank = rank(-ws, ties.method = "first")) |> ungroup()
keep <- bp |> filter(year == 2024, rank <= 15) |> pull(slug)
bp <- bp |> filter(slug %in% keep)
mov <- bp |> group_by(slug, country) |>
  summarise(r0 = rank[year == 2010], r1 = rank[year == 2024], v1 = ws[year == 2024], .groups = "drop") |>
  mutate(cls = case_when(r0 - r1 >= 4 ~ "up", r1 - r0 >= 4 ~ "down", TRUE ~ "flat"))
bp <- bp |> left_join(mov |> select(slug, cls), by = "slug")
lab_l <- bp |> filter(year == 2010); lab_r <- bp |> filter(year == 2024)
cls_col <- c(up = pal$teal, down = pal$coral, flat = pal$slate)

p3 <- ggplot(bp, aes(year, rank, group = slug, color = cls)) +
  geom_line(linewidth = 2.6, lineend = "round", alpha = 0.9) +
  geom_point(size = 5.2) +
  geom_text(data = lab_l, aes(label = country), hjust = 1, nudge_x = -0.45,
            family = "Inter", fontface = "bold", size = 8.4) +
  geom_text(data = lab_r, aes(label = paste0(country, "  ", round(ws), "%")), hjust = 0, nudge_x = 0.45,
            family = "Inter", fontface = "bold", size = 8.4) +
  scale_color_manual(values = cls_col) +
  geom_hline(yintercept = 15.5, linetype = "22", color = pal$slate, linewidth = 0.9) +
  annotate("text", x = 2027.4, y = 15.5, label = "top 15", color = pal$slate,
           family = "Inter", fontface = "italic", size = 7.6, vjust = 1.6) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25, 30), expand = expansion(add = 1.1)) +
  scale_x_continuous(breaks = seq(2010, 2024, 2), limits = c(2005.6, 2028.6),
                     expand = expansion(add = 0)) +
  labs(title = "The wind-and-solar league",
       subtitle = paste0("Rank of the 30 largest electricity producers by the share of their power that comes<br>",
                         "from wind and solar, 2010 → 2024. Only the 2024 top 15 are drawn. ",
                         "<span style='color:#34d7c2;'>**Climbers**</span> gained<br>4 places or more, ",
                         "<span style='color:#f5617a;'>**fallers**</span> lost 4 or more."),
       caption = cap, x = NULL, y = "Rank") +
  etheme() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = pal$text_sub, size = 26))
sv(p3, "03_windsolar_bump.png")

# ============================================================
# 04 — THE WORLD'S FOSSIL SHARE, YEAR BY YEAR (ridgeline)
# ============================================================
yrs <- seq(2000, 2024, by = 2)
raw_d <- map_dfr(yrs, function(y) {
  v <- mix |> filter(year == y) |> pull(ws); v <- v[!is.na(v)]
  dd <- density(v, from = 0, to = 55, bw = 1.8, n = 512)
  tibble(year = y, x = dd$x, d = dd$y)
})
H <- 7 / max(raw_d$d)          # ortak olcek: en yuksek sirt ~7 yil birimi
dens <- raw_d |> mutate(base = year, top = year + d * H, grp = factor(-year))

stat <- mix |> filter(year %in% yrs) |> group_by(year) |>
  summarise(med = median(ws, na.rm = TRUE), n = n(),
            under1 = sum(ws < 1, na.rm = TRUE), .groups = "drop")
lab3 <- stat |> filter(year %in% c(2000, 2012, 2024))

p4 <- ggplot() +
  geom_ribbon(data = dens, aes(x = x, ymin = base, ymax = top, group = grp, fill = year),
              color = pal$bg, linewidth = 1.1) +
  geom_path(data = stat, aes(med, year), color = pal$amber, linewidth = 1.6) +
  geom_point(data = stat, aes(med, year), color = pal$amber, size = 5.4) +
  geom_label(data = lab3, aes(med, year, label = sprintf("median %.1f%%", med)),
             fill = pal$bg, linewidth = 0, color = pal$amber, family = "Inter",
             fontface = "bold", size = 7.8, hjust = 0, nudge_x = 2.2,
             label.padding = unit(5, "pt")) +
  annotate("richtext", x = 23, y = 2012.5, hjust = 0, vjust = 1, fill = NA, label.color = NA,
           family = "Inter", size = 10, lineheight = 1.3, text.color = pal$text_main,
           label = paste0("**In 2000, 100 of 106 countries were below 1%**<br>",
                          "<span style='font-size:25pt;color:#8fa5c4;'>",
                          "by 2024 only 21 still are — the spike at zero melts and flows right</span>")) +
  scale_fill_gradient(low = pal$coral, high = pal$teal) +
  scale_x_continuous(labels = label_percent(scale = 1), breaks = seq(0, 50, 10),
                     limits = c(0, 55), expand = expansion(add = 0)) +
  scale_y_continuous(breaks = yrs, expand = expansion(add = c(0.6, 4.5))) +
  labs(title = "The melting of zero",
       subtitle = paste0("Every ridge is the distribution of ~105 countries by the share of their electricity<br>",
                         "that comes from <b>wind and solar</b>, one ridge per year. All ridges share one height<br>",
                         "scale, so the collapsing spike at zero is real. Amber trail = the median country."),
       caption = cap, x = "Wind + solar share of electricity", y = NULL) +
  etheme() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = pal$text_main, size = 27, face = "bold"))
sv(p4, "04_windsolar_ridgeline.png", w = 3400, h = 2700)

cat("\n--- ozet ---\n")
print(as.data.frame(mov |> arrange(r1) |> select(country, r0, r1, v1)))
