#!/usr/bin/env Rscript
# energtx batch 8 — charts 193-195 (2026-08-10) — electricity-focused premium set
# 193 waterfall   : where the world's new electricity came from, 2014 -> 2024
# 194 arrow plot  : coal's share of electricity, 2010 -> 2024, largest producers
# 195 stacked area: forty years of the world's power mix, 1985 -> 2024
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(forcats)
  library(scales); library(ragg); library(showtext); library(sysfonts); library(ggtext)
})
root <- "/home/fbartuyurdacan/energtx-infographics"; setwd(root)
sysfonts::font_add_google("Inter", "Inter"); showtext::showtext_auto(); showtext::showtext_opts(dpi = 150)

pal <- list(bg = "#0a1628", grid = "#16263f", text_main = "#e8f1ff", text_sub = "#8aa0bf",
            teal = "#34d7c2", coral = "#f5617a", amber = "#fbbf24", violet = "#c084fc",
            green = "#4ade80", orange = "#fb923c", blue = "#60a5fa", slate = "#55627a")

energtx_theme <- function(base = 34) {
  theme_minimal(base_family = "Inter", base_size = base) +
    theme(plot.background = element_rect(fill = pal$bg, color = NA),
          panel.background = element_rect(fill = pal$bg, color = NA),
          panel.grid.major = element_line(color = pal$grid, linewidth = 0.5),
          panel.grid.minor = element_blank(),
          plot.title = element_markdown(color = pal$text_main, face = "bold",
                                        size = base * 2.05, margin = margin(b = 10), lineheight = 1.1),
          plot.subtitle = element_markdown(color = pal$text_sub, size = base * 0.92,
                                           margin = margin(b = 20), lineheight = 1.35),
          plot.caption = element_markdown(color = pal$text_sub, size = base * 0.74,
                                          hjust = 0, margin = margin(t = 16)),
          plot.title.position = "plot", plot.caption.position = "plot",
          axis.text = element_text(color = pal$text_sub, size = base * 0.88),
          axis.title = element_text(color = pal$text_sub, size = base * 0.88),
          axis.ticks = element_blank(), legend.position = "none",
          plot.margin = margin(34, 44, 26, 44))
}
caption_txt <- "Source: Our World in Data energy dataset (Ember, Energy Institute) · <span style='color:#34d7c2;'>energtx.com</span> · @energtx.bsky.social"
save_png <- function(p, file, w = 3600, h = 2025) {
  ggsave(file.path(root, "png", file), p, device = ragg::agg_png,
         width = w, height = h, units = "px", dpi = 150)
  cat("yazildi:", file, "\n")
}

raw  <- readr::read_csv("owid-energy-data.csv", show_col_types = FALSE)
owid <- raw |> filter(!is.na(iso_code), nchar(iso_code) == 3)

# ============================================================
# 193 — Where the world's new electricity came from (waterfall)
# ============================================================
Y0 <- 2014; Y1 <- 2024
wide <- raw |> filter(country == "World", year %in% c(Y0, Y1)) |>
  select(year, Solar = solar_electricity, Wind = wind_electricity, Hydro = hydro_electricity,
         Other = other_renewable_electricity, Nuclear = nuclear_electricity,
         Gas = gas_electricity, Coal = coal_electricity, Oil = oil_electricity,
         Total = electricity_generation) |>
  pivot_longer(-year) |>
  pivot_wider(names_from = year, values_from = value, names_prefix = "y") |>
  rename(v0 = !!paste0("y", Y0), v1 = !!paste0("y", Y1)) |>
  mutate(delta = v1 - v0, pct = 100 * delta / v0)

tot0 <- wide$v0[wide$name == "Total"]; tot1 <- wide$v1[wide$name == "Total"]
ord  <- c("Solar", "Wind", "Hydro", "Other", "Nuclear", "Gas", "Coal", "Oil")
lev  <- c(as.character(Y0), ord, as.character(Y1))

steps <- wide |> filter(name %in% ord) |> mutate(name = factor(name, ord)) |> arrange(name) |>
  mutate(end = tot0 + cumsum(delta), start = end - delta,
         kind = if_else(name %in% c("Solar", "Wind", "Hydro", "Other", "Nuclear"), "clean", "fossil"))

bars <- bind_rows(
  tibble(name = as.character(Y0), start = 0, end = tot0, delta = tot0, pct = NA_real_, kind = "total"),
  steps |> transmute(name = as.character(name), start, end, delta, pct, kind),
  tibble(name = as.character(Y1), start = 0, end = tot1, delta = tot1, pct = NA_real_, kind = "total")
) |> mutate(xi = match(name, lev),
            ytop = pmax(start, end), ybot = pmin(start, end))

link <- bars |> arrange(xi) |> mutate(x1 = xi + 0.42, x2 = lead(xi) - 0.42, yv = end) |> filter(!is.na(x2))
lab  <- bars |> filter(kind != "total") |>
  mutate(t_abs = paste0(if_else(delta > 0, "+", "−"), comma(round(abs(delta)))),
         t_pct = paste0(if_else(delta > 0, "+", "−"), round(abs(pct)), "%"))

ws_sum <- sum(steps$delta[steps$name %in% c("Solar", "Wind")])
ws_pct <- 100 * ws_sum / (tot1 - tot0)

p193 <- ggplot(bars, aes(x = xi)) +
  geom_segment(data = link, aes(x = x1, xend = x2, y = yv, yend = yv),
               color = pal$slate, linewidth = 0.8, linetype = "22", inherit.aes = FALSE) +
  geom_rect(aes(xmin = xi - 0.42, xmax = xi + 0.42, ymin = start, ymax = end, fill = kind)) +
  geom_text(data = lab, aes(x = xi, y = ytop + 1150, label = t_abs, color = kind),
            family = "Inter", fontface = "bold", size = 10) +
  geom_text(data = lab, aes(x = xi, y = ybot - 1150, label = t_pct), color = pal$text_sub,
            family = "Inter", size = 8) +
  geom_text(data = bars |> filter(kind == "total"), aes(x = xi, y = end / 2, label = comma(round(end))),
            color = pal$text_main, family = "Inter", fontface = "bold", size = 11, angle = 90) +
  # wind + solar bracket, placed in the empty space under the first two steps
  annotate("segment", x = 1.58, xend = 3.42, y = 20300, yend = 20300, color = pal$teal, linewidth = 1.3) +
  annotate("segment", x = 1.58, xend = 1.58, y = 20300, yend = 21100, color = pal$teal, linewidth = 1.3) +
  annotate("segment", x = 3.42, xend = 3.42, y = 20300, yend = 21100, color = pal$teal, linewidth = 1.3) +
  annotate("richtext", x = 2.85, y = 19500, vjust = 1,
           label = paste0("**Wind + solar: ", comma(round(ws_sum)), " TWh added**<br>",
                          "<span style='font-size:30pt;'>", round(ws_pct),
                          "% of everything the world<br>added in a decade</span>"),
           fill = NA, label.color = NA, family = "Inter", size = 11,
           lineheight = 1.35, text.color = pal$teal) +
  scale_fill_manual(values = c(total = pal$slate, clean = pal$teal, fossil = pal$coral)) +
  scale_color_manual(values = c(clean = pal$teal, fossil = pal$coral)) +
  scale_x_continuous(breaks = seq_along(lev), labels = lev, expand = expansion(add = 0.55)) +
  scale_y_continuous(labels = comma, breaks = seq(0, 30000, 5000),
                     expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim = c(0, 34200)) +
  labs(title = "Half the world's new electricity is wind and sun",
       subtitle = paste0("Change in world electricity generation by source, ", Y0, " → ", Y1,
                         ", in terawatt-hours.<br>Output grew 30%, from ", comma(round(tot0)), " to ",
                         comma(round(tot1)), " TWh. <span style='color:#34d7c2;'>**Teal**</span> = low-carbon, ",
                         "<span style='color:#f5617a;'>**coral**</span> = fossil fuels."),
       caption = caption_txt, x = NULL, y = "Electricity generation (TWh)") +
  energtx_theme() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(color = pal$text_main, size = 27, face = "bold"))
save_png(p193, "193_new_electricity_waterfall.png")

# ============================================================
# 194 — Coal's retreat and its holdouts (arrow plot)
# ============================================================
top30 <- owid |> filter(year == 2024, !is.na(electricity_generation)) |>
  slice_max(electricity_generation, n = 30) |> pull(country)

co <- owid |> filter(country %in% top30, year %in% c(2010, 2024)) |>
  select(country, year, s = coal_share_elec) |>
  pivot_wider(names_from = year, values_from = s, names_prefix = "y") |>
  filter(!is.na(y2010), !is.na(y2024), pmax(y2010, y2024) >= 1) |>   # coal-kullanan ulkeler
  mutate(country = recode(country, "Turkey" = "Türkiye"),
         chg = y2024 - y2010,
         dir = if_else(chg < 0, "down", "up"),
         country = fct_reorder(country, y2024))

n_down <- sum(co$dir == "down"); n_up <- sum(co$dir == "up"); n_all <- nrow(co)

p194 <- ggplot(co, aes(y = country)) +
  geom_segment(aes(x = y2010, xend = y2024, yend = country, color = dir),
               linewidth = 3.6, lineend = "butt",
               arrow = arrow(length = unit(14, "pt"), type = "closed")) +
  geom_point(aes(x = y2010), color = pal$slate, size = 6) +
  geom_text(aes(x = 104, label = paste0(round(y2024), "%"), color = dir),
            hjust = 0, family = "Inter", fontface = "bold", size = 9) +
  annotate("text", x = 104, y = n_all + 0.85, label = "2024", color = pal$text_sub,
           family = "Inter", fontface = "bold.italic", size = 8.5, hjust = 0) +
  annotate("richtext", x = 26, y = 4.4, hjust = 0, vjust = 0.5,
           label = paste0("**Vietnam +33 pts, Indonesia +23, Pakistan +14**<br>",
                          "<span style='font-size:26pt;'>coal is still gaining ground in Asia</span>"),
           fill = NA, label.color = NA, family = "Inter", size = 9,
           lineheight = 1.35, text.color = pal$coral) +
  annotate("richtext", x = 1.5, y = 23.0, hjust = 0, vjust = 0.5,
           label = paste0("**The UK went from 28% to 0.7%**<br>",
                          "<span style='font-size:26pt;'>Poland −33 pts, the US −30</span>"),
           fill = NA, label.color = NA, family = "Inter", size = 9,
           lineheight = 1.35, text.color = pal$teal) +
  scale_color_manual(values = c(down = pal$teal, up = pal$coral)) +
  scale_x_continuous(labels = label_percent(scale = 1), breaks = seq(0, 100, 20),
                     limits = c(-1, 116), expand = expansion(add = c(1, 0))) +
  scale_y_discrete(expand = expansion(add = c(0.7, 1.6))) +
  labs(title = "Coal's retreat — and its holdouts",
       subtitle = paste0("Coal's share of electricity generation, 2010 → 2024.<br>",
                         "The ", n_all, " coal-using countries among the world's 30 largest electricity producers.<br>",
                         "Grey dot = 2010, arrow head = 2024. <span style='color:#34d7c2;'>**Teal**</span> = falling (",
                         n_down, "), <span style='color:#f5617a;'>**coral**</span> = rising (", n_up, ")."),
       caption = caption_txt, x = "Share of electricity generated from coal", y = NULL) +
  energtx_theme() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = pal$text_main, size = 25))
save_png(p194, "194_coal_retreat_arrows.png")

# ============================================================
# 195 — Forty years of the world's power mix (stacked area)
# ============================================================
mix_ord <- c("Coal", "Gas", "Oil", "Nuclear", "Hydro", "Other renew.", "Wind", "Solar")
mix_col <- c(Coal = "#c2455f", Gas = pal$orange, Oil = "#a97452", Nuclear = pal$violet,
             Hydro = pal$blue, `Other renew.` = pal$green, Wind = pal$teal, Solar = pal$amber)

mix <- raw |> filter(country == "World", year >= 1985, year <= 2024) |>
  select(year, Coal = coal_electricity, Gas = gas_electricity, Oil = oil_electricity,
         Nuclear = nuclear_electricity, Hydro = hydro_electricity,
         `Other renew.` = other_renewable_electricity, Wind = wind_electricity,
         Solar = solar_electricity) |>
  pivot_longer(-year, names_to = "src", values_to = "twh") |>
  mutate(src = factor(src, levels = mix_ord))

# position_stack(reverse = TRUE) -> seviye sirasi ALTTAN yukari (Coal en altta)
end_lab <- mix |> filter(year == 2024) |> arrange(src) |>
  mutate(cum = cumsum(twh), ypos = cum - twh / 2,
         ypos = ypos + c(0, 0, 350, 0, 0, -250, 250, 0),   # ince bantlar icin elle ayirma
         txt = paste0(src, "  ", comma(round(twh))))

tot24 <- sum(mix$twh[mix$year == 2024]); tot85 <- sum(mix$twh[mix$year == 1985])
ws24  <- sum(mix$twh[mix$year == 2024 & mix$src %in% c("Wind", "Solar")])
ws85  <- sum(mix$twh[mix$year == 1985 & mix$src %in% c("Wind", "Solar")])

p195 <- ggplot(mix, aes(year, twh, fill = src)) +
  geom_area(position = position_stack(reverse = TRUE), color = pal$bg, linewidth = 0.45) +
  geom_text(data = end_lab, aes(x = 2024.7, y = ypos, label = txt, color = src),
            hjust = 0, family = "Inter", fontface = "bold", size = 8, inherit.aes = FALSE) +
  annotate("segment", x = 2002.2, xend = 2022.8, y = 28350, yend = tot24 - ws24 / 2,
           color = pal$amber, linewidth = 1.1) +
  annotate("richtext", x = 1985.6, y = 30400, hjust = 0, vjust = 1,
           label = paste0("**Wind + solar, 2024: ", comma(round(ws24)), " TWh**<br>",
                          "<span style='font-size:29pt;'>", round(100 * ws24 / tot24, 1),
                          "% of world electricity. In 1985 the two<br>together made ",
                          round(ws85, 2), " TWh — a rounding error.</span>"),
           fill = NA, label.color = NA, family = "Inter", size = 10,
           lineheight = 1.35, text.color = pal$amber) +
  scale_fill_manual(values = mix_col) +
  scale_color_manual(values = mix_col) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), limits = c(1985, 2032),
                     expand = expansion(add = c(0.4, 0))) +
  scale_y_continuous(labels = comma, breaks = seq(0, 30000, 5000),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(title = "Forty years of the world's power mix",
       subtitle = paste0("Global electricity generation by source, 1985 → 2024, in terawatt-hours.<br>",
                         "The world now makes ", comma(round(tot24)), " TWh a year — ",
                         round(tot24 / tot85, 1), "× the 1985 total. Coal is still the thickest band,<br>",
                         "but the <span style='color:#fbbf24;'>**solar**</span> and ",
                         "<span style='color:#34d7c2;'>**wind**</span> wedges on top did not exist a generation ago."),
       caption = caption_txt, x = NULL, y = "Electricity generation (TWh)") +
  energtx_theme() +
  theme(panel.grid.major.x = element_blank())
save_png(p195, "195_world_power_mix_40y.png")

cat("\n--- ozet ---\n")
cat("193: toplam artis", comma(round(tot1 - tot0)), "TWh; wind+solar", comma(round(ws_sum)),
    "TWh =", round(ws_pct, 1), "%\n")
cat("194:", n_all, "ulke; dusen", n_down, "/ artan", n_up, "\n")
cat("195: 2024 wind+solar", comma(round(ws24)), "TWh =", round(100 * ws24 / tot24, 1),
    "%; 1985 =", round(ws85, 3), "TWh; toplam carpan", round(tot24 / tot85, 2), "\n")
