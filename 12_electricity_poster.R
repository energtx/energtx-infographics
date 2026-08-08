#!/usr/bin/env Rscript
# energtx — electricity poster: "Watch the world's grids turn green" (192)
# Heatmap 48 countries x 25 years, carbon intensity of electricity.
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(forcats)
  library(scales); library(ragg); library(showtext); library(sysfonts); library(ggtext)
})
root <- "/home/fbartuyurdacan/energtx-infographics"; setwd(root)
sysfonts::font_add_google("Inter", "Inter"); showtext::showtext_auto(); showtext::showtext_opts(dpi = 150)

pal <- list(bg = "#0a1628", grid = "#16263f", text_main = "#e8f1ff", text_sub = "#8aa0bf")
MID <- 471  # dunya ortalamasi, 2024 (OWID)

raw <- readr::read_csv("owid-energy-data.csv", show_col_types = FALSE)
owid <- raw |> filter(!is.na(iso_code), nchar(iso_code) == 3)

cc <- owid |> filter(year == 2023, !is.na(electricity_generation)) |>
  slice_max(electricity_generation, n = 48) |> pull(country)

d <- owid |> filter(country %in% cc, year >= 2000, year <= 2024, !is.na(carbon_intensity_elec)) |>
  transmute(country = recode(country, "Turkey" = "Türkiye",
                             "United Arab Emirates" = "UAE",
                             "United States" = "United States"),
            year, v = carbon_intensity_elec)

ord <- d |> filter(year == 2024) |> arrange(desc(v)) |> pull(country)
d <- d |> mutate(country = factor(country, levels = ord))   # en temiz USTTE (desc -> son level ustte)
lab24 <- d |> filter(year == 2024)

heat_cols <- c("#0d9488", "#2dd4bf", "#55627a", "#f5617a", "#c2455f")
heat_vals <- rescale(c(0, 200, MID, 800, 1150), to = c(0, 1))

p <- ggplot(d, aes(year, country, fill = v)) +
  geom_tile(color = pal$bg, linewidth = 0.8) +
  geom_text(data = lab24, aes(x = 2026.4, label = round(v), color = v),
            size = 6.8, family = "Inter", fontface = "bold", hjust = 1) +
  scale_fill_gradientn(colors = heat_cols, values = heat_vals, limits = c(0, 1150), oob = scales::squish,
                       name = "gCO2 per kWh",
                       breaks = c(0, 250, MID, 750, 1000),
                       labels = c("0", "250", paste0(MID, "\nworld avg"), "750", "1000")) +
  scale_color_gradientn(colors = heat_cols, values = heat_vals, limits = c(0, 1150), oob = scales::squish, guide = "none") +
  scale_x_continuous(breaks = seq(2000, 2020, 5), expand = expansion(add = c(0.3, 0)),
                     limits = c(1999.4, 2026.6)) +
  annotate("text", x = 2026.4, y = 49.2, label = "2024", color = pal$text_sub,
           size = 7.5, family = "Inter", fontface = "bold.italic", hjust = 1) +
  scale_y_discrete(expand = expansion(add = c(0.6, 2))) +
  labs(title = paste0(
         "<span style='font-size:60pt;'>**Watch the world's grids turn green**</span><br><br>",
         "<span style='font-size:22.5pt;color:#8aa0bf;'>Carbon intensity of electricity generation, 2000 → 2024, ",
         "in the 48 largest electricity producers.<br>",
         "<span style='color:#2dd4bf;'>**Teal**</span> is cleaner and ",
         "<span style='color:#f5617a;'>**coral**</span> is dirtier than today's world average of ",
         MID, " gCO2 per kWh. Cleanest grids at the top.</span>"),
       caption = "Data: Our World in Data energy dataset (Ember) · Chart: R + ggplot2 · energtx.com",
       x = NULL, y = NULL) +
  theme_minimal(base_family = "Inter", base_size = 26) +
  theme(plot.background = element_rect(fill = pal$bg, color = NA),
        panel.background = element_rect(fill = pal$bg, color = NA),
        panel.grid = element_blank(),
        plot.title = element_markdown(color = pal$text_main, lineheight = 1.25,
                                      margin = margin(t = 8, b = 24)),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(color = pal$text_sub, size = 21, hjust = 0,
                                    margin = margin(t = 16)),
        axis.text.x = element_text(color = pal$text_sub, size = 24),
        axis.text.y = element_text(color = pal$text_main, size = 23),
        axis.ticks = element_blank(),
        legend.position = "bottom", legend.justification = "left",
        legend.title = element_text(color = pal$text_sub, size = 24),
        legend.text = element_text(color = pal$text_sub, size = 22, lineheight = 0.9),
        legend.key.width = unit(90, "pt"), legend.key.height = unit(12, "pt"),
        plot.margin = margin(40, 48, 30, 48))

ggsave(file.path(root, "png", "192_grid_carbon_heatmap_poster.png"), p,
       device = ragg::agg_png, width = 2600, height = 3600, units = "px", dpi = 150)
cat("yazildi: 192_grid_carbon_heatmap_poster.png\n")
