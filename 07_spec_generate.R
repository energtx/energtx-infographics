#!/usr/bin/env Rscript
# energtx.com chart generator
# ------------------------------------------------------------------
# Reads specs.json and renders each entry as a 3600x2025 .webp chart
# matching the @energtx.bsky.social visual style.
# Data source: Our World in Data energy dataset (OWID).
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(jsonlite)
  library(scales)
  library(ragg)
  library(showtext)
  library(sysfonts)
  library(ggtext)
})

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1) args[[1]] else "/home/fbartuyurdacan/Desktop/energtx_batch"
only <- if (length(args) >= 2) args[[2]] else NA_character_

data_path  <- file.path(root, "data", "owid-energy-data.csv")
specs_path <- file.path(root, "specs.json")
img_dir    <- file.path(root, "images")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

# ---------- fonts ----------
sysfonts::font_add_google("Inter", "Inter")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 150)

# ---------- palette ----------
pal <- list(
  bg        = "#0a1628",
  panel     = "#0a1628",
  grid      = "#16263f",
  text_main = "#e8f1ff",
  text_sub  = "#8aa0bf",
  teal      = "#34d7c2",
  coral     = "#f5617a",
  amber     = "#fbbf24",
  indigo    = "#818cf8",
  green     = "#4ade80",
  violet    = "#c084fc",
  orange    = "#fb923c",
  pink      = "#f472b6",
  blue      = "#60a5fa",
  lime      = "#a3e635",
  rose      = "#fb7185"
)

cat_pal <- c(pal$teal, pal$coral, pal$amber, pal$indigo, pal$green,
             pal$violet, pal$orange, pal$pink, pal$blue, pal$lime,
             pal$rose, "#94a3b8")

energtx_theme <- function(base = 34) {
  theme_minimal(base_family = "Inter", base_size = base) +
    theme(
      plot.background     = element_rect(fill = pal$bg, color = NA),
      panel.background    = element_rect(fill = pal$bg, color = NA),
      panel.grid.major    = element_line(color = pal$grid, linewidth = 0.5),
      panel.grid.minor    = element_blank(),
      plot.title          = element_markdown(color = pal$text_main, face = "bold",
                                             size = base * 2.1, margin = margin(b = 8),
                                             lineheight = 1.1),
      plot.subtitle       = element_text(color = pal$text_sub, size = base * 1.05,
                                         margin = margin(b = 22), lineheight = 1.2),
      plot.caption        = element_markdown(color = pal$text_sub, size = base * 0.78,
                                             hjust = 0, margin = margin(t = 18)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text           = element_text(color = pal$text_sub, size = base * 0.92),
      axis.title          = element_text(color = pal$text_sub, size = base * 0.95),
      axis.ticks          = element_blank(),
      strip.text          = element_text(color = pal$text_main, size = base * 0.95,
                                         face = "bold"),
      strip.background    = element_rect(fill = pal$bg, color = NA),
      legend.background   = element_rect(fill = pal$bg, color = NA),
      legend.key          = element_rect(fill = pal$bg, color = NA),
      legend.text         = element_text(color = pal$text_sub, size = base * 0.9),
      legend.title        = element_text(color = pal$text_main, size = base * 0.95),
      legend.position     = "top",
      plot.margin         = margin(34, 44, 26, 44)
    )
}

caption_txt <- "Source: Our World in Data · <span style='color:#34d7c2;'>energtx.com</span> · @energtx.bsky.social"

# ---------- data ----------
cat("loading", data_path, "\n")
owid <- readr::read_csv(data_path, show_col_types = FALSE) %>%
  filter(!is.na(iso_code), nchar(iso_code) == 3, country != "World")

get_country <- function(cs, ind, yr_from = 1990, yr_to = 2024) {
  owid %>%
    filter(country %in% cs, year >= yr_from, year <= yr_to) %>%
    select(country, year, value = all_of(ind)) %>%
    filter(!is.na(value))
}

# ---------- chart renderers ----------
render_lollipop <- function(spec) {
  cs <- spec$countries
  ind <- spec$indicator
  y1 <- spec$year_from; y2 <- spec$year_to
  d1 <- owid %>% filter(country %in% cs, year == y1) %>%
    select(country, v1 = all_of(ind))
  d2 <- owid %>% filter(country %in% cs, year == y2) %>%
    select(country, v2 = all_of(ind))
  d  <- inner_join(d1, d2, by = "country") %>% filter(!is.na(v1), !is.na(v2)) %>%
    mutate(country = fct_reorder(country, v2))
  if (nrow(d) < 2) return(NULL)
  fmt <- if (isTRUE(spec$percent)) label_percent(accuracy = 1, scale = 1) else label_comma(accuracy = 0.1)
  ggplot(d, aes(y = country)) +
    geom_segment(aes(x = v1, xend = v2, yend = country),
                 color = pal$grid, linewidth = 3) +
    geom_point(aes(x = v1), color = pal$coral, size = 9) +
    geom_point(aes(x = v2), color = pal$teal,  size = 9) +
    geom_text(aes(x = v1, label = fmt(v1)), color = pal$coral,
              vjust = -1.6, size = 9, family = "Inter") +
    geom_text(aes(x = v2, label = fmt(v2)), color = pal$teal,
              vjust = -1.6, size = 9, family = "Inter") +
    scale_x_continuous(labels = fmt) +
    labs(title = spec$title, subtitle = spec$subtitle,
         x = NULL, y = NULL, caption = caption_txt) +
    energtx_theme()
}

render_hbar <- function(spec) {
  ind <- spec$indicator
  top <- if (!is.null(spec$top_n)) spec$top_n else 15
  yr  <- spec$year
  cs  <- if (!is.null(spec$countries) && length(spec$countries) > 0) spec$countries else NULL
  d <- owid %>% filter(year == yr) %>% select(country, value = all_of(ind)) %>%
    filter(!is.na(value))
  if (!is.null(cs)) d <- d %>% filter(country %in% cs)
  d <- d %>% arrange(desc(value)) %>% head(top) %>%
    mutate(country = fct_reorder(country, value))
  if (nrow(d) < 2) return(NULL)
  fmt <- if (isTRUE(spec$percent)) label_percent(accuracy = 1, scale = 1) else label_comma(accuracy = 0.1)
  ggplot(d, aes(x = value, y = country)) +
    geom_col(fill = pal$teal, width = 0.7) +
    geom_text(aes(label = fmt(value)), hjust = -0.15,
              color = pal$text_main, size = 9, family = "Inter") +
    scale_x_continuous(labels = fmt,
                       expand = expansion(mult = c(0, 0.18))) +
    labs(title = spec$title, subtitle = spec$subtitle,
         x = NULL, y = NULL, caption = caption_txt) +
    energtx_theme()
}

render_line <- function(spec) {
  ind <- spec$indicator
  d <- get_country(spec$countries, ind, spec$year_from, spec$year_to)
  if (nrow(d) < 6) return(NULL)
  fmt <- if (isTRUE(spec$percent)) label_percent(accuracy = 1, scale = 1) else label_comma(accuracy = 0.1)
  last <- d %>% group_by(country) %>% filter(year == max(year)) %>% ungroup()
  ggplot(d, aes(year, value, color = country, group = country)) +
    geom_line(linewidth = 1.6) +
    geom_point(data = last, size = 5) +
    geom_text(data = last, aes(label = country),
              hjust = -0.1, size = 9, family = "Inter",
              show.legend = FALSE) +
    scale_color_manual(values = cat_pal) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.18))) +
    scale_y_continuous(labels = fmt) +
    guides(color = "none") +
    labs(title = spec$title, subtitle = spec$subtitle,
         x = NULL, y = NULL, caption = caption_txt) +
    energtx_theme()
}

render_facet_area <- function(spec) {
  cs <- spec$countries
  shares <- c("coal_share_energy","gas_share_energy","oil_share_energy",
              "nuclear_share_energy","hydro_share_energy",
              "solar_share_energy","wind_share_energy","biofuel_share_energy")
  d <- owid %>% filter(country %in% cs,
                       year >= spec$year_from, year <= spec$year_to) %>%
    select(country, year, all_of(shares)) %>%
    pivot_longer(-c(country, year), names_to = "source", values_to = "share") %>%
    mutate(source = gsub("_share_energy","", source),
           source = factor(source,
                           levels = c("coal","oil","gas","nuclear",
                                      "hydro","solar","wind","biofuel"))) %>%
    filter(!is.na(share))
  if (nrow(d) < 20) return(NULL)
  fills <- c(coal = "#64748b", oil = "#94a3b8", gas = pal$orange,
             nuclear = pal$violet, hydro = pal$blue,
             solar = pal$amber, wind = pal$teal, biofuel = pal$green)
  ggplot(d, aes(year, share, fill = source)) +
    geom_area(position = "stack") +
    facet_wrap(~country, ncol = min(4, length(cs))) +
    scale_fill_manual(values = fills, name = NULL) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(title = spec$title, subtitle = spec$subtitle,
         x = NULL, y = NULL, caption = caption_txt) +
    energtx_theme()
}

render_heatmap <- function(spec) {
  cs <- spec$countries; ind <- spec$indicator
  d <- get_country(cs, ind, spec$year_from, spec$year_to) %>%
    mutate(country = factor(country, levels = rev(cs)))
  if (nrow(d) < 10) return(NULL)
  ggplot(d, aes(year, country, fill = value)) +
    geom_tile(color = pal$bg, linewidth = 0.4) +
    scale_fill_gradientn(colors = c(pal$bg, "#13324a", pal$teal, pal$amber, pal$coral),
                         na.value = pal$grid,
                         labels = if (isTRUE(spec$percent)) label_percent(scale = 1) else label_comma()) +
    labs(title = spec$title, subtitle = spec$subtitle,
         x = NULL, y = NULL, fill = NULL, caption = caption_txt) +
    energtx_theme() +
    theme(panel.grid = element_blank())
}

render_slope <- function(spec) {
  cs <- spec$countries; ind <- spec$indicator
  y1 <- spec$year_from; y2 <- spec$year_to
  d1 <- owid %>% filter(country %in% cs, year == y1) %>%
    select(country, value = all_of(ind)) %>% mutate(year = y1)
  d2 <- owid %>% filter(country %in% cs, year == y2) %>%
    select(country, value = all_of(ind)) %>% mutate(year = y2)
  d <- bind_rows(d1, d2) %>% filter(!is.na(value))
  if (length(unique(d$country)) < 2) return(NULL)
  fmt <- if (isTRUE(spec$percent)) label_percent(accuracy = 1, scale = 1) else label_comma(accuracy = 0.1)
  labs_r <- d %>% filter(year == y2)
  labs_l <- d %>% filter(year == y1)
  ggplot(d, aes(year, value, group = country, color = country)) +
    geom_line(linewidth = 1.6) +
    geom_point(size = 5) +
    geom_text(data = labs_r, aes(label = paste0(country, "  ", fmt(value))),
              hjust = -0.08, size = 9, family = "Inter", show.legend = FALSE) +
    geom_text(data = labs_l, aes(label = fmt(value)),
              hjust = 1.2, size = 9, family = "Inter", show.legend = FALSE) +
    scale_color_manual(values = cat_pal) +
    scale_x_continuous(breaks = c(y1, y2),
                       expand = expansion(mult = c(0.2, 0.45))) +
    scale_y_continuous(labels = fmt) +
    guides(color = "none") +
    labs(title = spec$title, subtitle = spec$subtitle,
         x = NULL, y = NULL, caption = caption_txt) +
    energtx_theme()
}

render_stacked_area_world <- function(spec) {
  cs <- spec$countries
  ind_set <- spec$indicators   # named list
  d <- owid %>% filter(country %in% cs,
                       year >= spec$year_from, year <= spec$year_to) %>%
    group_by(year) %>%
    summarise(across(all_of(unlist(ind_set)), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-year, names_to = "source", values_to = "value")
  if (nrow(d) < 20) return(NULL)
  d$source <- factor(d$source, levels = unlist(ind_set))
  labels <- setNames(names(ind_set), unlist(ind_set))
  ggplot(d, aes(year, value, fill = source)) +
    geom_area() +
    scale_fill_manual(values = cat_pal, labels = labels, name = NULL) +
    scale_y_continuous(labels = label_comma()) +
    labs(title = spec$title, subtitle = spec$subtitle,
         x = NULL, y = NULL, caption = caption_txt) +
    energtx_theme()
}

render_scatter <- function(spec) {
  dx <- owid %>% filter(year == spec$year) %>%
    select(country, x = all_of(spec$x), y = all_of(spec$y),
           size = all_of(spec$size)) %>%
    filter(!is.na(x), !is.na(y), !is.na(size))
  if (nrow(dx) < 8) return(NULL)
  if (!is.null(spec$countries) && length(spec$countries) > 0)
    dx <- dx %>% filter(country %in% spec$countries)
  ggplot(dx, aes(x, y, size = size)) +
    geom_point(color = pal$teal, alpha = 0.75) +
    ggrepel::geom_text_repel(aes(label = country), color = pal$text_main,
                             size = 8, family = "Inter",
                             max.overlaps = 30, segment.color = pal$grid) +
    scale_size(range = c(4, 28), guide = "none") +
    scale_x_continuous(labels = if (isTRUE(spec$x_percent)) label_percent(scale = 1) else label_comma()) +
    scale_y_continuous(labels = if (isTRUE(spec$y_percent)) label_percent(scale = 1) else label_comma()) +
    labs(title = spec$title, subtitle = spec$subtitle,
         x = spec$x_label, y = spec$y_label, caption = caption_txt) +
    energtx_theme()
}

render_diverging <- function(spec) {
  ind <- spec$indicator
  y1 <- spec$year_from; y2 <- spec$year_to
  d1 <- owid %>% filter(year == y1) %>% select(country, v1 = all_of(ind))
  d2 <- owid %>% filter(year == y2) %>% select(country, v2 = all_of(ind))
  d  <- inner_join(d1, d2, by = "country") %>%
    mutate(delta = v2 - v1) %>% filter(!is.na(delta))
  if (!is.null(spec$countries) && length(spec$countries) > 0)
    d <- d %>% filter(country %in% spec$countries)
  top <- if (!is.null(spec$top_n)) spec$top_n else 18
  d <- d %>% arrange(desc(abs(delta))) %>% head(top) %>%
    mutate(country = fct_reorder(country, delta),
           pos = delta >= 0)
  if (nrow(d) < 3) return(NULL)
  fmt <- if (isTRUE(spec$percent)) label_percent(accuracy = 1, scale = 1,
                                                 style_positive = "plus") else label_comma(accuracy = 0.1,
                                                                                         style_positive = "plus")
  ggplot(d, aes(delta, country, fill = pos)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = fmt(delta), hjust = ifelse(pos, -0.1, 1.1)),
              color = pal$text_main, size = 9, family = "Inter") +
    scale_fill_manual(values = c(`FALSE` = pal$coral, `TRUE` = pal$teal),
                      guide = "none") +
    scale_x_continuous(labels = fmt,
                       expand = expansion(mult = c(0.22, 0.22))) +
    labs(title = spec$title, subtitle = spec$subtitle,
         x = NULL, y = NULL, caption = caption_txt) +
    energtx_theme()
}

renderers <- list(
  lollipop      = render_lollipop,
  hbar          = render_hbar,
  line          = render_line,
  facet_area    = render_facet_area,
  heatmap       = render_heatmap,
  slope         = render_slope,
  stacked_area  = render_stacked_area_world,
  scatter       = render_scatter,
  diverging     = render_diverging
)

# ---------- driver ----------
specs <- fromJSON(specs_path, simplifyVector = FALSE)
cat("specs:", length(specs), "\n")

ok <- 0; fail <- 0
for (i in seq_along(specs)) {
  sp <- specs[[i]]
  if (!is.na(only) && sp$id != only) next
  fn <- renderers[[sp$type]]
  if (is.null(fn)) { cat("  skip [", sp$id, "] unknown type", sp$type, "\n"); fail <- fail + 1; next }
  p <- tryCatch(fn(sp), error = function(e) { cat("  err [", sp$id, "]: ", conditionMessage(e), "\n"); NULL })
  if (is.null(p)) { fail <- fail + 1; next }
  out <- file.path(img_dir, sprintf("%s.webp", sp$id))
  tryCatch({
    agg_webp(out, width = 3600, height = 2025, units = "px",
             background = pal$bg, res = 150, quality = 88)
    print(p)
    dev.off()
    ok <- ok + 1
    if (ok %% 10 == 0) cat("  rendered", ok, "\n")
  }, error = function(e) {
    cat("  write err [", sp$id, "]:", conditionMessage(e), "\n"); fail <<- fail + 1
  })
}
cat("done. ok=", ok, " fail=", fail, "\n")
