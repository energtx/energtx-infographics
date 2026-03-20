# ============================================================
# energtx Infographic Generator — Batch 3 (101–155)
# New chart types: ridgeline, donut, slope, diverging, faceted
# Broader audience hashtags
# ============================================================

source("C:/Users/bartu/Desktop/energtx-infographics/01_data.R")
library(ggridges)
library(ggforce)

g20 <- c("United States", "China", "India", "Japan", "Germany", "United Kingdom",
        "France", "Brazil", "Canada", "South Korea", "Russia", "Australia",
        "Italy", "Mexico", "Indonesia", "Turkey", "Saudi Arabia", "Argentina",
        "South Africa")

eu <- c("Germany", "France", "Italy", "Spain", "Netherlands", "Belgium",
       "Poland", "Sweden", "Denmark", "Finland", "Austria", "Portugal",
       "Czech Republic", "Romania", "Greece", "Ireland", "Hungary")


# ============================================================
# 101 — Donut: World Energy Mix
# ============================================================
p101_data <- raw %>%
  filter(country == "World", year == latest_year) %>%
  select(Coal = coal_share_energy, Oil = oil_share_energy,
         Gas = gas_share_energy, Nuclear = nuclear_share_energy,
         Hydro = hydro_share_energy, Wind = wind_share_energy,
         Solar = solar_share_energy) %>%
  pivot_longer(everything(), names_to = "source", values_to = "share") %>%
  filter(!is.na(share), share > 0) %>%
  mutate(source = factor(source, levels = c("Oil","Gas","Coal","Nuclear","Hydro","Wind","Solar")),
         ymax = cumsum(share), ymin = lag(ymax, default = 0),
         mid = (ymin + ymax) / 2,
         label = paste0(source, "\n", round(share, 1), "%"))

p101 <- ggplot(p101_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = source)) +
  geom_rect(colour = etx_bg) +
  geom_text(aes(x = 3.5, y = mid, label = label), colour = etx_text, size = 3, family = "inter") +
  coord_polar(theta = "y") +
  xlim(c(1, 4.5)) +
  scale_fill_manual(values = c("Coal" = etx_coal, "Oil" = "#92400E", "Gas" = etx_gas,
                                "Nuclear" = etx_nuclear, "Hydro" = etx_hydro,
                                "Wind" = etx_wind, "Solar" = etx_solar)) +
  annotate("text", x = 1, y = 0, label = glue("WORLD\n{latest_year}"),
           colour = etx_accent, family = "spacegrotesk", fontface = "bold", size = 5) +
  labs(title = "What Powers the World?",
       subtitle = glue("Global primary energy mix · {latest_year}"),
       caption = etx_caption()) +
  theme_energtx() +
  theme(axis.text = element_blank(), panel.grid = element_blank())
etx_save(p101, file.path(out_dir, "101_world_donut.png"))


# ============================================================
# 102 — Donut: China Energy Mix
# ============================================================
p102_data <- latest %>%
  filter(country == "China") %>%
  select(Coal = coal_share_energy, Oil = oil_share_energy,
         Gas = gas_share_energy, Nuclear = nuclear_share_energy,
         Hydro = hydro_share_energy, Wind = wind_share_energy,
         Solar = solar_share_energy) %>%
  pivot_longer(everything(), names_to = "source", values_to = "share") %>%
  filter(!is.na(share), share > 0) %>%
  mutate(source = factor(source, levels = c("Coal","Oil","Gas","Nuclear","Hydro","Wind","Solar")),
         ymax = cumsum(share), ymin = lag(ymax, default = 0),
         mid = (ymin + ymax) / 2,
         label = paste0(source, "\n", round(share, 1), "%"))

p102 <- ggplot(p102_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = source)) +
  geom_rect(colour = etx_bg) +
  geom_text(aes(x = 3.5, y = mid, label = label), colour = etx_text, size = 3, family = "inter") +
  coord_polar(theta = "y") + xlim(c(1, 4.5)) +
  scale_fill_manual(values = fuel_colors) +
  annotate("text", x = 1, y = 0, label = glue("CHINA\n{latest_year}"),
           colour = etx_accent4, family = "spacegrotesk", fontface = "bold", size = 5) +
  labs(title = "What Powers China?",
       subtitle = glue("China primary energy mix · {latest_year}"),
       caption = etx_caption()) +
  theme_energtx() +
  theme(axis.text = element_blank(), panel.grid = element_blank())
etx_save(p102, file.path(out_dir, "102_china_donut.png"))


# ============================================================
# 103 — Donut: India Energy Mix
# ============================================================
p103_data <- latest %>%
  filter(country == "India") %>%
  select(Coal = coal_share_energy, Oil = oil_share_energy,
         Gas = gas_share_energy, Nuclear = nuclear_share_energy,
         Hydro = hydro_share_energy, Wind = wind_share_energy,
         Solar = solar_share_energy) %>%
  pivot_longer(everything(), names_to = "source", values_to = "share") %>%
  filter(!is.na(share), share > 0) %>%
  mutate(source = factor(source, levels = c("Coal","Oil","Gas","Nuclear","Hydro","Wind","Solar")),
         ymax = cumsum(share), ymin = lag(ymax, default = 0),
         mid = (ymin + ymax) / 2,
         label = paste0(source, "\n", round(share, 1), "%"))

p103 <- ggplot(p103_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = source)) +
  geom_rect(colour = etx_bg) +
  geom_text(aes(x = 3.5, y = mid, label = label), colour = etx_text, size = 3, family = "inter") +
  coord_polar(theta = "y") + xlim(c(1, 4.5)) +
  scale_fill_manual(values = fuel_colors) +
  annotate("text", x = 1, y = 0, label = glue("INDIA\n{latest_year}"),
           colour = etx_accent3, family = "spacegrotesk", fontface = "bold", size = 5) +
  labs(title = "What Powers India?",
       subtitle = glue("India primary energy mix · {latest_year}"),
       caption = etx_caption()) +
  theme_energtx() +
  theme(axis.text = element_blank(), panel.grid = element_blank())
etx_save(p103, file.path(out_dir, "103_india_donut.png"))


# ============================================================
# 104 — Ridgeline: Renewable Share Distribution by Continent
# ============================================================
p104_data <- latest %>%
  filter(!is.na(renewables_share_energy), !is.na(continent)) %>%
  mutate(continent = fct_reorder(continent, renewables_share_energy, .fun = median))

p104 <- ggplot(p104_data, aes(x = renewables_share_energy, y = continent, fill = continent)) +
  geom_density_ridges(alpha = 0.7, colour = etx_text, linewidth = 0.3, scale = 1.2) +
  scale_fill_manual(values = c("Africa" = etx_accent3, "Americas" = etx_accent,
                                "Asia" = etx_accent4, "Europe" = etx_accent2, "Oceania" = etx_accent6)) +
  labs(title = "How Renewable Is Each Continent?",
       subtitle = glue("Distribution of renewable energy share across countries · {latest_year}"),
       caption = etx_caption(), x = "Renewable share (%)", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 11),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p104, file.path(out_dir, "104_ridgeline_renewables.png"))


# ============================================================
# 105 — Ridgeline: Energy Per Capita by Continent
# ============================================================
p105_data <- latest %>%
  filter(!is.na(energy_per_capita), !is.na(continent)) %>%
  mutate(continent = fct_reorder(continent, energy_per_capita, .fun = median))

p105 <- ggplot(p105_data, aes(x = energy_per_capita, y = continent, fill = continent)) +
  geom_density_ridges(alpha = 0.7, colour = etx_text, linewidth = 0.3, scale = 1.2) +
  scale_fill_manual(values = c("Africa" = etx_accent3, "Americas" = etx_accent,
                                "Asia" = etx_accent4, "Europe" = etx_accent2, "Oceania" = etx_accent6)) +
  scale_x_continuous(labels = comma) +
  labs(title = "Energy Consumption Per Capita: A Continental View",
       subtitle = glue("Distribution of primary energy per capita (kWh) · {latest_year}"),
       caption = etx_caption(), x = "kWh per capita", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 11),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p105, file.path(out_dir, "105_ridgeline_percapita.png"))


# ============================================================
# 106 — Slope Chart: Renewable Share 2010 vs 2024
# ============================================================
slope_countries <- c("Germany", "United Kingdom", "China", "India", "Brazil",
                     "United States", "Japan", "Australia", "France", "Turkey")
p106_data <- energy %>%
  filter(country %in% slope_countries, year %in% c(2010, latest_year),
         !is.na(renewables_share_energy)) %>%
  select(country, year, share = renewables_share_energy)

p106 <- ggplot(p106_data, aes(x = factor(year), y = share, group = country, colour = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  geom_text_repel(data = p106_data %>% filter(year == latest_year),
                  aes(label = paste0(country, " ", round(share, 1), "%")),
                  hjust = -0.2, direction = "y", family = "inter", size = 3,
                  segment.colour = etx_grid, nudge_x = 0.15) +
  scale_colour_manual(values = rep(etx_palette, 2)) +
  labs(title = "Renewable Energy: Where Were We, Where Are We Now?",
       subtitle = glue("Renewable share of primary energy: 2010 vs {latest_year}"),
       caption = etx_caption(), x = NULL, y = "%") +
  coord_cartesian(clip = "off") +
  theme_energtx() +
  theme(axis.text.x = element_text(colour = etx_text, size = 14, face = "bold"),
        plot.margin = margin(24, 80, 20, 24))
etx_save(p106, file.path(out_dir, "106_slope_renewables.png"))


# ============================================================
# 107 — Slope Chart: Fossil Share 2010 vs 2024
# ============================================================
p107_data <- energy %>%
  filter(country %in% slope_countries, year %in% c(2010, latest_year),
         !is.na(fossil_share_energy)) %>%
  select(country, year, share = fossil_share_energy)

p107 <- ggplot(p107_data, aes(x = factor(year), y = share, group = country, colour = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  geom_text_repel(data = p107_data %>% filter(year == latest_year),
                  aes(label = paste0(country, " ", round(share, 1), "%")),
                  hjust = -0.2, direction = "y", family = "inter", size = 3,
                  segment.colour = etx_grid, nudge_x = 0.15) +
  scale_colour_manual(values = rep(etx_palette, 2)) +
  labs(title = "Fossil Fuel Dependency: Then vs Now",
       subtitle = glue("Fossil share of primary energy: 2010 vs {latest_year}"),
       caption = etx_caption(), x = NULL, y = "%") +
  coord_cartesian(clip = "off") +
  theme_energtx() +
  theme(axis.text.x = element_text(colour = etx_text, size = 14, face = "bold"),
        plot.margin = margin(24, 80, 20, 24))
etx_save(p107, file.path(out_dir, "107_slope_fossil.png"))


# ============================================================
# 108 — Diverging Bar: Carbon Intensity Change 2010→2024
# ============================================================
p108_data <- energy %>%
  filter(year %in% c(2010, latest_year), !is.na(carbon_intensity_elec)) %>%
  select(country, year, ci = carbon_intensity_elec) %>%
  pivot_wider(names_from = year, values_from = ci, names_prefix = "y") %>%
  drop_na() %>%
  mutate(change = .data[[paste0("y", latest_year)]] - y2010,
         direction = ifelse(change < 0, "Improved", "Worsened")) %>%
  slice_max(abs(change), n = 25) %>%
  mutate(country = fct_reorder(country, change))

p108 <- ggplot(p108_data, aes(x = change, y = country, fill = direction)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = etx_text, linewidth = 0.4) +
  scale_fill_manual(values = c("Improved" = etx_accent, "Worsened" = etx_accent4)) +
  labs(title = "Grid Decarbonization: Winners & Losers",
       subtitle = glue("Change in carbon intensity of electricity (gCO2/kWh) · 2010 to {latest_year}"),
       caption = etx_caption(), x = "Change (gCO2/kWh)", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(title = NULL))
etx_save(p108, file.path(out_dir, "108_diverging_carbon.png"))


# ============================================================
# 109 — Small Multiples: G7 Energy Mix Evolution (Faceted Area)
# ============================================================
g7 <- c("United States", "United Kingdom", "France", "Germany", "Japan", "Italy", "Canada")

p109_data <- energy %>%
  filter(country %in% g7, year >= 2000) %>%
  select(country, year, Coal = coal_share_energy, Oil = oil_share_energy,
         Gas = gas_share_energy, Nuclear = nuclear_share_energy,
         Renewables = renewables_share_energy) %>%
  pivot_longer(c(-country, -year), names_to = "fuel", values_to = "share") %>%
  filter(!is.na(share)) %>%
  mutate(fuel = factor(fuel, levels = c("Coal","Oil","Gas","Nuclear","Renewables")))

p109_colors <- c("Coal" = etx_coal, "Oil" = "#92400E", "Gas" = etx_gas,
                 "Nuclear" = etx_nuclear, "Renewables" = etx_accent)

p109 <- ggplot(p109_data, aes(x = year, y = share, fill = fuel)) +
  geom_area(alpha = 0.85) +
  facet_wrap(~country, nrow = 2) +
  scale_fill_manual(values = p109_colors) +
  labs(title = "G7 Energy Transitions: Seven Countries, Seven Stories",
       subtitle = glue("Primary energy mix evolution (%) · 2000-{latest_year}"),
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 8),
        legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_text(colour = etx_sub, size = 7),
        axis.text.y = element_text(colour = etx_sub, size = 7)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p109, file.path(out_dir, "109_g7_faceted.png"), w = 14, h = 8)


# ============================================================
# 110 — Small Multiples: BRICS Energy Mix Evolution
# ============================================================
brics <- c("Brazil", "Russia", "India", "China", "South Africa")

p110_data <- energy %>%
  filter(country %in% brics, year >= 2000) %>%
  select(country, year, Coal = coal_share_energy, Oil = oil_share_energy,
         Gas = gas_share_energy, Nuclear = nuclear_share_energy,
         Renewables = renewables_share_energy) %>%
  pivot_longer(c(-country, -year), names_to = "fuel", values_to = "share") %>%
  filter(!is.na(share)) %>%
  mutate(fuel = factor(fuel, levels = c("Coal","Oil","Gas","Nuclear","Renewables")))

p110 <- ggplot(p110_data, aes(x = year, y = share, fill = fuel)) +
  geom_area(alpha = 0.85) +
  facet_wrap(~country, nrow = 1) +
  scale_fill_manual(values = p109_colors) +
  labs(title = "BRICS Energy: Five Economies, Five Transformations",
       subtitle = glue("Primary energy mix evolution (%) · 2000-{latest_year}"),
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 8),
        legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_text(colour = etx_sub, size = 7)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p110, file.path(out_dir, "110_brics_faceted.png"), w = 14, h = 5)


# ============================================================
# 111 — Diverging Bar: Renewable Share Growth (2020→2024)
# ============================================================
p111_data <- energy %>%
  filter(year %in% c(2020, latest_year), !is.na(renewables_share_energy)) %>%
  select(country, year, share = renewables_share_energy) %>%
  pivot_wider(names_from = year, values_from = share, names_prefix = "y") %>%
  drop_na() %>%
  mutate(change = .data[[paste0("y", latest_year)]] - y2020,
         direction = ifelse(change > 0, "Gained", "Lost")) %>%
  slice_max(abs(change), n = 25) %>%
  mutate(country = fct_reorder(country, change))

p111 <- ggplot(p111_data, aes(x = change, y = country, fill = direction)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = etx_text, linewidth = 0.4) +
  scale_fill_manual(values = c("Gained" = etx_accent, "Lost" = etx_accent4)) +
  labs(title = "Renewable Energy: Who Gained, Who Lost Since 2020?",
       subtitle = glue("Change in renewable share (pp) · 2020 to {latest_year}"),
       caption = etx_caption(), x = "Percentage points", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(title = NULL))
etx_save(p111, file.path(out_dir, "111_diverging_renewable.png"))


# ============================================================
# 112 — Faceted Line: Solar Growth by Region
# ============================================================
solar_countries <- c("China", "United States", "India", "Japan", "Germany",
                     "Australia", "Spain", "Brazil", "South Korea", "Turkey",
                     "United Kingdom", "France")

p112_data <- energy %>%
  filter(country %in% solar_countries, year >= 2010, !is.na(solar_electricity))

p112 <- ggplot(p112_data, aes(x = year, y = solar_electricity)) +
  geom_area(fill = etx_solar, alpha = 0.4) +
  geom_line(colour = etx_solar, linewidth = 0.8) +
  facet_wrap(~country, scales = "free_y", nrow = 3) +
  scale_y_continuous(labels = comma) +
  labs(title = "Solar Power: 12 Countries, 12 Growth Curves",
       subtitle = "Solar electricity generation (TWh) · 2010-present",
       caption = etx_caption(), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(axis.text.x = element_text(colour = etx_sub, size = 7),
        axis.text.y = element_text(colour = etx_sub, size = 7))
etx_save(p112, file.path(out_dir, "112_solar_faceted.png"), w = 14, h = 8)


# ============================================================
# 113 — Faceted Line: Wind Growth by Region
# ============================================================
wind_countries <- c("China", "United States", "Germany", "India", "United Kingdom",
                    "Spain", "Brazil", "France", "Turkey", "Sweden",
                    "Canada", "Denmark")

p113_data <- energy %>%
  filter(country %in% wind_countries, year >= 2005, !is.na(wind_electricity))

p113 <- ggplot(p113_data, aes(x = year, y = wind_electricity)) +
  geom_area(fill = etx_wind, alpha = 0.4) +
  geom_line(colour = etx_wind, linewidth = 0.8) +
  facet_wrap(~country, scales = "free_y", nrow = 3) +
  scale_y_continuous(labels = comma) +
  labs(title = "Wind Power: 12 Countries, 12 Growth Curves",
       subtitle = "Wind electricity generation (TWh) · 2005-present",
       caption = etx_caption(), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(axis.text.x = element_text(colour = etx_sub, size = 7),
        axis.text.y = element_text(colour = etx_sub, size = 7))
etx_save(p113, file.path(out_dir, "113_wind_faceted.png"), w = 14, h = 8)


# ============================================================
# 114 — Stacked Proportional: Top 10 Solar Producers Share
# ============================================================
top_solar <- energy %>%
  filter(year == latest_year, !is.na(solar_electricity)) %>%
  slice_max(solar_electricity, n = 10) %>% pull(country)

p114_world <- raw %>%
  filter(country == "World", year >= 2010, !is.na(solar_electricity)) %>%
  select(year, world = solar_electricity)

p114_data <- energy %>%
  filter(country %in% top_solar, year >= 2010, !is.na(solar_electricity)) %>%
  select(year, country, solar_electricity)

p114 <- ggplot(p114_data, aes(x = year, y = solar_electricity, fill = country)) +
  geom_area(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = setNames(rep(etx_palette, 2), top_solar)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Who Owns the Solar Market?",
       subtitle = "Share of global solar generation by country (%) · 2010-present",
       caption = etx_caption(), x = NULL, y = NULL) +
  theme_energtx() +
  theme(legend.position = "right",
        legend.text = element_text(colour = etx_text, size = 8),
        axis.text.x = element_text(colour = etx_sub)) +
  guides(fill = guide_legend(title = NULL, ncol = 1))
etx_save(p114, file.path(out_dir, "114_solar_market_share.png"))


# ============================================================
# 115 — Stacked Proportional: Top 10 Wind Producers Share
# ============================================================
top_wind <- energy %>%
  filter(year == latest_year, !is.na(wind_electricity)) %>%
  slice_max(wind_electricity, n = 10) %>% pull(country)

p115_data <- energy %>%
  filter(country %in% top_wind, year >= 2005, !is.na(wind_electricity)) %>%
  select(year, country, wind_electricity)

p115 <- ggplot(p115_data, aes(x = year, y = wind_electricity, fill = country)) +
  geom_area(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = setNames(rep(etx_palette, 2), top_wind)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Who Owns the Wind Market?",
       subtitle = "Share of global wind generation by country (%) · 2005-present",
       caption = etx_caption(), x = NULL, y = NULL) +
  theme_energtx() +
  theme(legend.position = "right",
        legend.text = element_text(colour = etx_text, size = 8),
        axis.text.x = element_text(colour = etx_sub)) +
  guides(fill = guide_legend(title = NULL, ncol = 1))
etx_save(p115, file.path(out_dir, "115_wind_market_share.png"))


# ============================================================
# 116 — Polar/Radar: G7 Clean Energy Scorecard
# ============================================================
p116_data <- latest %>%
  filter(country %in% g7) %>%
  select(country,
         `Renewable %` = renewables_share_energy,
         `Low-Carbon Elec %` = low_carbon_share_elec,
         `Solar TWh` = solar_electricity,
         `Wind TWh` = wind_electricity,
         `Nuclear %` = nuclear_share_elec) %>%
  pivot_longer(-country, names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(metric) %>%
  mutate(normalized = (value - min(value)) / (max(value) - min(value) + 0.01)) %>%
  ungroup()

p116 <- ggplot(p116_data, aes(x = metric, y = normalized, fill = country)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_polar() +
  scale_fill_manual(values = setNames(etx_palette[1:7], g7)) +
  labs(title = "G7 Clean Energy Scorecard",
       subtitle = glue("Normalized metrics comparison · {latest_year}"),
       caption = etx_caption()) +
  theme_energtx() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(colour = etx_text, size = 8),
        panel.grid.major.y = element_blank(),
        legend.position = "right",
        legend.text = element_text(colour = etx_text, size = 8)) +
  guides(fill = guide_legend(title = NULL, ncol = 1))
etx_save(p116, file.path(out_dir, "116_g7_radar.png"), w = 10, h = 8)


# ============================================================
# 117-125 — Individual Country Donuts (popular countries)
# ============================================================
donut_countries <- list(
  c("United States", "117", etx_accent2),
  c("Germany", "118", etx_accent6),
  c("Japan", "119", etx_accent5),
  c("United Kingdom", "120", etx_accent2),
  c("Brazil", "121", etx_accent),
  c("Turkey", "122", etx_accent4),
  c("Russia", "123", etx_accent4),
  c("Australia", "124", etx_solar),
  c("South Korea", "125", etx_accent6)
)

for (dc in donut_countries) {
  cname <- dc[1]; cnum <- dc[2]; ccol <- dc[3]
  dd <- latest %>%
    filter(country == cname) %>%
    select(Coal = coal_share_energy, Oil = oil_share_energy,
           Gas = gas_share_energy, Nuclear = nuclear_share_energy,
           Hydro = hydro_share_energy, Wind = wind_share_energy,
           Solar = solar_share_energy) %>%
    pivot_longer(everything(), names_to = "source", values_to = "share") %>%
    filter(!is.na(share), share > 0.3) %>%
    mutate(source = factor(source, levels = c("Coal","Oil","Gas","Nuclear","Hydro","Wind","Solar")),
           ymax = cumsum(share), ymin = lag(ymax, default = 0),
           mid = (ymin + ymax) / 2,
           label = paste0(source, "\n", round(share, 1), "%"))

  pd <- ggplot(dd, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = source)) +
    geom_rect(colour = etx_bg) +
    geom_text(aes(x = 3.5, y = mid, label = label), colour = etx_text, size = 3, family = "inter") +
    coord_polar(theta = "y") + xlim(c(1, 4.5)) +
    scale_fill_manual(values = fuel_colors) +
    annotate("text", x = 1, y = 0, label = glue("{toupper(cname)}\n{latest_year}"),
             colour = ccol, family = "spacegrotesk", fontface = "bold", size = 4.5) +
    labs(title = glue("What Powers {cname}?"),
         subtitle = glue("{cname} primary energy mix · {latest_year}"),
         caption = etx_caption()) +
    theme_energtx() +
    theme(axis.text = element_blank(), panel.grid = element_blank())

  etx_save(pd, file.path(out_dir, paste0(cnum, "_", gsub(" ", "_", tolower(cname)), "_donut.png")))
}


# ============================================================
# 126 — Faceted Area: EU Big 5 Electricity Mix
# ============================================================
eu5 <- c("Germany", "France", "Italy", "Spain", "Poland")

p126_data <- energy %>%
  filter(country %in% eu5, year >= 2000) %>%
  select(country, year, Coal = coal_share_elec, Gas = gas_share_elec,
         Nuclear = nuclear_share_elec, Wind = wind_share_elec,
         Solar = solar_share_elec, Hydro = hydro_share_elec) %>%
  pivot_longer(c(-country, -year), names_to = "source", values_to = "share") %>%
  filter(!is.na(share)) %>%
  mutate(source = factor(source, levels = c("Coal","Gas","Nuclear","Hydro","Wind","Solar")))

p126_colors <- c("Coal" = etx_coal, "Gas" = etx_gas, "Nuclear" = etx_nuclear,
                 "Hydro" = etx_hydro, "Wind" = etx_wind, "Solar" = etx_solar)

p126 <- ggplot(p126_data, aes(x = year, y = share, fill = source)) +
  geom_area(alpha = 0.85) +
  facet_wrap(~country, nrow = 1) +
  scale_fill_manual(values = p126_colors) +
  labs(title = "EU Big 5 Electricity Mix: Five Grids, Five Paths",
       subtitle = glue("Electricity generation share (%) · 2000-{latest_year}"),
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 8),
        legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_text(colour = etx_sub, size = 7)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p126, file.path(out_dir, "126_eu5_elec_faceted.png"), w = 14, h = 5)


# ============================================================
# 127 — Diverging: Electricity Generation Change 2019→2024
# ============================================================
p127_data <- energy %>%
  filter(year %in% c(2019, latest_year), !is.na(electricity_generation)) %>%
  select(country, year, elec = electricity_generation) %>%
  pivot_wider(names_from = year, values_from = elec, names_prefix = "y") %>%
  drop_na() %>%
  mutate(change_pct = (.data[[paste0("y", latest_year)]] / y2019 - 1) * 100,
         direction = ifelse(change_pct > 0, "Growth", "Decline")) %>%
  filter(is.finite(change_pct)) %>%
  slice_max(abs(change_pct), n = 25) %>%
  mutate(country = fct_reorder(country, change_pct))

p127 <- ggplot(p127_data, aes(x = change_pct, y = country, fill = direction)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = etx_text, linewidth = 0.4) +
  scale_fill_manual(values = c("Growth" = etx_accent, "Decline" = etx_accent4)) +
  labs(title = "Post-COVID Electricity: Who Bounced Back?",
       subtitle = glue("Electricity generation change (%) · 2019 vs {latest_year}"),
       caption = etx_caption(), x = "% change", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(title = NULL))
etx_save(p127, file.path(out_dir, "127_post_covid_elec.png"))


# ============================================================
# 128 — Paired Bar: Fossil vs Clean Electricity (Top 15)
# ============================================================
p128_data <- latest %>%
  filter(!is.na(fossil_share_elec), !is.na(low_carbon_share_elec)) %>%
  slice_max(electricity_generation, n = 15) %>%
  select(country, Fossil = fossil_share_elec, `Low-Carbon` = low_carbon_share_elec) %>%
  pivot_longer(-country, names_to = "type", values_to = "share") %>%
  mutate(country = fct_reorder(country, ifelse(type == "Low-Carbon", share, 0), .fun = max))

p128 <- ggplot(p128_data, aes(x = share, y = country, fill = type)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Fossil" = etx_accent4, "Low-Carbon" = etx_accent)) +
  labs(title = "Clean vs Dirty Electricity: Top 15 Generators",
       subtitle = glue("Fossil vs low-carbon share of electricity (%) · {latest_year}"),
       caption = etx_caption(), x = "%", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(title = NULL))
etx_save(p128, file.path(out_dir, "128_clean_vs_dirty.png"))


# ============================================================
# 129 — Waterfall-style: Global Energy Addition by Source
# ============================================================
p129_data <- raw %>%
  filter(country == "World") %>%
  filter(year %in% c(2015, latest_year)) %>%
  select(year, Coal = coal_consumption, Gas = gas_consumption,
         Oil = oil_consumption, Nuclear = nuclear_consumption,
         Hydro = hydro_consumption, Wind = wind_consumption,
         Solar = solar_consumption) %>%
  pivot_longer(-year, names_to = "source", values_to = "value") %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "y") %>%
  drop_na() %>%
  mutate(change = .data[[paste0("y", latest_year)]] - y2015,
         direction = ifelse(change > 0, "Growth", "Decline"),
         source = fct_reorder(source, change))

p129 <- ggplot(p129_data, aes(x = source, y = change, fill = direction)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, colour = etx_text, linewidth = 0.3) +
  geom_text(aes(label = paste0(ifelse(change > 0, "+", ""), comma(round(change))), y = change),
            vjust = ifelse(p129_data$change > 0, -0.5, 1.5),
            colour = etx_text, family = "spacegrotesk", size = 3.5) +
  scale_fill_manual(values = c("Growth" = etx_accent, "Decline" = etx_accent4)) +
  labs(title = "Where Did New Energy Come From Since 2015?",
       subtitle = glue("Change in global energy consumption by source (TWh) · 2015 to {latest_year}"),
       caption = etx_caption(), x = NULL, y = "TWh change") +
  theme_energtx() +
  theme(axis.text.x = element_text(colour = etx_text, size = 11))
etx_save(p129, file.path(out_dir, "129_energy_waterfall.png"))


# ============================================================
# 130 — Donut: US Energy Mix
# Already handled by loop above (117), adding global trend
# Faceted Donuts: World vs China vs US vs India (2x2)
# ============================================================
# Using a simpler paired comparison instead
p130_data <- raw %>%
  filter(country == "World", year >= 1990, !is.na(renewables_share_energy)) %>%
  select(year, Renewable = renewables_share_energy) %>%
  mutate(Fossil = 100 - Renewable) %>%
  pivot_longer(-year, names_to = "type", values_to = "share")

p130 <- ggplot(p130_data, aes(x = year, y = share, fill = type)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = c("Fossil" = etx_accent4, "Renewable" = etx_accent)) +
  geom_hline(yintercept = 50, colour = etx_accent6, linetype = "dashed", linewidth = 0.4) +
  annotate("text", x = 2005, y = 53, label = "50% line",
           colour = etx_accent6, family = "inter", size = 3) +
  labs(title = "The Great Energy Race: Fossil vs Renewable",
       subtitle = "Global share of primary energy (%) · 1990-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 10),
        axis.text.x = element_text(colour = etx_sub)) +
  guides(fill = guide_legend(title = NULL))
etx_save(p130, file.path(out_dir, "130_fossil_vs_renewable_race.png"))


# ============================================================
cat("\n========================================\n")
cat("Batch 3: 30 new infographics generated! (101-130)\n")
cat("Total: 130 infographics in", out_dir, "\n")
cat("========================================\n")
