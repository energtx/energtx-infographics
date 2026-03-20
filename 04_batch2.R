# ============================================================
# energtx Infographic Generator — Batch 2 (56–110)
# Karşılaştırma ağırlıklı 55 yeni infografik
# ============================================================

source("C:/Users/bartu/Desktop/energtx-infographics/01_data.R")

# -- Reusable country groups --
g20 <- c("United States", "China", "India", "Japan", "Germany", "United Kingdom",
        "France", "Brazil", "Canada", "South Korea", "Russia", "Australia",
        "Italy", "Mexico", "Indonesia", "Turkey", "Saudi Arabia", "Argentina",
        "South Africa")

eu <- c("Germany", "France", "Italy", "Spain", "Netherlands", "Belgium",
       "Poland", "Sweden", "Denmark", "Finland", "Austria", "Portugal",
       "Czech Republic", "Romania", "Greece", "Ireland", "Hungary")

heat_countries <- c("Germany", "United Kingdom", "Spain", "France", "Italy",
                   "Denmark", "Sweden", "Norway", "Brazil", "China",
                   "India", "Japan", "United States", "Australia", "South Korea")

# ============================================================
# 56 — Coal Share: Top 10 vs Bottom 10
# ============================================================
coal_top <- latest %>% filter(!is.na(coal_share_energy), coal_share_energy > 0) %>%
  slice_max(coal_share_energy, n = 10)
coal_bot <- latest %>% filter(!is.na(coal_share_energy), coal_share_energy > 0) %>%
  slice_min(coal_share_energy, n = 10)
p56_data <- bind_rows(
  coal_top %>% mutate(group = "Most Coal-Dependent"),
  coal_bot %>% mutate(group = "Least Coal-Dependent")
) %>% mutate(country = fct_reorder(country, coal_share_energy))

p56 <- ggplot(p56_data, aes(x = coal_share_energy, y = country, fill = group)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("Most Coal-Dependent" = etx_coal, "Least Coal-Dependent" = etx_accent)) +
  geom_text(aes(label = paste0(round(coal_share_energy, 1), "%")),
            hjust = -0.2, colour = etx_text, family = "spacegrotesk", size = 3.2) +
  labs(title = "Coal Dependency: The Two Extremes",
       subtitle = glue("Coal share of primary energy (%) · {latest_year}"),
       caption = etx_caption(), x = NULL, y = NULL) +
  coord_cartesian(clip = "off") +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        legend.position = c(0.75, 0.2),
        legend.text = element_text(colour = etx_text, size = 8))
etx_save(p56, file.path(out_dir, "56_coal_extremes.png"))


# ============================================================
# 57 — Solar Share: Asia vs Europe vs Americas
# ============================================================
p57_data <- latest %>%
  filter(!is.na(solar_share_elec), !is.na(continent)) %>%
  group_by(continent) %>%
  summarise(avg = mean(solar_share_elec, na.rm = TRUE),
            max_val = max(solar_share_elec, na.rm = TRUE),
            min_val = min(solar_share_elec, na.rm = TRUE), .groups = "drop") %>%
  filter(continent != "Africa") %>%
  mutate(continent = fct_reorder(continent, avg))

p57 <- ggplot(p57_data, aes(y = continent)) +
  geom_segment(aes(x = min_val, xend = max_val, yend = continent), colour = etx_grid, linewidth = 2) +
  geom_point(aes(x = avg), colour = etx_solar, size = 5) +
  geom_point(aes(x = min_val), colour = etx_accent4, size = 3) +
  geom_point(aes(x = max_val), colour = etx_accent, size = 3) +
  geom_text(aes(x = avg, label = paste0(round(avg, 1), "% avg")),
            vjust = -1.5, colour = etx_text, family = "spacegrotesk", size = 3.5) +
  labs(title = "Solar Electricity by Continent: Range & Average",
       subtitle = glue("Min, average, max solar share of electricity (%) · {latest_year}"),
       caption = etx_caption(), x = "%", y = NULL) +
  coord_cartesian(clip = "off") +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 11),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p57, file.path(out_dir, "57_solar_by_continent.png"))


# ============================================================
# 58 — Renewables Race: Germany vs UK vs France vs Spain
# ============================================================
eu4 <- c("Germany", "United Kingdom", "France", "Spain")
p58_data <- energy %>%
  filter(country %in% eu4, year >= 2000, !is.na(renewables_share_elec))

p58 <- ggplot(p58_data, aes(x = year, y = renewables_share_elec, colour = country)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("Germany" = etx_accent6, "United Kingdom" = etx_accent2,
                                  "France" = etx_nuclear, "Spain" = etx_accent3)) +
  labs(title = "Europe's Big Four: Renewables Race",
       subtitle = "Renewable share of electricity (%) · 2000-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.8),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p58, file.path(out_dir, "58_eu4_renewables_race.png"))


# ============================================================
# 59 — US vs China vs India: Electricity Generation
# ============================================================
big3 <- c("United States", "China", "India")
p59_data <- energy %>%
  filter(country %in% big3, year >= 1990, !is.na(electricity_generation))

p59 <- ggplot(p59_data, aes(x = year, y = electricity_generation, colour = country)) +
  geom_line(linewidth = 1.5) +
  scale_colour_manual(values = c("United States" = etx_accent2, "China" = etx_accent4, "India" = etx_accent3)) +
  scale_y_continuous(labels = comma) +
  labs(title = "The Electricity Titans: US vs China vs India",
       subtitle = "Total electricity generation (TWh) · 1990-present",
       caption = etx_caption(), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.8),
        legend.text = element_text(colour = etx_text, size = 10),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p59, file.path(out_dir, "59_us_china_india_elec.png"))


# ============================================================
# 60 — Nuclear: France vs US vs China vs South Korea
# ============================================================
nuc4 <- c("France", "United States", "China", "South Korea")
p60_data <- energy %>%
  filter(country %in% nuc4, year >= 1990, !is.na(nuclear_electricity))

p60 <- ggplot(p60_data, aes(x = year, y = nuclear_electricity, colour = country)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("France" = etx_nuclear, "United States" = etx_accent2,
                                  "China" = etx_accent4, "South Korea" = etx_accent6)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Nuclear Power Race: Four Strategies",
       subtitle = "Nuclear electricity generation (TWh) · 1990-present",
       caption = etx_caption("IAEA"), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.8),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p60, file.path(out_dir, "60_nuclear_race.png"))


# ============================================================
# 61 — Gas Share: Russia vs US vs Iran vs Qatar
# ============================================================
gas4 <- c("Russia", "United States", "Iran", "Saudi Arabia")
p61_data <- energy %>%
  filter(country %in% gas4, year >= 2000, !is.na(gas_share_energy))

p61 <- ggplot(p61_data, aes(x = year, y = gas_share_energy, colour = country)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("Russia" = etx_accent4, "United States" = etx_accent2,
                                  "Iran" = etx_accent3, "Saudi Arabia" = etx_accent6)) +
  labs(title = "Gas Giants: Who Depends Most?",
       subtitle = "Natural gas share of primary energy (%) · 2000-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.35),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p61, file.path(out_dir, "61_gas_giants.png"))


# ============================================================
# 62 — Wind: Denmark vs UK vs Germany vs Spain
# ============================================================
wind4 <- c("Denmark", "United Kingdom", "Germany", "Spain")
p62_data <- energy %>%
  filter(country %in% wind4, year >= 2000, !is.na(wind_share_elec))

p62 <- ggplot(p62_data, aes(x = year, y = wind_share_elec, colour = country)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("Denmark" = etx_wind, "United Kingdom" = etx_accent2,
                                  "Germany" = etx_accent6, "Spain" = etx_accent3)) +
  labs(title = "Wind Share Race: Europe's Leaders",
       subtitle = "Wind share of electricity (%) · 2000-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.75),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p62, file.path(out_dir, "62_wind_share_race.png"))


# ============================================================
# 63 — Solar: China vs US vs India vs Japan vs Australia
# ============================================================
sol5 <- c("China", "United States", "India", "Japan", "Australia")
p63_data <- energy %>%
  filter(country %in% sol5, year >= 2010, !is.na(solar_share_elec))

p63 <- ggplot(p63_data, aes(x = year, y = solar_share_elec, colour = country)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("China" = etx_accent4, "United States" = etx_accent2,
                                  "India" = etx_accent3, "Japan" = etx_accent5, "Australia" = etx_solar)) +
  labs(title = "Solar Share Race: Five Contenders",
       subtitle = "Solar share of electricity (%) · 2010-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.75),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p63, file.path(out_dir, "63_solar_share_race.png"))


# ============================================================
# 64 — Fossil vs Renewable: G20 Grouped Bar
# ============================================================
p64_data <- latest %>%
  filter(country %in% g20, !is.na(fossil_share_energy), !is.na(renewables_share_energy)) %>%
  select(country, Fossil = fossil_share_energy, Renewable = renewables_share_energy) %>%
  pivot_longer(-country, names_to = "type", values_to = "share") %>%
  mutate(country = fct_reorder(country, ifelse(type == "Renewable", share, 0), .fun = max))

p64 <- ggplot(p64_data, aes(x = share, y = country, fill = type)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Fossil" = etx_accent4, "Renewable" = etx_accent)) +
  labs(title = "G20: Fossil vs Renewable Energy",
       subtitle = glue("Share of primary energy (%) · {latest_year}"),
       caption = etx_caption(), x = "%", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(title = NULL))
etx_save(p64, file.path(out_dir, "64_g20_fossil_vs_renewable.png"))


# ============================================================
# 65 — Per Capita Electricity: Gulf vs Nordic vs South Asia
# ============================================================
gulf <- c("Saudi Arabia", "United Arab Emirates")
nordic_s <- c("Norway", "Sweden")
south_asia <- c("India", "Pakistan", "Bangladesh")
p65_data <- latest %>%
  filter(country %in% c(gulf, nordic_s, south_asia), !is.na(per_capita_electricity)) %>%
  mutate(
    region = case_when(
      country %in% gulf ~ "Gulf States",
      country %in% nordic_s ~ "Nordic",
      TRUE ~ "South Asia"
    ),
    country = fct_reorder(country, per_capita_electricity)
  )

p65 <- ggplot(p65_data, aes(x = per_capita_electricity, y = country, fill = region)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(comma(round(per_capita_electricity)), " kWh")),
            hjust = -0.1, colour = etx_text, family = "spacegrotesk", size = 3.3) +
  scale_fill_manual(values = c("Gulf States" = etx_accent3, "Nordic" = etx_accent2, "South Asia" = etx_accent5)) +
  scale_x_continuous(labels = comma) +
  labs(title = "Electricity Per Capita: Three Worlds",
       subtitle = glue("Electricity consumption per person (kWh) · {latest_year}"),
       caption = etx_caption(), x = NULL, y = NULL) +
  coord_cartesian(clip = "off") +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 11),
        legend.position = c(0.75, 0.25),
        legend.text = element_text(colour = etx_text, size = 9))
etx_save(p65, file.path(out_dir, "65_percapita_three_worlds.png"))


# ============================================================
# 66 — Hydro vs Nuclear: Clean Baseload Compared
# ============================================================
p66_data <- latest %>%
  filter(!is.na(hydro_share_elec), !is.na(nuclear_share_elec)) %>%
  select(country, Hydro = hydro_share_elec, Nuclear = nuclear_share_elec) %>%
  pivot_longer(-country, names_to = "source", values_to = "share") %>%
  group_by(country) %>% mutate(total = sum(share)) %>% ungroup() %>%
  filter(total > 30) %>%
  mutate(country = fct_reorder(country, total))

p66 <- ggplot(p66_data, aes(x = share, y = country, fill = source)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("Hydro" = etx_hydro, "Nuclear" = etx_nuclear)) +
  labs(title = "Clean Baseload: Hydro vs Nuclear",
       subtitle = glue("Combined hydro + nuclear share of electricity (%) · {latest_year} · Countries >30%"),
       caption = etx_caption(), x = "%", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(title = NULL))
etx_save(p66, file.path(out_dir, "66_hydro_vs_nuclear.png"))


# ============================================================
# 67 — Oil Producers vs Oil Consumers
# ============================================================
top_prod <- latest %>% filter(!is.na(oil_production)) %>% slice_max(oil_production, n = 8)
top_cons <- latest %>% filter(!is.na(oil_consumption)) %>% slice_max(oil_consumption, n = 8)
p67_overlap <- intersect(top_prod$country, top_cons$country)
p67_data <- latest %>%
  filter(country %in% union(top_prod$country, top_cons$country)) %>%
  select(country, Production = oil_production, Consumption = oil_consumption) %>%
  pivot_longer(-country, names_to = "type", values_to = "TWh") %>%
  filter(!is.na(TWh)) %>%
  mutate(country = fct_reorder(country, TWh, .fun = max))

p67 <- ggplot(p67_data, aes(x = TWh, y = country, fill = type)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Production" = etx_accent, "Consumption" = etx_accent3)) +
  scale_x_continuous(labels = comma) +
  labs(title = "Oil: Producers vs Consumers",
       subtitle = glue("Oil production vs consumption (TWh) · {latest_year}"),
       caption = etx_caption("Energy Institute"), x = "TWh", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(title = NULL))
etx_save(p67, file.path(out_dir, "67_oil_prod_vs_cons.png"))


# ============================================================
# 68 — Energy Mix: EU vs US vs China
# ============================================================
p68_data <- latest %>%
  filter(country %in% c("United States", "China")) %>%
  bind_rows(
    energy %>% filter(country %in% eu, year == latest_year) %>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      mutate(country = "EU Average")
  ) %>%
  select(country, Coal = coal_share_energy, Oil = oil_share_energy,
         Gas = gas_share_energy, Nuclear = nuclear_share_energy,
         Renewables = renewables_share_energy) %>%
  pivot_longer(-country, names_to = "source", values_to = "share") %>%
  filter(!is.na(share)) %>%
  mutate(source = factor(source, levels = c("Coal","Oil","Gas","Nuclear","Renewables")))

p68_colors <- c("Coal" = etx_coal, "Oil" = etx_oil, "Gas" = etx_gas,
                "Nuclear" = etx_nuclear, "Renewables" = etx_accent)

p68 <- ggplot(p68_data, aes(x = country, y = share, fill = source)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = p68_colors) +
  labs(title = "Energy DNA: EU vs US vs China",
       subtitle = glue("Primary energy mix (%) · {latest_year}"),
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(axis.text.x = element_text(colour = etx_text, size = 12),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p68, file.path(out_dir, "68_eu_us_china_mix.png"))


# ============================================================
# 69 — Decade Change: Renewable Share 2014 vs 2024
# ============================================================
p69_data <- energy %>%
  filter(year %in% c(2014, latest_year), !is.na(renewables_share_energy)) %>%
  select(country, year, renewables_share_energy) %>%
  pivot_wider(names_from = year, values_from = renewables_share_energy, names_prefix = "y") %>%
  drop_na() %>%
  mutate(change = .data[[paste0("y", latest_year)]] - y2014) %>%
  slice_max(change, n = 20) %>%
  mutate(country = fct_reorder(country, change))

p69 <- ggplot(p69_data) +
  geom_segment(aes(x = y2014, xend = .data[[paste0("y", latest_year)]],
                   y = country, yend = country), colour = etx_grid, linewidth = 0.8) +
  geom_point(aes(x = y2014, y = country), colour = etx_accent3, size = 3.5) +
  geom_point(aes(x = .data[[paste0("y", latest_year)]], y = country), colour = etx_accent, size = 3.5) +
  labs(title = "Decade of Change: Renewable Energy Growth",
       subtitle = glue("Renewable share of energy: 2014 vs {latest_year} · Top 20 gainers"),
       caption = etx_caption(), x = "%", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p69, file.path(out_dir, "69_decade_renewable_change.png"))


# ============================================================
# 70 — Coal Phase-Out: Germany vs UK vs Poland vs Turkey
# ============================================================
coal4 <- c("Germany", "United Kingdom", "Poland", "Turkey")
p70_data <- energy %>%
  filter(country %in% coal4, year >= 2000, !is.na(coal_share_elec))

p70 <- ggplot(p70_data, aes(x = year, y = coal_share_elec, colour = country)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("Germany" = etx_accent6, "United Kingdom" = etx_accent2,
                                  "Poland" = etx_coal, "Turkey" = etx_accent4)) +
  labs(title = "Coal Phase-Out: Four Different Speeds",
       subtitle = "Coal share of electricity (%) · 2000-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = c(0.85, 0.75),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p70, file.path(out_dir, "70_coal_phaseout_4.png"))


# ============================================================
# 71 — Middle East Energy: Saudi vs UAE vs Iran vs Iraq
# ============================================================
me4 <- c("Saudi Arabia", "United Arab Emirates", "Iran", "Iraq")
p71_data <- latest %>%
  filter(country %in% me4) %>%
  select(country, Oil = oil_share_energy, Gas = gas_share_energy,
         Nuclear = nuclear_share_energy, Solar = solar_share_energy) %>%
  pivot_longer(-country, names_to = "source", values_to = "share") %>%
  filter(!is.na(share), share > 0)

p71_colors <- c("Oil" = etx_oil, "Gas" = etx_gas, "Nuclear" = etx_nuclear, "Solar" = etx_solar)

p71 <- ggplot(p71_data, aes(x = country, y = share, fill = source)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = p71_colors) +
  labs(title = "Middle East Energy: Four Petrostates Compared",
       subtitle = glue("Primary energy mix (%) · {latest_year}"),
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(axis.text.x = element_text(colour = etx_text, size = 10),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p71, file.path(out_dir, "71_middle_east_mix.png"))


# ============================================================
# 72 — Southeast Asia: Indonesia vs Vietnam vs Thailand vs Philippines
# ============================================================
sea4 <- c("Indonesia", "Vietnam", "Thailand", "Philippines")
p72_data <- latest %>%
  filter(country %in% sea4) %>%
  select(country, Coal = coal_share_elec, Gas = gas_share_elec,
         Hydro = hydro_share_elec, Solar = solar_share_elec, Wind = wind_share_elec) %>%
  pivot_longer(-country, names_to = "source", values_to = "share") %>%
  filter(!is.na(share), share > 0) %>%
  mutate(source = factor(source, levels = c("Coal","Gas","Hydro","Wind","Solar")))

p72_colors <- c("Coal" = etx_coal, "Gas" = etx_gas, "Hydro" = etx_hydro,
                "Wind" = etx_wind, "Solar" = etx_solar)

p72 <- ggplot(p72_data, aes(x = country, y = share, fill = source)) +
  geom_col(position = "fill", width = 0.6) +
  scale_fill_manual(values = p72_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Southeast Asia: Four Grids Compared",
       subtitle = glue("Electricity mix (%) · {latest_year}"),
       caption = etx_caption(), x = NULL, y = NULL) +
  theme_energtx() +
  theme(axis.text.x = element_text(colour = etx_text, size = 10),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p72, file.path(out_dir, "72_southeast_asia.png"))


# ============================================================
# 73 — Latin America: Brazil vs Argentina vs Chile vs Colombia vs Mexico
# ============================================================
latam <- c("Brazil", "Argentina", "Chile", "Colombia", "Mexico")
p73_data <- latest %>%
  filter(country %in% latam) %>%
  select(country, Hydro = hydro_share_elec, Wind = wind_share_elec,
         Solar = solar_share_elec, Gas = gas_share_elec, Coal = coal_share_elec) %>%
  pivot_longer(-country, names_to = "source", values_to = "share") %>%
  filter(!is.na(share), share > 0) %>%
  mutate(source = factor(source, levels = c("Coal","Gas","Hydro","Wind","Solar")))

p73 <- ggplot(p73_data, aes(x = country, y = share, fill = source)) +
  geom_col(position = "fill", width = 0.6) +
  scale_fill_manual(values = p72_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Latin America: Five Electricity Strategies",
       subtitle = glue("Electricity mix (%) · {latest_year}"),
       caption = etx_caption(), x = NULL, y = NULL) +
  theme_energtx() +
  theme(axis.text.x = element_text(colour = etx_text, size = 10),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p73, file.path(out_dir, "73_latin_america.png"))


# ============================================================
# 74 — Electricity Per Capita Heatmap (G20 × Years)
# ============================================================
p74_data <- energy %>%
  filter(country %in% g20, year >= 2000, !is.na(per_capita_electricity))

p74 <- ggplot(p74_data, aes(x = year, y = fct_rev(country), fill = per_capita_electricity)) +
  geom_tile(colour = etx_bg, linewidth = 0.5) +
  scale_fill_gradient(low = "#1a1a2e", high = etx_accent2) +
  labs(title = "G20 Electricity Per Capita Over Time",
       subtitle = "Electricity consumption per capita (kWh) · 2000-present",
       caption = etx_caption(), x = NULL, y = NULL) +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.title = element_text(colour = etx_text, size = 9),
        legend.text = element_text(colour = etx_sub, size = 8),
        legend.key.width = unit(2, "cm"), legend.key.height = unit(0.3, "cm"),
        axis.text.y = element_text(colour = etx_text, size = 8),
        axis.text.x = element_text(colour = etx_sub, size = 8)) +
  guides(fill = guide_colorbar(title = "kWh", title.position = "left"))
etx_save(p74, file.path(out_dir, "74_g20_elec_heatmap.png"))


# ============================================================
# 75 — Fossil Share Heatmap (15 Countries × Years)
# ============================================================
p75_data <- energy %>%
  filter(country %in% heat_countries, year >= 2000, !is.na(fossil_share_energy))

p75 <- ggplot(p75_data, aes(x = year, y = fct_rev(country), fill = fossil_share_energy)) +
  geom_tile(colour = etx_bg, linewidth = 0.5) +
  scale_fill_gradient2(low = etx_accent, mid = etx_accent6, high = etx_accent4, midpoint = 60, limits = c(0,100)) +
  labs(title = "Fossil Fuel Dependency: 25 Years in a Heatmap",
       subtitle = "Fossil share of primary energy (%) · 2000-present",
       caption = etx_caption(), x = NULL, y = NULL) +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.title = element_text(colour = etx_text, size = 9),
        legend.text = element_text(colour = etx_sub, size = 8),
        legend.key.width = unit(2, "cm"), legend.key.height = unit(0.3, "cm"),
        axis.text.y = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub, size = 8)) +
  guides(fill = guide_colorbar(title = "%", title.position = "left"))
etx_save(p75, file.path(out_dir, "75_fossil_heatmap.png"))


# ============================================================
# 76 — Gas Production: US vs Russia vs Iran
# ============================================================
gas3 <- c("United States", "Russia", "Iran")
p76_data <- energy %>%
  filter(country %in% gas3, year >= 1990, !is.na(gas_production))

p76 <- ggplot(p76_data, aes(x = year, y = gas_production, colour = country)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("United States" = etx_accent2, "Russia" = etx_accent4, "Iran" = etx_accent3)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Natural Gas Production: The Big Three",
       subtitle = "Gas production (TWh) · 1990-present",
       caption = etx_caption("Energy Institute"), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.85),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p76, file.path(out_dir, "76_gas_production_3.png"))


# ============================================================
# 77 — Oil Production: US vs Saudi vs Russia
# ============================================================
oil3 <- c("United States", "Saudi Arabia", "Russia")
p77_data <- energy %>%
  filter(country %in% oil3, year >= 1990, !is.na(oil_production))

p77 <- ggplot(p77_data, aes(x = year, y = oil_production, colour = country)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("United States" = etx_accent2, "Saudi Arabia" = etx_accent6, "Russia" = etx_accent4)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Oil Production: The Three Superpowers",
       subtitle = "Oil production (TWh) · 1990-present",
       caption = etx_caption("Energy Institute"), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.3),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p77, file.path(out_dir, "77_oil_production_3.png"))


# ============================================================
# 78 — Coal Consumption: China vs India vs US
# ============================================================
coal3 <- c("China", "India", "United States")
p78_data <- energy %>%
  filter(country %in% coal3, year >= 1990, !is.na(coal_consumption))

p78 <- ggplot(p78_data, aes(x = year, y = coal_consumption, colour = country)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("China" = etx_accent4, "India" = etx_accent3, "United States" = etx_accent2)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Coal Consumption: The Divergence",
       subtitle = "Coal consumption (TWh) · 1990-present",
       caption = etx_caption(), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.5),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p78, file.path(out_dir, "78_coal_consumption_3.png"))


# ============================================================
# 79 — Energy Per Capita: Top 10 vs Bottom 10 Dumbbell
# ============================================================
p79_top <- latest %>% filter(!is.na(energy_per_capita)) %>% slice_max(energy_per_capita, n = 10)
p79_bot <- latest %>% filter(!is.na(energy_per_capita)) %>% slice_min(energy_per_capita, n = 10)
p79_data <- bind_rows(p79_top %>% mutate(g = "Top 10"), p79_bot %>% mutate(g = "Bottom 10")) %>%
  mutate(country = fct_reorder(country, energy_per_capita))

p79 <- ggplot(p79_data, aes(x = energy_per_capita, y = country, fill = g)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("Top 10" = etx_accent3, "Bottom 10" = etx_accent5)) +
  scale_x_continuous(labels = comma) +
  labs(title = "Energy Per Capita: The 50x Gap",
       subtitle = glue("Primary energy per capita (kWh) · {latest_year}"),
       caption = etx_caption(), x = "kWh", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(title = NULL))
etx_save(p79, file.path(out_dir, "79_energy_percapita_gap.png"))


# ============================================================
# 80 — Low Carbon: France vs Sweden vs Norway vs Brazil vs Canada
# ============================================================
lc5 <- c("France", "Sweden", "Norway", "Brazil", "Canada")
p80_data <- energy %>%
  filter(country %in% lc5, year >= 2000, !is.na(low_carbon_share_elec))

p80 <- ggplot(p80_data, aes(x = year, y = low_carbon_share_elec, colour = country)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("France" = etx_nuclear, "Sweden" = etx_accent2,
                                  "Norway" = etx_hydro, "Brazil" = etx_accent,
                                  "Canada" = etx_accent3)) +
  geom_hline(yintercept = 90, colour = etx_accent6, linetype = "dashed", linewidth = 0.4) +
  labs(title = "The 90% Club: Near-Zero Carbon Grids",
       subtitle = "Low-carbon share of electricity (%) · 2000-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = c(0.85, 0.3),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p80, file.path(out_dir, "80_90_percent_club.png"))


# ============================================================
# 81 — Electricity Import/Export Balance
# ============================================================
p81_data <- latest %>%
  filter(!is.na(net_elec_imports)) %>%
  slice_max(abs(net_elec_imports), n = 20) %>%
  mutate(
    country = fct_reorder(country, net_elec_imports),
    direction = ifelse(net_elec_imports > 0, "Net Importer", "Net Exporter")
  )

p81 <- ggplot(p81_data, aes(x = net_elec_imports, y = country, fill = direction)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = etx_text, linewidth = 0.3) +
  scale_fill_manual(values = c("Net Importer" = etx_accent4, "Net Exporter" = etx_accent)) +
  labs(title = "Electricity Trade: Who Imports, Who Exports?",
       subtitle = glue("Net electricity imports (TWh) · {latest_year}"),
       caption = etx_caption(), x = "TWh", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9)) +
  guides(fill = guide_legend(title = NULL))
etx_save(p81, file.path(out_dir, "81_elec_imports_exports.png"))


# ============================================================
# 82 — Renewables Treemap
# ============================================================
p82_data <- latest %>%
  filter(!is.na(renewables_electricity), renewables_electricity > 5) %>%
  mutate(label = paste0(country, "\n", round(renewables_electricity), " TWh"))

p82 <- ggplot(p82_data, aes(area = renewables_electricity,
                            fill = renewables_share_elec, label = label)) +
  geom_treemap(colour = etx_bg, size = 2) +
  geom_treemap_text(colour = etx_text, family = "inter", size = 9, place = "centre", grow = FALSE) +
  scale_fill_gradient(low = etx_accent3, high = etx_accent) +
  labs(title = "Global Renewable Electricity: Who Generates Most?",
       subtitle = glue("Size = TWh renewable · Color = renewable share (%) · {latest_year}"),
       caption = etx_caption()) +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.title = element_text(colour = etx_text, size = 9),
        legend.text = element_text(colour = etx_sub, size = 8),
        legend.key.width = unit(2, "cm"), legend.key.height = unit(0.3, "cm")) +
  guides(fill = guide_colorbar(title = "Renewable %", title.position = "left"))
etx_save(p82, file.path(out_dir, "82_renewables_treemap.png"))


# ============================================================
# 83 — Energy Consumption Growth: 2000 vs 2024
# ============================================================
p83_data <- energy %>%
  filter(year %in% c(2000, latest_year), !is.na(primary_energy_consumption)) %>%
  select(country, year, primary_energy_consumption) %>%
  pivot_wider(names_from = year, values_from = primary_energy_consumption, names_prefix = "y") %>%
  drop_na() %>%
  mutate(growth = (.data[[paste0("y", latest_year)]] / y2000 - 1) * 100) %>%
  filter(is.finite(growth)) %>%
  slice_max(abs(growth), n = 20) %>%
  mutate(country = fct_reorder(country, growth),
         bar_col = ifelse(growth > 0, etx_accent3, etx_accent2))

p83 <- ggplot(p83_data, aes(x = growth, y = country, fill = bar_col)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = etx_text, linewidth = 0.3) +
  scale_fill_identity() +
  labs(title = "Energy Appetite: Who Grew, Who Shrank?",
       subtitle = glue("Primary energy consumption change (%) · 2000 vs {latest_year}"),
       caption = etx_caption(), x = "% change", y = NULL) +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p83, file.path(out_dir, "83_energy_growth.png"))


# ============================================================
# 84 — Carbon Intensity Heatmap (15 Countries × Years)
# ============================================================
p84_data <- energy %>%
  filter(country %in% heat_countries, year >= 2000, !is.na(carbon_intensity_elec))

p84 <- ggplot(p84_data, aes(x = year, y = fct_rev(country), fill = carbon_intensity_elec)) +
  geom_tile(colour = etx_bg, linewidth = 0.5) +
  scale_fill_gradient2(low = etx_accent, mid = etx_accent6, high = etx_accent4, midpoint = 300) +
  labs(title = "Grid Decarbonization: Watch It Happen",
       subtitle = "Carbon intensity of electricity (gCO2/kWh) · 2000-present",
       caption = etx_caption(), x = NULL, y = NULL) +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.title = element_text(colour = etx_text, size = 9),
        legend.text = element_text(colour = etx_sub, size = 8),
        legend.key.width = unit(2, "cm"), legend.key.height = unit(0.3, "cm"),
        axis.text.y = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub, size = 8)) +
  guides(fill = guide_colorbar(title = "gCO2/kWh", title.position = "left"))
etx_save(p84, file.path(out_dir, "84_carbon_intensity_heatmap.png"))


# ============================================================
# 85 — Solar Generation: China vs Rest of World
# ============================================================
p85_world <- raw %>% filter(country == "World", year >= 2010, !is.na(solar_electricity)) %>%
  select(year, world_solar = solar_electricity)
p85_china <- energy %>% filter(country == "China", year >= 2010, !is.na(solar_electricity)) %>%
  select(year, china_solar = solar_electricity)
p85_data <- inner_join(p85_world, p85_china, by = "year") %>%
  mutate(rest = world_solar - china_solar) %>%
  select(year, China = china_solar, `Rest of World` = rest) %>%
  pivot_longer(-year, names_to = "group", values_to = "TWh")

p85 <- ggplot(p85_data, aes(x = year, y = TWh, fill = group)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = c("China" = etx_accent4, "Rest of World" = etx_solar)) +
  scale_y_continuous(labels = comma) +
  labs(title = "China vs Everyone Else: Solar Dominance",
       subtitle = "Solar electricity generation (TWh) · 2010-present",
       caption = etx_caption(), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p85, file.path(out_dir, "85_china_vs_world_solar.png"))


# ============================================================
# 86 — Wind: China vs Rest of World
# ============================================================
p86_world <- raw %>% filter(country == "World", year >= 2005, !is.na(wind_electricity)) %>%
  select(year, world_wind = wind_electricity)
p86_china <- energy %>% filter(country == "China", year >= 2005, !is.na(wind_electricity)) %>%
  select(year, china_wind = wind_electricity)
p86_data <- inner_join(p86_world, p86_china, by = "year") %>%
  mutate(rest = world_wind - china_wind) %>%
  select(year, China = china_wind, `Rest of World` = rest) %>%
  pivot_longer(-year, names_to = "group", values_to = "TWh")

p86 <- ggplot(p86_data, aes(x = year, y = TWh, fill = group)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = c("China" = etx_accent4, "Rest of World" = etx_wind)) +
  scale_y_continuous(labels = comma) +
  labs(title = "China vs Everyone Else: Wind Power",
       subtitle = "Wind electricity generation (TWh) · 2005-present",
       caption = etx_caption(), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p86, file.path(out_dir, "86_china_vs_world_wind.png"))


# ============================================================
# 87 — GHG Emissions: Top 15
# ============================================================
p87_data <- latest %>%
  filter(!is.na(greenhouse_gas_emissions)) %>%
  slice_max(greenhouse_gas_emissions, n = 15) %>%
  mutate(country = fct_reorder(country, greenhouse_gas_emissions))

p87 <- ggplot(p87_data, aes(x = greenhouse_gas_emissions, y = country)) +
  geom_col(fill = etx_accent4, width = 0.7) +
  geom_text(aes(label = comma(round(greenhouse_gas_emissions))),
            hjust = -0.1, colour = etx_text, family = "spacegrotesk", size = 3.3) +
  scale_x_continuous(labels = comma) +
  labs(title = "Biggest Greenhouse Gas Emitters",
       subtitle = glue("Total GHG emissions (MtCO2e) · {latest_year}"),
       caption = etx_caption("Global Carbon Project"), x = NULL, y = NULL) +
  coord_cartesian(clip = "off") +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 10))
etx_save(p87, file.path(out_dir, "87_ghg_top15.png"))


# ============================================================
# 88 — Electricity Demand Per Capita: Top 20
# ============================================================
p88_data <- latest %>%
  filter(!is.na(electricity_demand_per_capita)) %>%
  slice_max(electricity_demand_per_capita, n = 20) %>%
  mutate(country = fct_reorder(country, electricity_demand_per_capita))

p88 <- ggplot(p88_data, aes(x = electricity_demand_per_capita, y = country)) +
  geom_segment(aes(x = 0, xend = electricity_demand_per_capita, yend = country),
               colour = etx_grid, linewidth = 0.6) +
  geom_point(colour = etx_accent2, size = 4) +
  geom_text(aes(label = paste0(comma(round(electricity_demand_per_capita)), " kWh")),
            hjust = -0.2, colour = etx_text, family = "spacegrotesk", size = 3.2) +
  labs(title = "Who Demands the Most Electricity Per Person?",
       subtitle = glue("Electricity demand per capita (kWh) · {latest_year}"),
       caption = etx_caption(), x = NULL, y = NULL) +
  coord_cartesian(clip = "off") +
  theme_energtx() +
  theme(axis.text.y = element_text(colour = etx_text, size = 9))
etx_save(p88, file.path(out_dir, "88_elec_demand_percapita.png"))


# ============================================================
# 89 — Germany vs Japan: Two Energy Transitions
# ============================================================
p89_data <- energy %>%
  filter(country %in% c("Germany", "Japan"), year >= 2000, !is.na(renewables_share_elec))

p89 <- ggplot(p89_data, aes(x = year, y = renewables_share_elec, colour = country)) +
  geom_line(linewidth = 1.5) +
  scale_colour_manual(values = c("Germany" = etx_accent6, "Japan" = etx_accent5)) +
  labs(title = "Germany vs Japan: Two Energy Transitions",
       subtitle = "Renewable share of electricity (%) · 2000-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.85),
        legend.text = element_text(colour = etx_text, size = 10),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p89, file.path(out_dir, "89_germany_vs_japan.png"))


# ============================================================
# 90 — UK Coal Collapse: A Success Story
# ============================================================
p90_data <- energy %>%
  filter(country == "United Kingdom", year >= 1990) %>%
  select(year, Coal = coal_share_elec, Wind = wind_share_elec,
         Solar = solar_share_elec, Gas = gas_share_elec) %>%
  pivot_longer(-year, names_to = "source", values_to = "share") %>%
  filter(!is.na(share))

p90_colors <- c("Coal" = etx_coal, "Gas" = etx_gas, "Wind" = etx_wind, "Solar" = etx_solar)

p90 <- ggplot(p90_data, aes(x = year, y = share, colour = source)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = p90_colors) +
  labs(title = "UK: How to Kill Coal in 15 Years",
       subtitle = "Electricity share by source (%) · 1990-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = c(0.5, 0.7),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p90, file.path(out_dir, "90_uk_coal_collapse.png"))


# ============================================================
# 91 — India Energy Mix Evolution
# ============================================================
p91_data <- energy %>%
  filter(country == "India", year >= 2000) %>%
  select(year, Coal = coal_share_energy, Oil = oil_share_energy,
         Gas = gas_share_energy, Solar = solar_share_energy,
         Wind = wind_share_energy, Hydro = hydro_share_energy) %>%
  pivot_longer(-year, names_to = "fuel", values_to = "share") %>%
  filter(!is.na(share)) %>%
  mutate(fuel = factor(fuel, levels = c("Coal","Oil","Gas","Hydro","Wind","Solar")))

p91_colors <- c("Coal" = etx_coal, "Oil" = etx_oil, "Gas" = etx_gas,
                "Hydro" = etx_hydro, "Wind" = etx_wind, "Solar" = etx_solar)

p91 <- ggplot(p91_data, aes(x = year, y = share, fill = fuel)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = p91_colors) +
  labs(title = "India's Energy Mix: Coal Dominance Under Pressure",
       subtitle = "Primary energy share (%) · 2000-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9),
        legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_text(colour = etx_sub)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p91, file.path(out_dir, "91_india_energy_mix.png"))


# ============================================================
# 92 — China Energy Mix Evolution
# ============================================================
p92_data <- energy %>%
  filter(country == "China", year >= 2000) %>%
  select(year, Coal = coal_share_energy, Oil = oil_share_energy,
         Gas = gas_share_energy, Nuclear = nuclear_share_energy,
         Hydro = hydro_share_energy, Wind = wind_share_energy,
         Solar = solar_share_energy) %>%
  pivot_longer(-year, names_to = "fuel", values_to = "share") %>%
  filter(!is.na(share)) %>%
  mutate(fuel = factor(fuel, levels = c("Coal","Oil","Gas","Nuclear","Hydro","Wind","Solar")))

p92 <- ggplot(p92_data, aes(x = year, y = share, fill = fuel)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = fuel_colors) +
  labs(title = "China's Energy Transformation",
       subtitle = "Primary energy share (%) · 2000-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9),
        legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_text(colour = etx_sub)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p92, file.path(out_dir, "92_china_energy_mix.png"))


# ============================================================
# 93 — Russia Energy Mix
# ============================================================
p93_data <- energy %>%
  filter(country == "Russia", year >= 1992) %>%
  select(year, Coal = coal_share_energy, Oil = oil_share_energy,
         Gas = gas_share_energy, Nuclear = nuclear_share_energy,
         Hydro = hydro_share_energy) %>%
  pivot_longer(-year, names_to = "fuel", values_to = "share") %>%
  filter(!is.na(share)) %>%
  mutate(fuel = factor(fuel, levels = c("Coal","Oil","Gas","Nuclear","Hydro")))

p93_colors <- c("Coal" = etx_coal, "Oil" = etx_oil, "Gas" = etx_gas,
                "Nuclear" = etx_nuclear, "Hydro" = etx_hydro)

p93 <- ggplot(p93_data, aes(x = year, y = share, fill = fuel)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = p93_colors) +
  labs(title = "Russia: The Gas Superpower",
       subtitle = "Primary energy share (%) · 1992-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9),
        legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_text(colour = etx_sub)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p93, file.path(out_dir, "93_russia_energy_mix.png"))


# ============================================================
# 94 — Electricity Share of Energy: Rising Trend
# ============================================================
elec5 <- c("China", "United States", "Germany", "India", "Japan")
p94_data <- energy %>%
  filter(country %in% elec5, year >= 1990, !is.na(electricity_share_energy))

p94 <- ggplot(p94_data, aes(x = year, y = electricity_share_energy, colour = country)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("China" = etx_accent4, "United States" = etx_accent2,
                                  "Germany" = etx_accent6, "India" = etx_accent3, "Japan" = etx_accent5)) +
  labs(title = "Electrification: How Much Energy Is Electricity?",
       subtitle = "Electricity as share of total energy (%) · 1990-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.75),
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p94, file.path(out_dir, "94_electrification_trend.png"))


# ============================================================
# 95 — Austria vs Switzerland: Alpine Clean Energy
# ============================================================
alp2 <- c("Austria", "Switzerland")
p95_data <- energy %>%
  filter(country %in% alp2, year >= 1990) %>%
  select(year, country, Hydro = hydro_share_elec, Nuclear = nuclear_share_elec,
         Wind = wind_share_elec, Solar = solar_share_elec) %>%
  pivot_longer(c(-year, -country), names_to = "source", values_to = "share") %>%
  filter(!is.na(share))

p95 <- ggplot(p95_data, aes(x = year, y = share, fill = source)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = c("Hydro" = etx_hydro, "Nuclear" = etx_nuclear,
                                "Wind" = etx_wind, "Solar" = etx_solar)) +
  facet_wrap(~country) +
  labs(title = "Alpine Clean Energy: Austria vs Switzerland",
       subtitle = "Low-carbon electricity share (%) · 1990-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub, size = 8)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p95, file.path(out_dir, "95_austria_vs_switzerland.png"))


# ============================================================
# 96 — Portugal vs Ireland: Small Countries, Big Renewables
# ============================================================
pi2 <- c("Portugal", "Ireland")
p96_data <- energy %>%
  filter(country %in% pi2, year >= 2000, !is.na(renewables_share_elec))

p96 <- ggplot(p96_data, aes(x = year, y = renewables_share_elec, colour = country)) +
  geom_line(linewidth = 1.5) +
  scale_colour_manual(values = c("Portugal" = etx_accent3, "Ireland" = etx_accent)) +
  labs(title = "Small Countries, Big Ambitions",
       subtitle = "Renewable share of electricity (%) · Portugal vs Ireland · 2000-present",
       caption = etx_caption(), x = NULL, y = "%") +
  theme_energtx() +
  theme(legend.position = c(0.15, 0.85),
        legend.text = element_text(colour = etx_text, size = 10),
        axis.text.x = element_text(colour = etx_sub))
etx_save(p96, file.path(out_dir, "96_portugal_vs_ireland.png"))


# ============================================================
# 97 — Global Electricity by Source: Stacked Area
# ============================================================
p97_data <- raw %>%
  filter(country == "World", year >= 1985) %>%
  select(year, Coal = coal_electricity, Oil = oil_electricity,
         Gas = gas_electricity, Nuclear = nuclear_electricity,
         Hydro = hydro_electricity, Wind = wind_electricity,
         Solar = solar_electricity) %>%
  pivot_longer(-year, names_to = "source", values_to = "TWh") %>%
  filter(!is.na(TWh)) %>%
  mutate(source = factor(source, levels = c("Coal","Oil","Gas","Nuclear","Hydro","Wind","Solar")))

p97_colors <- c("Coal" = etx_coal, "Oil" = etx_oil, "Gas" = etx_gas,
                "Nuclear" = etx_nuclear, "Hydro" = etx_hydro,
                "Wind" = etx_wind, "Solar" = etx_solar)

p97 <- ggplot(p97_data, aes(x = year, y = TWh, fill = source)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = p97_colors) +
  scale_y_continuous(labels = comma) +
  labs(title = "How the World Makes Electricity",
       subtitle = "Global electricity generation by source (TWh) · 1985-present",
       caption = etx_caption(), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9),
        legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_text(colour = etx_sub)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p97, file.path(out_dir, "97_global_elec_by_source.png"))


# ============================================================
# 98 — Coal Electricity: China vs India vs Rest
# ============================================================
p98_world <- raw %>% filter(country == "World", year >= 2000, !is.na(coal_electricity)) %>%
  select(year, world = coal_electricity)
p98_ci <- energy %>% filter(country %in% c("China","India"), year >= 2000, !is.na(coal_electricity)) %>%
  select(year, country, coal_electricity) %>%
  pivot_wider(names_from = country, values_from = coal_electricity)
p98_data <- inner_join(p98_world, p98_ci, by = "year") %>%
  mutate(`Rest of World` = world - China - India) %>%
  select(year, China, India, `Rest of World`) %>%
  pivot_longer(-year, names_to = "group", values_to = "TWh")

p98 <- ggplot(p98_data, aes(x = year, y = TWh, fill = group)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = c("China" = etx_accent4, "India" = etx_accent3, "Rest of World" = etx_coal)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Coal Electricity: China + India vs The World",
       subtitle = "Coal electricity generation (TWh) · 2000-present",
       caption = etx_caption(), x = NULL, y = "TWh") +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub)) +
  guides(fill = guide_legend(nrow = 1, title = NULL))
etx_save(p98, file.path(out_dir, "98_coal_china_india_rest.png"))


# ============================================================
# 99 — Nuclear Electricity: Heatmap by Country
# ============================================================
nuc_countries <- latest %>%
  filter(!is.na(nuclear_share_elec), nuclear_share_elec > 0) %>% pull(country)

p99_data <- energy %>%
  filter(country %in% nuc_countries, year >= 2000, !is.na(nuclear_share_elec))

p99 <- ggplot(p99_data, aes(x = year, y = fct_rev(country), fill = nuclear_share_elec)) +
  geom_tile(colour = etx_bg, linewidth = 0.5) +
  scale_fill_gradient(low = "#1a1a2e", high = etx_nuclear) +
  labs(title = "Nuclear Power by Country: 25 Years",
       subtitle = "Nuclear share of electricity (%) · 2000-present",
       caption = etx_caption("IAEA"), x = NULL, y = NULL) +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.title = element_text(colour = etx_text, size = 9),
        legend.text = element_text(colour = etx_sub, size = 8),
        legend.key.width = unit(2, "cm"), legend.key.height = unit(0.3, "cm"),
        axis.text.y = element_text(colour = etx_text, size = 9),
        axis.text.x = element_text(colour = etx_sub, size = 8)) +
  guides(fill = guide_colorbar(title = "%", title.position = "left"))
etx_save(p99, file.path(out_dir, "99_nuclear_heatmap.png"))


# ============================================================
# 100 — Electricity Generation Treemap by Continent
# ============================================================
p100_data <- latest %>%
  filter(!is.na(electricity_generation), !is.na(continent)) %>%
  group_by(continent) %>%
  summarise(total = sum(electricity_generation, na.rm = TRUE),
            avg_lc = mean(low_carbon_share_elec, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(continent, "\n", comma(round(total)), " TWh"))

p100 <- ggplot(p100_data, aes(area = total, fill = avg_lc, label = label)) +
  geom_treemap(colour = etx_bg, size = 3) +
  geom_treemap_text(colour = etx_text, family = "spacegrotesk", fontface = "bold",
                    size = 16, place = "centre") +
  scale_fill_gradient(low = etx_accent4, high = etx_accent) +
  labs(title = "Global Electricity by Continent",
       subtitle = glue("Size = TWh · Color = avg low-carbon share · {latest_year}"),
       caption = etx_caption()) +
  theme_energtx() +
  theme(legend.position = "bottom",
        legend.title = element_text(colour = etx_text, size = 9),
        legend.text = element_text(colour = etx_sub, size = 8),
        legend.key.width = unit(2, "cm"), legend.key.height = unit(0.3, "cm")) +
  guides(fill = guide_colorbar(title = "Low-carbon %", title.position = "left"))
etx_save(p100, file.path(out_dir, "100_continent_treemap.png"))


# ============================================================
cat("\n========================================\n")
cat("Batch 2: 45 new infographics generated! (56-100)\n")
cat("Total: 100 infographics in", out_dir, "\n")
cat("========================================\n")
