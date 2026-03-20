# ============================================================
# energtx Infographic Generator — 55 Unique Visuals
# ============================================================
# Run: source("C:/Users/bartu/Desktop/energtx-infographics/02_generate.R")
# Output: C:/Users/bartu/Desktop/energtx-infographics/png/
# ============================================================

source("C:/Users/bartu/Desktop/energtx-infographics/01_data.R")

# ============================================================
# 01 — Energy Intensity (Energy per GDP) — Top 20 Least Efficient
# ============================================================
p01_data <- latest %>%
 filter(!is.na(energy_per_gdp), energy_per_gdp > 0) %>%
 slice_max(energy_per_gdp, n = 20) %>%
 mutate(country = fct_reorder(country, energy_per_gdp))

p01 <- ggplot(p01_data, aes(x = energy_per_gdp, y = country)) +
 geom_segment(aes(x = 0, xend = energy_per_gdp, yend = country),
              colour = etx_grid, linewidth = 0.6) +
 geom_point(size = 4, colour = etx_accent4) +
 geom_text(aes(label = round(energy_per_gdp, 1)),
           hjust = -0.5, colour = etx_text, family = "spacegrotesk",
           fontface = "bold", size = 3.5) +
 labs(
   title = "Most Energy-Intensive Economies",
   subtitle = glue("Energy consumption per unit of GDP (kWh/$) · {latest_year}"),
   caption = etx_caption()
 ) +
 coord_cartesian(clip = "off") +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 10))

etx_save(p01, file.path(out_dir, "01_energy_intensity.png"))


# ============================================================
# 02 — Carbon Intensity of Electricity — Best 20
# ============================================================
p02_data <- latest %>%
 filter(!is.na(carbon_intensity_elec), carbon_intensity_elec > 0) %>%
 slice_min(carbon_intensity_elec, n = 20) %>%
 mutate(
   country = fct_reorder(country, -carbon_intensity_elec),
   bar_col = ifelse(carbon_intensity_elec < 100, etx_accent, etx_accent2)
 )

p02 <- ggplot(p02_data, aes(x = carbon_intensity_elec, y = country)) +
 geom_col(aes(fill = bar_col), width = 0.7) +
 geom_text(aes(label = paste0(round(carbon_intensity_elec), " gCO₂/kWh")),
           hjust = -0.1, colour = etx_text, family = "inter", size = 3.2) +
 scale_fill_identity() +
 labs(
   title = "Cleanest Electricity Grids in the World",
   subtitle = glue("Carbon intensity of electricity generation (gCO₂/kWh) · {latest_year}"),
   caption = etx_caption()
 ) +
 coord_cartesian(clip = "off") +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 10))

etx_save(p02, file.path(out_dir, "02_carbon_intensity_cleanest.png"))


# ============================================================
# 03 — Carbon Intensity — Worst 20
# ============================================================
p03_data <- latest %>%
 filter(!is.na(carbon_intensity_elec)) %>%
 slice_max(carbon_intensity_elec, n = 20) %>%
 mutate(country = fct_reorder(country, carbon_intensity_elec))

p03 <- ggplot(p03_data, aes(x = carbon_intensity_elec, y = country)) +
 geom_col(fill = etx_accent4, width = 0.7) +
 geom_text(aes(label = paste0(round(carbon_intensity_elec), " gCO₂/kWh")),
           hjust = -0.1, colour = etx_text, family = "inter", size = 3.2) +
 labs(
   title = "Dirtiest Electricity Grids in the World",
   subtitle = glue("Carbon intensity of electricity generation (gCO₂/kWh) · {latest_year}"),
   caption = etx_caption()
 ) +
 coord_cartesian(clip = "off") +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 10))

etx_save(p03, file.path(out_dir, "03_carbon_intensity_dirtiest.png"))


# ============================================================
# 04 — Hydropower Generation — Top 15
# ============================================================
p04_data <- latest %>%
 filter(!is.na(hydro_electricity)) %>%
 slice_max(hydro_electricity, n = 15) %>%
 mutate(country = fct_reorder(country, hydro_electricity))

p04 <- ggplot(p04_data, aes(x = hydro_electricity, y = country)) +
 geom_segment(aes(x = 0, xend = hydro_electricity, yend = country),
              colour = etx_grid, linewidth = 0.6) +
 geom_point(size = 5, colour = etx_hydro) +
 geom_text(aes(label = paste0(comma(round(hydro_electricity)), " TWh")),
           hjust = -0.3, colour = etx_text, family = "spacegrotesk",
           fontface = "bold", size = 3.5) +
 labs(
   title = "Hydropower Giants",
   subtitle = glue("Electricity generation from hydropower (TWh) · {latest_year}"),
   caption = etx_caption()
 ) +
 coord_cartesian(clip = "off") +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 10))

etx_save(p04, file.path(out_dir, "04_hydropower_top15.png"))


# ============================================================
# 05 — Low-Carbon Electricity Share — Top 20
# ============================================================
p05_data <- latest %>%
 filter(!is.na(low_carbon_share_elec)) %>%
 slice_max(low_carbon_share_elec, n = 20) %>%
 mutate(country = fct_reorder(country, low_carbon_share_elec))

p05 <- ggplot(p05_data, aes(x = low_carbon_share_elec, y = country)) +
 geom_col(fill = etx_accent, width = 0.7, alpha = 0.85) +
 geom_text(aes(label = paste0(round(low_carbon_share_elec, 1), "%")),
           hjust = -0.2, colour = etx_text, family = "spacegrotesk",
           fontface = "bold", size = 3.5) +
 labs(
   title = "Low-Carbon Electricity Champions",
   subtitle = glue("Share of electricity from low-carbon sources (%) · {latest_year}"),
   caption = etx_caption()
 ) +
 coord_cartesian(clip = "off") +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 10))

etx_save(p05, file.path(out_dir, "05_low_carbon_share.png"))


# ============================================================
# 06 — Fossil Fuel Share — Highest Dependency
# ============================================================
p06_data <- latest %>%
 filter(!is.na(fossil_share_energy)) %>%
 slice_max(fossil_share_energy, n = 20) %>%
 mutate(country = fct_reorder(country, fossil_share_energy))

p06 <- ggplot(p06_data, aes(x = fossil_share_energy, y = country)) +
 geom_col(fill = etx_accent3, width = 0.7) +
 geom_vline(xintercept = 80, colour = etx_accent4, linetype = "dashed", linewidth = 0.5) +
 geom_text(aes(label = paste0(round(fossil_share_energy, 1), "%")),
           hjust = -0.2, colour = etx_text, family = "spacegrotesk",
           fontface = "bold", size = 3.5) +
 labs(
   title = "Most Fossil Fuel Dependent Economies",
   subtitle = glue("Fossil fuels as share of primary energy (%) · {latest_year}"),
   caption = etx_caption()
 ) +
 coord_cartesian(clip = "off") +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 10))

etx_save(p06, file.path(out_dir, "06_fossil_dependency.png"))


# ============================================================
# 07 — Solar Generation Growth — Line Chart (Top 10)
# ============================================================
top_solar <- energy %>%
 filter(year == latest_year, !is.na(solar_electricity)) %>%
 slice_max(solar_electricity, n = 10) %>%
 pull(country)

p07_data <- energy %>%
 filter(country %in% top_solar, year >= 2005, !is.na(solar_electricity))

p07 <- ggplot(p07_data, aes(x = year, y = solar_electricity, colour = country)) +
 geom_line(linewidth = 1) +
 geom_text_repel(
   data = p07_data %>% filter(year == max(year)),
   aes(label = country), direction = "y", nudge_x = 1,
   family = "inter", size = 3.2, segment.colour = etx_grid
 ) +
 scale_colour_manual(values = rep(etx_palette, 2)) +
 scale_y_continuous(labels = comma) +
 labs(
   title = "The Solar Explosion",
   subtitle = "Solar electricity generation (TWh) · Top 10 countries · 2005–present",
   caption = etx_caption(),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p07, file.path(out_dir, "07_solar_growth.png"))


# ============================================================
# 08 — Wind Generation Growth — Line Chart (Top 10)
# ============================================================
top_wind <- energy %>%
 filter(year == latest_year, !is.na(wind_electricity)) %>%
 slice_max(wind_electricity, n = 10) %>%
 pull(country)

p08_data <- energy %>%
 filter(country %in% top_wind, year >= 2000, !is.na(wind_electricity))

p08 <- ggplot(p08_data, aes(x = year, y = wind_electricity, colour = country)) +
 geom_line(linewidth = 1) +
 geom_text_repel(
   data = p08_data %>% filter(year == max(year)),
   aes(label = country), direction = "y", nudge_x = 1,
   family = "inter", size = 3.2, segment.colour = etx_grid
 ) +
 scale_colour_manual(values = rep(etx_palette, 2)) +
 scale_y_continuous(labels = comma) +
 labs(
   title = "Wind Power Surge",
   subtitle = "Wind electricity generation (TWh) · Top 10 countries · 2000–present",
   caption = etx_caption(),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p08, file.path(out_dir, "08_wind_growth.png"))


# ============================================================
# 09 — G7 Energy Mix — Stacked Bar
# ============================================================
g7 <- c("United States", "United Kingdom", "France", "Germany",
       "Japan", "Italy", "Canada")

p09_data <- latest %>%
 filter(country %in% g7) %>%
 select(country,
        Coal = coal_consumption, Oil = oil_consumption,
        Gas = gas_consumption, Nuclear = nuclear_consumption,
        Hydro = hydro_consumption, Solar = solar_consumption,
        Wind = wind_consumption) %>%
 pivot_longer(-country, names_to = "fuel", values_to = "value") %>%
 filter(!is.na(value), value > 0) %>%
 mutate(fuel = factor(fuel, levels = c("Coal", "Oil", "Gas", "Nuclear",
                                        "Hydro", "Wind", "Solar")))

fuel_colors <- c(
 "Coal" = etx_coal, "Oil" = etx_oil, "Gas" = etx_gas,
 "Nuclear" = etx_nuclear, "Hydro" = etx_hydro,
 "Wind" = etx_wind, "Solar" = etx_solar
)

p09 <- ggplot(p09_data, aes(x = fct_rev(country), y = value, fill = fuel)) +
 geom_col(position = "fill", width = 0.7) +
 scale_fill_manual(values = fuel_colors) +
 scale_y_continuous(labels = percent) +
 coord_flip(clip = "off") +
 labs(
   title = "G7 Energy Mix Compared",
   subtitle = glue("Primary energy consumption by source (%) · {latest_year}"),
   caption = etx_caption(),
   x = NULL, y = NULL
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.y = element_text(colour = etx_text, size = 11)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p09, file.path(out_dir, "09_g7_energy_mix.png"))


# ============================================================
# 10 — Electricity Per Capita — Scatter (GDP vs Electricity)
# ============================================================
p10_data <- latest %>%
 filter(!is.na(gdp), !is.na(per_capita_electricity), !is.na(population)) %>%
 mutate(gdp_per_cap = gdp / population)

p10 <- ggplot(p10_data, aes(x = gdp_per_cap, y = per_capita_electricity)) +
 geom_point(aes(size = population), colour = etx_accent2, alpha = 0.7) +
 geom_text_repel(aes(label = iso3), family = "spacegrotesk",
                 colour = etx_text, size = 2.8, max.overlaps = 15) +
 scale_x_log10(labels = dollar) +
 scale_y_log10(labels = comma) +
 scale_size_continuous(range = c(2, 15), guide = "none") +
 labs(
   title = "Wealth vs. Electricity Consumption",
   subtitle = glue("GDP per capita vs electricity per capita (kWh) · {latest_year}"),
   caption = etx_caption("Our World in Data · World Bank"),
   x = "GDP per capita (log scale)", y = "Electricity per capita kWh (log scale)"
 ) +
 theme_energtx() +
 theme(
   panel.grid.major.x = element_line(colour = etx_grid, linewidth = 0.3),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p10, file.path(out_dir, "10_gdp_vs_electricity.png"))


# ============================================================
# 11 — India vs China Electricity Growth
# ============================================================
p11_data <- energy %>%
 filter(country %in% c("China", "India"), year >= 2000,
        !is.na(electricity_generation))

p11 <- ggplot(p11_data, aes(x = year, y = electricity_generation, colour = country)) +
 geom_line(linewidth = 1.5) +
 geom_area(aes(fill = country), alpha = 0.1, position = "identity") +
 scale_colour_manual(values = c("China" = etx_accent4, "India" = etx_accent3)) +
 scale_fill_manual(values = c("China" = etx_accent4, "India" = etx_accent3)) +
 scale_y_continuous(labels = comma) +
 labs(
   title = "China vs India: The Electricity Race",
   subtitle = "Total electricity generation (TWh) · 2000–present",
   caption = etx_caption(),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(
   legend.position = c(0.15, 0.85),
   legend.text = element_text(colour = etx_text),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p11, file.path(out_dir, "11_china_vs_india.png"))


# ============================================================
# 12 — US Energy Mix Evolution (Stacked Area)
# ============================================================
p12_data <- energy %>%
 filter(country == "United States", year >= 1990) %>%
 select(year,
        Coal = coal_consumption, Oil = oil_consumption,
        Gas = gas_consumption, Nuclear = nuclear_consumption,
        Hydro = hydro_consumption, Solar = solar_consumption,
        Wind = wind_consumption) %>%
 pivot_longer(-year, names_to = "fuel", values_to = "value") %>%
 filter(!is.na(value)) %>%
 mutate(fuel = factor(fuel, levels = c("Coal", "Oil", "Gas", "Nuclear",
                                        "Hydro", "Wind", "Solar")))

p12 <- ggplot(p12_data, aes(x = year, y = value, fill = fuel)) +
 geom_area(alpha = 0.85) +
 scale_fill_manual(values = fuel_colors) +
 scale_y_continuous(labels = comma) +
 labs(
   title = "How America Powers Itself",
   subtitle = "US primary energy consumption by source (TWh) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.x = element_text(colour = etx_sub)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p12, file.path(out_dir, "12_us_energy_evolution.png"))


# ============================================================
# 13 — France Nuclear Dominance
# ============================================================
p13_data <- energy %>%
 filter(country == "France", year >= 1990) %>%
 select(year,
        Nuclear = nuclear_share_elec,
        Hydro = hydro_share_elec,
        Wind = wind_share_elec,
        Solar = solar_share_elec) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

share_colors <- c("Nuclear" = etx_nuclear, "Hydro" = etx_hydro,
                  "Wind" = etx_wind, "Solar" = etx_solar)

p13 <- ggplot(p13_data, aes(x = year, y = share, fill = source)) +
 geom_area(alpha = 0.85) +
 scale_fill_manual(values = share_colors) +
 labs(
   title = "France: The Nuclear Exception",
   subtitle = "Share of electricity generation by low-carbon source (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.x = element_text(colour = etx_sub)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p13, file.path(out_dir, "13_france_nuclear.png"))


# ============================================================
# 14 — Nordic Countries Energy Mix (Stacked Bar)
# ============================================================
nordic <- c("Norway", "Sweden", "Denmark", "Finland")

p14_data <- latest %>%
 filter(country %in% nordic) %>%
 select(country,
        Coal = coal_share_energy, Oil = oil_share_energy,
        Gas = gas_share_energy, Nuclear = nuclear_share_energy,
        Hydro = hydro_share_energy, Solar = solar_share_energy,
        Wind = wind_share_energy) %>%
 pivot_longer(-country, names_to = "fuel", values_to = "share") %>%
 filter(!is.na(share), share > 0) %>%
 mutate(fuel = factor(fuel, levels = c("Coal", "Oil", "Gas", "Nuclear",
                                        "Hydro", "Wind", "Solar")))

p14 <- ggplot(p14_data, aes(x = country, y = share, fill = fuel)) +
 geom_col(width = 0.7) +
 scale_fill_manual(values = fuel_colors) +
 labs(
   title = "Nordic Energy: Four Countries, Four Strategies",
   subtitle = glue("Primary energy mix (%) · {latest_year}"),
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.x = element_text(colour = etx_text, size = 11)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p14, file.path(out_dir, "14_nordic_energy.png"))


# ============================================================
# 15 — Denmark: The Wind Champion (Area Chart)
# ============================================================
p15_data <- energy %>%
 filter(country == "Denmark", year >= 1990, !is.na(wind_share_elec))

p15 <- ggplot(p15_data, aes(x = year, y = wind_share_elec)) +
 geom_area(fill = etx_wind, alpha = 0.6) +
 geom_line(colour = etx_wind, linewidth = 1.2) +
 geom_hline(yintercept = 50, colour = etx_accent4, linetype = "dashed") +
 annotate("text", x = 2000, y = 53, label = "50% threshold",
          colour = etx_accent4, family = "inter", size = 3.5) +
 labs(
   title = "Denmark: The Wind Champion",
   subtitle = "Wind's share of electricity generation (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p15, file.path(out_dir, "15_denmark_wind.png"))


# ============================================================
# 16 — Australia Solar Explosion
# ============================================================
p16_data <- energy %>%
 filter(country == "Australia", year >= 2008, !is.na(solar_electricity))

p16 <- ggplot(p16_data, aes(x = year, y = solar_electricity)) +
 geom_area(fill = etx_solar, alpha = 0.5) +
 geom_line(colour = etx_solar, linewidth = 1.2) +
 geom_point(data = p16_data %>% filter(year == max(year)),
            colour = etx_solar, size = 4) +
 labs(
   title = "Australia's Solar Revolution",
   subtitle = "Solar electricity generation (TWh) · 2008–present",
   caption = etx_caption(),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p16, file.path(out_dir, "16_australia_solar.png"))


# ============================================================
# 17 — Poland Coal Problem
# ============================================================
p17_data <- energy %>%
 filter(country == "Poland", year >= 1990) %>%
 select(year, Coal = coal_share_elec, Wind = wind_share_elec,
        Solar = solar_share_elec, Gas = gas_share_elec) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p17_colors <- c("Coal" = etx_coal, "Gas" = etx_gas,
               "Wind" = etx_wind, "Solar" = etx_solar)

p17 <- ggplot(p17_data, aes(x = year, y = share, colour = source)) +
 geom_line(linewidth = 1.3) +
 scale_colour_manual(values = p17_colors) +
 labs(
   title = "Poland's Coal Dilemma",
   subtitle = "Share of electricity by source (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = c(0.85, 0.5),
   legend.text = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p17, file.path(out_dir, "17_poland_coal.png"))


# ============================================================
# 18 — Norway: Hydro + Oil Paradox
# ============================================================
p18_data <- energy %>%
 filter(country == "Norway", year >= 1990) %>%
 mutate(
   clean_elec = hydro_share_elec + replace_na(wind_share_elec, 0),
   fossil_energy = fossil_share_energy
 ) %>%
 select(year, `Clean Electricity %` = clean_elec,
        `Fossil Primary Energy %` = fossil_energy) %>%
 pivot_longer(-year, names_to = "metric", values_to = "value") %>%
 filter(!is.na(value))

p18 <- ggplot(p18_data, aes(x = year, y = value, colour = metric)) +
 geom_line(linewidth = 1.3) +
 scale_colour_manual(values = c("Clean Electricity %" = etx_accent,
                                "Fossil Primary Energy %" = etx_accent3)) +
 labs(
   title = "Norway's Paradox: Clean Grid, Fossil Economy",
   subtitle = "Clean electricity share vs fossil primary energy share (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = c(0.7, 0.5),
   legend.text = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p18, file.path(out_dir, "18_norway_paradox.png"))


# ============================================================
# 19 — UK Offshore Wind Leader
# ============================================================
p19_data <- energy %>%
 filter(country == "United Kingdom", year >= 2000, !is.na(wind_electricity))

p19 <- ggplot(p19_data, aes(x = year, y = wind_electricity)) +
 geom_col(fill = etx_wind, width = 0.7) +
 labs(
   title = "United Kingdom: Offshore Wind Pioneer",
   subtitle = "Wind electricity generation (TWh) · 2000–present",
   caption = etx_caption(),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p19, file.path(out_dir, "19_uk_wind.png"))


# ============================================================
# 20 — Brazil: Renewable Champion
# ============================================================
p20_data <- energy %>%
 filter(country == "Brazil", year >= 1990) %>%
 select(year, Hydro = hydro_share_elec, Wind = wind_share_elec,
        Solar = solar_share_elec, Bio = biofuel_share_elec) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p20_colors <- c("Hydro" = etx_hydro, "Wind" = etx_wind,
               "Solar" = etx_solar, "Bio" = etx_bio)

p20 <- ggplot(p20_data, aes(x = year, y = share, fill = source)) +
 geom_area(alpha = 0.85) +
 scale_fill_manual(values = p20_colors) +
 labs(
   title = "Brazil: South America's Renewable Powerhouse",
   subtitle = "Renewable share of electricity generation (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.x = element_text(colour = etx_sub)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p20, file.path(out_dir, "20_brazil_renewable.png"))


# ============================================================
# 21 — Japan Energy Transition (Pre/Post Fukushima)
# ============================================================
p21_data <- energy %>%
 filter(country == "Japan", year >= 2000) %>%
 select(year, Nuclear = nuclear_share_elec, Solar = solar_share_elec,
        Wind = wind_share_elec, Gas = gas_share_elec, Coal = coal_share_elec) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p21_colors <- c("Nuclear" = etx_nuclear, "Solar" = etx_solar,
               "Wind" = etx_wind, "Gas" = etx_gas, "Coal" = etx_coal)

p21 <- ggplot(p21_data, aes(x = year, y = share, colour = source)) +
 geom_line(linewidth = 1.2) +
 geom_vline(xintercept = 2011, colour = etx_accent4, linetype = "dashed") +
 annotate("text", x = 2011.5, y = 35, label = "Fukushima\n2011",
          colour = etx_accent4, family = "inter", size = 3, hjust = 0) +
 scale_colour_manual(values = p21_colors) +
 labs(
   title = "Japan's Energy Crisis: Before & After Fukushima",
   subtitle = "Electricity generation share by source (%) · 2000–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = c(0.85, 0.75),
   legend.text = element_text(colour = etx_text, size = 8),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p21, file.path(out_dir, "21_japan_fukushima.png"))


# ============================================================
# 22 — Saudi Arabia: Oil Kingdom to Solar
# ============================================================
p22_data <- energy %>%
 filter(country == "Saudi Arabia", year >= 2000) %>%
 select(year, Oil = oil_share_energy, Gas = gas_share_energy,
        Solar = solar_share_energy) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p22_colors <- c("Oil" = etx_oil, "Gas" = etx_gas, "Solar" = etx_solar)

p22 <- ggplot(p22_data, aes(x = year, y = share, colour = source)) +
 geom_line(linewidth = 1.3) +
 scale_colour_manual(values = p22_colors) +
 labs(
   title = "Saudi Arabia: From Oil Kingdom to Solar?",
   subtitle = "Primary energy share by source (%) · 2000–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = c(0.85, 0.5),
   legend.text = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p22, file.path(out_dir, "22_saudi_solar.png"))


# ============================================================
# 23 — South Korea Energy Dependency
# ============================================================
p23_data <- energy %>%
 filter(country == "South Korea", year >= 1990) %>%
 select(year, Nuclear = nuclear_share_elec, Coal = coal_share_elec,
        Gas = gas_share_elec, Solar = solar_share_elec) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p23_colors <- c("Nuclear" = etx_nuclear, "Coal" = etx_coal,
               "Gas" = etx_gas, "Solar" = etx_solar)

p23 <- ggplot(p23_data, aes(x = year, y = share, fill = source)) +
 geom_area(alpha = 0.85) +
 scale_fill_manual(values = p23_colors) +
 labs(
   title = "South Korea: The Import-Dependent Giant",
   subtitle = "Electricity generation share (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.x = element_text(colour = etx_sub)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p23, file.path(out_dir, "23_south_korea.png"))


# ============================================================
# 24 — BRICS Energy Mix Comparison
# ============================================================
brics <- c("Brazil", "Russia", "India", "China", "South Africa")

p24_data <- latest %>%
 filter(country %in% brics) %>%
 select(country, Coal = coal_share_energy, Oil = oil_share_energy,
        Gas = gas_share_energy, Nuclear = nuclear_share_energy,
        Hydro = hydro_share_energy, Solar = solar_share_energy,
        Wind = wind_share_energy) %>%
 pivot_longer(-country, names_to = "fuel", values_to = "share") %>%
 filter(!is.na(share), share > 0) %>%
 mutate(fuel = factor(fuel, levels = c("Coal","Oil","Gas","Nuclear","Hydro","Wind","Solar")))

p24 <- ggplot(p24_data, aes(x = country, y = share, fill = fuel)) +
 geom_col(position = "fill", width = 0.7) +
 scale_fill_manual(values = fuel_colors) +
 scale_y_continuous(labels = percent) +
 labs(
   title = "BRICS Energy DNA",
   subtitle = glue("Primary energy mix comparison (%) · {latest_year}"),
   caption = etx_caption(),
   x = NULL, y = NULL
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.x = element_text(colour = etx_text, size = 11)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p24, file.path(out_dir, "24_brics_energy.png"))


# ============================================================
# 25 — Fossil Energy Per Capita Dumbbell: 2000 vs Latest
# ============================================================
p25_data <- energy %>%
 filter(year %in% c(2000, latest_year), !is.na(fossil_energy_per_capita)) %>%
 select(country, year, fossil_energy_per_capita) %>%
 pivot_wider(names_from = year, values_from = fossil_energy_per_capita, names_prefix = "y") %>%
 drop_na() %>%
 mutate(change = .data[[paste0("y", latest_year)]] - y2000) %>%
 slice_max(abs(change), n = 20) %>%
 mutate(country = fct_reorder(country, change))

col_latest <- paste0("y", latest_year)

p25 <- ggplot(p25_data) +
 geom_segment(aes(x = y2000, xend = .data[[col_latest]],
                  y = country, yend = country),
              colour = etx_grid, linewidth = 0.8) +
 geom_point(aes(x = y2000, y = country), colour = etx_accent3, size = 3.5) +
 geom_point(aes(x = .data[[col_latest]], y = country),
            colour = etx_accent, size = 3.5) +
 labs(
   title = "Who Cut Fossil Fuel Use? Who Didn't?",
   subtitle = glue("Fossil energy per capita change (kWh) · 2000 vs {latest_year} · Top 20 movers"),
   caption = etx_caption(),
   x = "kWh per capita", y = NULL
 ) +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 9),
       axis.text.x = element_text(colour = etx_sub))

etx_save(p25, file.path(out_dir, "25_fossil_dumbbell.png"))


# ============================================================
# 26 — Renewable Share Heatmap (15 Countries × Years)
# ============================================================
heat_countries <- c("Germany", "United Kingdom", "Spain", "France", "Italy",
                   "Denmark", "Sweden", "Norway", "Brazil", "China",
                   "India", "Japan", "United States", "Australia", "South Korea")

p26_data <- energy %>%
 filter(country %in% heat_countries, year >= 2000,
        !is.na(renewables_share_elec))

p26 <- ggplot(p26_data, aes(x = year, y = fct_rev(country),
                            fill = renewables_share_elec)) +
 geom_tile(colour = etx_bg, linewidth = 0.5) +
 scale_fill_gradient2(low = etx_accent4, mid = etx_accent6,
                     high = etx_accent, midpoint = 40,
                     limits = c(0, 100)) +
 labs(
   title = "Watching the Energy Transition Unfold",
   subtitle = "Renewable share of electricity (%) · 2000–present",
   caption = etx_caption(),
   x = NULL, y = NULL
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.title = element_text(colour = etx_text, size = 9),
   legend.text = element_text(colour = etx_sub, size = 8),
   legend.key.width = unit(2, "cm"),
   legend.key.height = unit(0.3, "cm"),
   axis.text.y = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_sub, size = 8)
 ) +
 guides(fill = guide_colorbar(title = "%", title.position = "left"))

etx_save(p26, file.path(out_dir, "26_renewable_heatmap.png"))


# ============================================================
# 27 — Electricity Treemap by Generation
# ============================================================
p27_data <- latest %>%
 filter(!is.na(electricity_generation), electricity_generation > 10) %>%
 mutate(label = paste0(country, "\n", comma(round(electricity_generation)), " TWh"))

p27 <- ggplot(p27_data, aes(area = electricity_generation,
                            fill = low_carbon_share_elec, label = label)) +
 geom_treemap(colour = etx_bg, size = 2) +
 geom_treemap_text(colour = etx_text, family = "inter", size = 9,
                   place = "centre", grow = FALSE) +
 scale_fill_gradient2(low = etx_accent4, mid = etx_accent6,
                     high = etx_accent, midpoint = 50,
                     na.value = etx_coal) +
 labs(
   title = "Who Generates the Most Electricity?",
   subtitle = glue("Size = TWh generated · Color = low-carbon share · {latest_year}"),
   caption = etx_caption()
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.title = element_text(colour = etx_text, size = 9),
   legend.text = element_text(colour = etx_sub, size = 8),
   legend.key.width = unit(2, "cm"),
   legend.key.height = unit(0.3, "cm")
 ) +
 guides(fill = guide_colorbar(title = "Low-carbon %", title.position = "left"))

etx_save(p27, file.path(out_dir, "27_electricity_treemap.png"))


# ============================================================
# 28 — Bioenergy Share — Top 15
# ============================================================
p28_data <- latest %>%
 filter(!is.na(biofuel_share_energy), biofuel_share_energy > 0) %>%
 slice_max(biofuel_share_energy, n = 15) %>%
 mutate(country = fct_reorder(country, biofuel_share_energy))

p28 <- ggplot(p28_data, aes(x = biofuel_share_energy, y = country)) +
 geom_col(fill = etx_bio, width = 0.7) +
 geom_text(aes(label = paste0(round(biofuel_share_energy, 1), "%")),
           hjust = -0.2, colour = etx_text, family = "spacegrotesk",
           fontface = "bold", size = 3.5) +
 labs(
   title = "Bioenergy Leaders",
   subtitle = glue("Biofuel share of primary energy (%) · {latest_year}"),
   caption = etx_caption(),
   x = NULL, y = NULL
 ) +
 coord_cartesian(clip = "off") +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 10))

etx_save(p28, file.path(out_dir, "28_bioenergy_share.png"))


# ============================================================
# 29 — Electricity Demand Growth Rate (2010 vs Latest)
# ============================================================
p29_data <- energy %>%
 filter(year %in% c(2010, latest_year), !is.na(electricity_demand)) %>%
 select(country, year, electricity_demand) %>%
 pivot_wider(names_from = year, values_from = electricity_demand, names_prefix = "y") %>%
 filter(!is.na(y2010), !is.na(.data[[paste0("y", latest_year)]])) %>%
 mutate(growth = (.data[[paste0("y", latest_year)]] / y2010 - 1) * 100) %>%
 filter(is.finite(growth)) %>%
 slice_max(abs(growth), n = 20) %>%
 mutate(
   country = fct_reorder(country, growth),
   bar_col = ifelse(growth > 0, etx_accent, etx_accent4)
 )

p29 <- ggplot(p29_data, aes(x = growth, y = country, fill = bar_col)) +
 geom_col(width = 0.7) +
 geom_vline(xintercept = 0, colour = etx_text, linewidth = 0.3) +
 scale_fill_identity() +
 labs(
   title = "Electricity Demand: Who's Surging, Who's Flat?",
   subtitle = glue("Change in electricity demand (%) · 2010 vs {latest_year}"),
   caption = etx_caption(),
   x = "% change", y = NULL
 ) +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 9),
       axis.text.x = element_text(colour = etx_sub))

etx_save(p29, file.path(out_dir, "29_demand_growth.png"))


# ============================================================
# 30 — Nuclear Share by Country — Bar Chart
# ============================================================
p30_data <- latest %>%
 filter(!is.na(nuclear_share_elec), nuclear_share_elec > 0) %>%
 mutate(country = fct_reorder(country, nuclear_share_elec))

p30 <- ggplot(p30_data, aes(x = nuclear_share_elec, y = country)) +
 geom_col(fill = etx_nuclear, width = 0.7) +
 geom_text(aes(label = paste0(round(nuclear_share_elec, 1), "%")),
           hjust = -0.2, colour = etx_text, family = "spacegrotesk",
           fontface = "bold", size = 3.5) +
 labs(
   title = "Nuclear Electricity: Who Relies on It?",
   subtitle = glue("Nuclear share of electricity generation (%) · {latest_year}"),
   caption = etx_caption("IAEA · Our World in Data"),
   x = NULL, y = NULL
 ) +
 coord_cartesian(clip = "off") +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 10))

etx_save(p30, file.path(out_dir, "30_nuclear_share.png"))


# ============================================================
# 31 — Chile Solar+Wind Revolution
# ============================================================
p31_data <- energy %>%
 filter(country == "Chile", year >= 2010) %>%
 select(year, Solar = solar_share_elec, Wind = wind_share_elec) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p31 <- ggplot(p31_data, aes(x = year, y = share, fill = source)) +
 geom_area(alpha = 0.8) +
 scale_fill_manual(values = c("Solar" = etx_solar, "Wind" = etx_wind)) +
 labs(
   title = "Chile: Latin America's Clean Energy Surprise",
   subtitle = "Solar & wind share of electricity (%) · 2010–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_sub)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p31, file.path(out_dir, "31_chile_clean.png"))


# ============================================================
# 32 — South Africa Coal Dependency
# ============================================================
p32_data <- energy %>%
 filter(country == "South Africa", year >= 1990, !is.na(coal_share_elec))

p32 <- ggplot(p32_data, aes(x = year, y = coal_share_elec)) +
 geom_area(fill = etx_coal, alpha = 0.6) +
 geom_line(colour = etx_coal, linewidth = 1.2) +
 labs(
   title = "South Africa: Locked Into Coal",
   subtitle = "Coal share of electricity generation (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p32, file.path(out_dir, "32_south_africa_coal.png"))


# ============================================================
# 33 — Indonesia Coal Surge
# ============================================================
p33_data <- energy %>%
 filter(country == "Indonesia", year >= 1990) %>%
 select(year, Coal = coal_share_elec, Gas = gas_share_elec,
        Hydro = hydro_share_elec) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p33_colors <- c("Coal" = etx_coal, "Gas" = etx_gas, "Hydro" = etx_hydro)

p33 <- ggplot(p33_data, aes(x = year, y = share, colour = source)) +
 geom_line(linewidth = 1.3) +
 scale_colour_manual(values = p33_colors) +
 labs(
   title = "Indonesia: Southeast Asia's Coal Challenge",
   subtitle = "Electricity share by source (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = c(0.85, 0.5),
   legend.text = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p33, file.path(out_dir, "33_indonesia_coal.png"))


# ============================================================
# 34 — Turkey Energy Profile
# ============================================================
p34_data <- energy %>%
 filter(country == "Turkey", year >= 2000) %>%
 select(year, Coal = coal_share_elec, Gas = gas_share_elec,
        Hydro = hydro_share_elec, Wind = wind_share_elec,
        Solar = solar_share_elec) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share)) %>%
 mutate(source = factor(source, levels = c("Coal","Gas","Hydro","Wind","Solar")))

p34_colors <- c("Coal" = etx_coal, "Gas" = etx_gas, "Hydro" = etx_hydro,
               "Wind" = etx_wind, "Solar" = etx_solar)

p34 <- ggplot(p34_data, aes(x = year, y = share, fill = source)) +
 geom_area(alpha = 0.85) +
 scale_fill_manual(values = p34_colors) +
 labs(
   title = "Turkey: At the Crossroads of Energy Transition",
   subtitle = "Electricity generation share (%) · 2000–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.x = element_text(colour = etx_sub)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p34, file.path(out_dir, "34_turkey_energy.png"))


# ============================================================
# 35 — Egypt Energy Growth
# ============================================================
p35_data <- energy %>%
 filter(country == "Egypt", year >= 1990, !is.na(electricity_generation))

p35 <- ggplot(p35_data, aes(x = year, y = electricity_generation)) +
 geom_area(fill = etx_accent3, alpha = 0.5) +
 geom_line(colour = etx_accent3, linewidth = 1.2) +
 labs(
   title = "Egypt: Rising Electricity Demand",
   subtitle = "Total electricity generation (TWh) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p35, file.path(out_dir, "35_egypt_growth.png"))


# ============================================================
# 36 — UAE Diversification
# ============================================================
p36_data <- energy %>%
 filter(country == "United Arab Emirates", year >= 2000) %>%
 select(year, Oil = oil_share_energy, Gas = gas_share_energy,
        Solar = solar_share_energy, Nuclear = nuclear_share_energy) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p36_colors <- c("Oil" = etx_oil, "Gas" = etx_gas,
               "Solar" = etx_solar, "Nuclear" = etx_nuclear)

p36 <- ggplot(p36_data, aes(x = year, y = share, colour = source)) +
 geom_line(linewidth = 1.3) +
 scale_colour_manual(values = p36_colors) +
 labs(
   title = "UAE: Beyond Oil — Nuclear & Solar Emerge",
   subtitle = "Primary energy share by source (%) · 2000–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = c(0.8, 0.5),
   legend.text = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p36, file.path(out_dir, "36_uae_diversification.png"))


# ============================================================
# 37 — Spain Solar Growth
# ============================================================
p37_data <- energy %>%
 filter(country == "Spain", year >= 2005, !is.na(solar_electricity))

p37 <- ggplot(p37_data, aes(x = year, y = solar_electricity)) +
 geom_area(fill = etx_solar, alpha = 0.5) +
 geom_line(colour = etx_solar, linewidth = 1.2) +
 labs(
   title = "Spain: Europe's Solar Belt",
   subtitle = "Solar electricity generation (TWh) · 2005–present",
   caption = etx_caption(),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p37, file.path(out_dir, "37_spain_solar.png"))


# ============================================================
# 38 — Italy Gas Dependency
# ============================================================
p38_data <- energy %>%
 filter(country == "Italy", year >= 1990, !is.na(gas_share_elec))

p38 <- ggplot(p38_data, aes(x = year, y = gas_share_elec)) +
 geom_area(fill = etx_gas, alpha = 0.5) +
 geom_line(colour = etx_gas, linewidth = 1.2) +
 labs(
   title = "Italy: Europe's Gas-Dependent Economy",
   subtitle = "Natural gas share of electricity generation (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p38, file.path(out_dir, "38_italy_gas.png"))


# ============================================================
# 39 — G20 Per Capita Emissions — Lollipop
# ============================================================
g20 <- c("United States", "China", "India", "Japan", "Germany", "United Kingdom",
        "France", "Brazil", "Canada", "South Korea", "Russia", "Australia",
        "Italy", "Mexico", "Indonesia", "Turkey", "Saudi Arabia", "Argentina",
        "South Africa")

p39_data <- latest %>%
 filter(country %in% g20, !is.na(fossil_energy_per_capita)) %>%
 mutate(country = fct_reorder(country, fossil_energy_per_capita))

p39 <- ggplot(p39_data, aes(x = fossil_energy_per_capita, y = country)) +
 geom_segment(aes(x = 0, xend = fossil_energy_per_capita, yend = country),
              colour = etx_grid, linewidth = 0.6) +
 geom_point(aes(colour = fossil_energy_per_capita), size = 4) +
 geom_text(aes(label = paste0(comma(round(fossil_energy_per_capita)), " kWh")),
           hjust = -0.2, colour = etx_text, family = "spacegrotesk", size = 3.3) +
 scale_colour_gradient(low = etx_accent, high = etx_accent4) +
 labs(
   title = "G20 Fossil Fuel Footprint",
   subtitle = glue("Fossil energy per capita (kWh) · {latest_year}"),
   caption = etx_caption(),
   x = NULL, y = NULL
 ) +
 coord_cartesian(clip = "off") +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 10))

etx_save(p39, file.path(out_dir, "39_g20_co2.png"))


# ============================================================
# 40 — EU Renewables Trend — Average
# ============================================================
eu <- c("Germany", "France", "Italy", "Spain", "Netherlands", "Belgium",
       "Poland", "Sweden", "Denmark", "Finland", "Austria", "Portugal",
       "Czech Republic", "Romania", "Greece", "Ireland", "Hungary")

p40_data <- energy %>%
 filter(country %in% eu, year >= 2000, !is.na(renewables_share_energy)) %>%
 group_by(year) %>%
 summarise(avg_share = mean(renewables_share_energy, na.rm = TRUE), .groups = "drop")

p40 <- ggplot(p40_data, aes(x = year, y = avg_share)) +
 geom_area(fill = etx_accent, alpha = 0.3) +
 geom_line(colour = etx_accent, linewidth = 1.5) +
 geom_point(data = p40_data %>% filter(year == max(year)),
            colour = etx_accent, size = 5) +
 labs(
   title = "Europe's Renewable Energy Trajectory",
   subtitle = "Average renewable share of primary energy across EU countries (%) · 2000–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p40, file.path(out_dir, "40_eu_renewables.png"))


# ============================================================
# 41 — Energy Per Capita — Richest vs Poorest
# ============================================================
p41_data <- latest %>%
 filter(!is.na(energy_per_capita)) %>%
 mutate(rank = row_number(desc(energy_per_capita))) %>%
 filter(rank <= 10 | rank > (n() - 10)) %>%
 mutate(
   group = ifelse(rank <= 10, "Highest", "Lowest"),
   country = fct_reorder(country, energy_per_capita)
 )

p41 <- ggplot(p41_data, aes(x = energy_per_capita, y = country,
                            fill = group)) +
 geom_col(width = 0.7) +
 scale_fill_manual(values = c("Highest" = etx_accent3, "Lowest" = etx_accent2)) +
 scale_x_continuous(labels = comma) +
 labs(
   title = "Energy Inequality: Top 10 vs Bottom 10",
   subtitle = glue("Primary energy consumption per capita (kWh) · {latest_year}"),
   caption = etx_caption(),
   x = "kWh per capita", y = NULL
 ) +
 theme_energtx() +
 theme(
   axis.text.y = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_sub),
   legend.position = c(0.8, 0.2),
   legend.text = element_text(colour = etx_text, size = 9)
 )

etx_save(p41, file.path(out_dir, "41_energy_inequality.png"))


# ============================================================
# 42 — Solar vs Wind Scatter (Latest)
# ============================================================
p42_data <- latest %>%
 filter(!is.na(solar_electricity), !is.na(wind_electricity),
        solar_electricity > 0, wind_electricity > 0)

p42 <- ggplot(p42_data, aes(x = solar_electricity, y = wind_electricity)) +
 geom_abline(slope = 1, intercept = 0, colour = etx_grid, linetype = "dashed") +
 geom_point(aes(size = electricity_generation), colour = etx_accent2, alpha = 0.7) +
 geom_text_repel(aes(label = iso3), family = "spacegrotesk",
                 colour = etx_text, size = 2.8, max.overlaps = 12) +
 scale_x_log10(labels = comma) +
 scale_y_log10(labels = comma) +
 scale_size_continuous(range = c(2, 12), guide = "none") +
 annotate("text", x = 0.5, y = 300, label = "More Wind →",
          colour = etx_wind, family = "inter", size = 3, angle = 0) +
 annotate("text", x = 300, y = 0.5, label = "More Solar →",
          colour = etx_solar, family = "inter", size = 3, angle = 0) +
 labs(
   title = "Solar vs Wind: Who Bets on What?",
   subtitle = glue("Solar vs wind generation (TWh, log scale) · {latest_year}"),
   caption = etx_caption(),
   x = "Solar (TWh)", y = "Wind (TWh)"
 ) +
 theme_energtx() +
 theme(
   panel.grid.major.x = element_line(colour = etx_grid, linewidth = 0.3),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p42, file.path(out_dir, "42_solar_vs_wind.png"))


# ============================================================
# 43 — Fossil Fuel Share Decline Over Time (Global)
# ============================================================
p43_data <- raw %>%
 filter(country == "World", year >= 1990, !is.na(fossil_share_energy))

p43 <- ggplot(p43_data, aes(x = year, y = fossil_share_energy)) +
 geom_area(fill = etx_accent4, alpha = 0.3) +
 geom_line(colour = etx_accent4, linewidth = 1.5) +
 geom_hline(yintercept = 80, colour = etx_accent3, linetype = "dashed") +
 annotate("text", x = 1995, y = 81.5, label = "80% threshold",
          colour = etx_accent3, family = "inter", size = 3) +
 labs(
   title = "Is the World Moving Away from Fossil Fuels?",
   subtitle = "Global fossil fuel share of primary energy (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p43, file.path(out_dir, "43_global_fossil_decline.png"))


# ============================================================
# 44 — Global Solar + Wind Combined Growth
# ============================================================
p44_data <- raw %>%
 filter(country == "World", year >= 2000) %>%
 select(year, Solar = solar_electricity, Wind = wind_electricity) %>%
 pivot_longer(-year, names_to = "source", values_to = "TWh") %>%
 filter(!is.na(TWh))

p44 <- ggplot(p44_data, aes(x = year, y = TWh, fill = source)) +
 geom_area(alpha = 0.85) +
 scale_fill_manual(values = c("Solar" = etx_solar, "Wind" = etx_wind)) +
 scale_y_continuous(labels = comma) +
 labs(
   title = "The Unstoppable Rise of Solar & Wind",
   subtitle = "Global electricity generation from solar + wind (TWh) · 2000–present",
   caption = etx_caption(),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_sub)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p44, file.path(out_dir, "44_solar_wind_global.png"))


# ============================================================
# 45 — Continent-Level Comparison (Grouped Bar)
# ============================================================
p45_data <- latest %>%
 filter(!is.na(continent), !is.na(renewables_share_energy)) %>%
 group_by(continent) %>%
 summarise(
   avg_renewable = mean(renewables_share_energy, na.rm = TRUE),
   avg_fossil = mean(fossil_share_energy, na.rm = TRUE),
   .groups = "drop"
 ) %>%
 pivot_longer(-continent, names_to = "type", values_to = "share") %>%
 mutate(type = ifelse(type == "avg_renewable", "Renewable", "Fossil"))

p45 <- ggplot(p45_data, aes(x = continent, y = share, fill = type)) +
 geom_col(position = "dodge", width = 0.7) +
 scale_fill_manual(values = c("Renewable" = etx_accent, "Fossil" = etx_accent4)) +
 labs(
   title = "Renewable vs Fossil: A Continental Divide",
   subtitle = glue("Average energy share by continent (%) · {latest_year}"),
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_text, size = 10)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p45, file.path(out_dir, "45_continent_comparison.png"))


# ============================================================
# 46 — Canada Hydro + Nuclear Mix
# ============================================================
p46_data <- energy %>%
 filter(country == "Canada", year >= 1990) %>%
 select(year, Hydro = hydro_share_elec, Nuclear = nuclear_share_elec,
        Wind = wind_share_elec, Solar = solar_share_elec) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p46_colors <- c("Hydro" = etx_hydro, "Nuclear" = etx_nuclear,
               "Wind" = etx_wind, "Solar" = etx_solar)

p46 <- ggplot(p46_data, aes(x = year, y = share, fill = source)) +
 geom_area(alpha = 0.85) +
 scale_fill_manual(values = p46_colors) +
 labs(
   title = "Canada: Hydro Giant with Nuclear Backbone",
   subtitle = "Low-carbon share of electricity generation (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.x = element_text(colour = etx_sub)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p46, file.path(out_dir, "46_canada_clean.png"))


# ============================================================
# 47 — Sweden Clean Electricity
# ============================================================
p47_data <- energy %>%
 filter(country == "Sweden", year >= 1990) %>%
 select(year, Hydro = hydro_share_elec, Nuclear = nuclear_share_elec,
        Wind = wind_share_elec, Solar = solar_share_elec,
        Bio = biofuel_share_elec) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p47_colors <- c("Hydro" = etx_hydro, "Nuclear" = etx_nuclear,
               "Wind" = etx_wind, "Solar" = etx_solar, "Bio" = etx_bio)

p47 <- ggplot(p47_data, aes(x = year, y = share, fill = source)) +
 geom_area(alpha = 0.85) +
 scale_fill_manual(values = p47_colors) +
 labs(
   title = "Sweden: Near-100% Clean Electricity",
   subtitle = "Low-carbon share of electricity generation (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.x = element_text(colour = etx_sub)
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p47, file.path(out_dir, "47_sweden_clean.png"))


# ============================================================
# 48 — Netherlands Gas Phase-Out
# ============================================================
p48_data <- energy %>%
 filter(country == "Netherlands", year >= 1990, !is.na(gas_share_energy))

p48 <- ggplot(p48_data, aes(x = year, y = gas_share_energy)) +
 geom_area(fill = etx_gas, alpha = 0.5) +
 geom_line(colour = etx_gas, linewidth = 1.2) +
 labs(
   title = "Netherlands: Weaning Off Natural Gas",
   subtitle = "Natural gas share of primary energy (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p48, file.path(out_dir, "48_netherlands_gas.png"))


# ============================================================
# 49 — Mexico Energy Reform
# ============================================================
p49_data <- energy %>%
 filter(country == "Mexico", year >= 2000) %>%
 select(year, Oil = oil_share_energy, Gas = gas_share_energy,
        Solar = solar_share_energy, Wind = wind_share_energy) %>%
 pivot_longer(-year, names_to = "source", values_to = "share") %>%
 filter(!is.na(share))

p49_colors <- c("Oil" = etx_oil, "Gas" = etx_gas,
               "Solar" = etx_solar, "Wind" = etx_wind)

p49 <- ggplot(p49_data, aes(x = year, y = share, colour = source)) +
 geom_line(linewidth = 1.3) +
 scale_colour_manual(values = p49_colors) +
 labs(
   title = "Mexico: Fossil Giant with Untapped Renewables",
   subtitle = "Primary energy share by source (%) · 2000–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(
   legend.position = c(0.8, 0.5),
   legend.text = element_text(colour = etx_text, size = 9),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p49, file.path(out_dir, "49_mexico_energy.png"))


# ============================================================
# 50 — Argentina Gas Potential
# ============================================================
p50_data <- energy %>%
 filter(country == "Argentina", year >= 1990, !is.na(gas_share_energy))

p50 <- ggplot(p50_data, aes(x = year, y = gas_share_energy)) +
 geom_area(fill = etx_gas, alpha = 0.5) +
 geom_line(colour = etx_gas, linewidth = 1.2) +
 labs(
   title = "Argentina: South America's Gas Powerhouse",
   subtitle = "Natural gas share of primary energy (%) · 1990–present",
   caption = etx_caption(),
   x = NULL, y = "%"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p50, file.path(out_dir, "50_argentina_gas.png"))


# ============================================================
# 51 — Global Nuclear Renaissance
# ============================================================
p51_data <- raw %>%
 filter(country == "World", year >= 1970, !is.na(nuclear_electricity))

p51 <- ggplot(p51_data, aes(x = year, y = nuclear_electricity)) +
 geom_area(fill = etx_nuclear, alpha = 0.5) +
 geom_line(colour = etx_nuclear, linewidth = 1.2) +
 geom_vline(xintercept = c(1986, 2011), colour = etx_accent4,
            linetype = "dashed", linewidth = 0.5) +
 annotate("text", x = 1987, y = max(p51_data$nuclear_electricity, na.rm = TRUE) * 0.9,
          label = "Chernobyl", colour = etx_accent4, family = "inter",
          size = 3, hjust = 0) +
 annotate("text", x = 2012, y = max(p51_data$nuclear_electricity, na.rm = TRUE) * 0.9,
          label = "Fukushima", colour = etx_accent4, family = "inter",
          size = 3, hjust = 0) +
 scale_y_continuous(labels = comma) +
 labs(
   title = "Nuclear Power: Rise, Fall, and Renaissance",
   subtitle = "Global nuclear electricity generation (TWh) · 1970–present",
   caption = etx_caption("IAEA · Our World in Data"),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p51, file.path(out_dir, "51_nuclear_global.png"))


# ============================================================
# 52 — Global Coal Consumption Timeline
# ============================================================
p52_data <- raw %>%
 filter(country == "World", year >= 1965, !is.na(coal_consumption))

p52 <- ggplot(p52_data, aes(x = year, y = coal_consumption)) +
 geom_area(fill = etx_coal, alpha = 0.6) +
 geom_line(colour = etx_coal, linewidth = 1.2) +
 scale_y_continuous(labels = comma) +
 labs(
   title = "Global Coal: Still Growing Despite Climate Pledges",
   subtitle = "World coal consumption (TWh) · 1965–present",
   caption = etx_caption("Energy Institute"),
   x = NULL, y = "TWh"
 ) +
 theme_energtx() +
 theme(axis.text.x = element_text(colour = etx_sub))

etx_save(p52, file.path(out_dir, "52_global_coal.png"))


# ============================================================
# 53 — Renewables Growth Rate — Top 15 Fastest
# ============================================================
p53_data <- energy %>%
 filter(year %in% c(2015, latest_year), !is.na(renewables_share_energy)) %>%
 select(country, year, renewables_share_energy) %>%
 pivot_wider(names_from = year, values_from = renewables_share_energy, names_prefix = "y") %>%
 filter(!is.na(y2015), !is.na(.data[[paste0("y", latest_year)]])) %>%
 mutate(growth_pp = .data[[paste0("y", latest_year)]] - y2015) %>%
 slice_max(growth_pp, n = 15) %>%
 mutate(country = fct_reorder(country, growth_pp))

p53 <- ggplot(p53_data, aes(x = growth_pp, y = country)) +
 geom_col(fill = etx_accent, width = 0.7) +
 geom_text(aes(label = paste0("+", round(growth_pp, 1), " pp")),
           hjust = -0.2, colour = etx_text, family = "spacegrotesk",
           fontface = "bold", size = 3.5) +
 labs(
   title = "Fastest Renewable Energy Growth",
   subtitle = glue("Change in renewable share (percentage points) · 2015 vs {latest_year}"),
   caption = etx_caption(),
   x = NULL, y = NULL
 ) +
 coord_cartesian(clip = "off") +
 theme_energtx() +
 theme(axis.text.y = element_text(colour = etx_text, size = 10))

etx_save(p53, file.path(out_dir, "53_renewables_growth.png"))


# ============================================================
# 54 — Global Primary Energy Mix (Stream Chart)
# ============================================================
p54_data <- raw %>%
 filter(country == "World", year >= 1965) %>%
 select(year,
        Coal = coal_consumption, Oil = oil_consumption,
        Gas = gas_consumption, Nuclear = nuclear_consumption,
        Hydro = hydro_consumption, Solar = solar_consumption,
        Wind = wind_consumption) %>%
 pivot_longer(-year, names_to = "fuel", values_to = "value") %>%
 filter(!is.na(value)) %>%
 mutate(fuel = factor(fuel, levels = c("Coal","Oil","Gas","Nuclear","Hydro","Wind","Solar")))

p54 <- ggplot(p54_data, aes(x = year, y = value, fill = fuel)) +
 geom_area(alpha = 0.85) +
 scale_fill_manual(values = fuel_colors) +
 scale_y_continuous(labels = comma) +
 labs(
   title = "60 Years of Global Energy: A Stream",
   subtitle = "World primary energy consumption by source · 1965–present",
   caption = etx_caption("Energy Institute"),
   x = NULL, y = NULL
 ) +
 theme_energtx() +
 theme(
   legend.position = "bottom",
   legend.text = element_text(colour = etx_text, size = 9),
   legend.key.size = unit(0.4, "cm"),
   axis.text.x = element_text(colour = etx_sub),
   axis.text.y = element_blank()
 ) +
 guides(fill = guide_legend(nrow = 1, title = NULL))

etx_save(p54, file.path(out_dir, "54_global_stream.png"))


# ============================================================
# 55 — CO₂ Per Capita vs Renewable Share (Scatter)
# ============================================================
p55_data <- latest %>%
 filter(!is.na(fossil_energy_per_capita), !is.na(renewables_share_energy))

p55 <- ggplot(p55_data, aes(x = renewables_share_energy, y = fossil_energy_per_capita)) +
 geom_point(aes(size = population), colour = etx_accent5, alpha = 0.7) +
 geom_text_repel(aes(label = iso3), family = "spacegrotesk",
                 colour = etx_text, size = 2.8, max.overlaps = 15) +
 geom_smooth(method = "lm", se = FALSE, colour = etx_accent4,
             linetype = "dashed", linewidth = 0.6) +
 scale_size_continuous(range = c(2, 14), guide = "none") +
 labs(
   title = "More Renewables = Less Fossil Dependency?",
   subtitle = glue("Renewable share vs fossil energy per capita · {latest_year}"),
   caption = etx_caption(),
   x = "Renewable share of energy (%)", y = "Fossil energy per capita (kWh)"
 ) +
 theme_energtx() +
 theme(
   panel.grid.major.x = element_line(colour = etx_grid, linewidth = 0.3),
   axis.text.x = element_text(colour = etx_sub)
 )

etx_save(p55, file.path(out_dir, "55_renewables_vs_co2.png"))


# ============================================================
cat("\n========================================\n")
cat("55 infographics generated!\n")
cat("Output:", out_dir, "\n")
cat("========================================\n")
