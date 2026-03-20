# energtx — Global Energy Data Infographics

<p align="center">
  <a href="https://energtx.com"><img src="https://img.shields.io/badge/energtx.com-Visit%20Platform-00E5A0?style=for-the-badge" alt="energtx.com"></a>
  <a href="https://bsky.app/profile/energtx.bsky.social"><img src="https://img.shields.io/badge/Bluesky-@energtx-0085FF?style=for-the-badge&logo=bluesky" alt="Bluesky"></a>
  <a href="mailto:info@energtx.com"><img src="https://img.shields.io/badge/Contact-info@energtx.com-E2E8F0?style=for-the-badge" alt="Email"></a>
</p>

**100 infographic visualizations** of global energy data — electricity, renewables, CO₂, fossil fuels, and energy transitions across **56 countries**.

Built with **R + ggplot2** using data from [Our World in Data](https://github.com/owid/energy-data). All visuals use the energtx dark theme.

---

## Gallery

### Country Comparisons

| | | |
|---|---|---|
| ![G7 Energy Mix](png/09_g7_energy_mix.png) | ![BRICS Energy](png/24_brics_energy.png) | ![EU vs US vs China](png/68_eu_us_china_mix.png) |
| G7 Energy Mix | BRICS Energy DNA | EU vs US vs China |
| ![Nordic Energy](png/14_nordic_energy.png) | ![Middle East](png/71_middle_east_mix.png) | ![Southeast Asia](png/72_southeast_asia.png) |
| Nordic Countries | Middle East Petrostates | Southeast Asia |
| ![Latin America](png/73_latin_america.png) | ![G20 Fossil vs Renewable](png/64_g20_fossil_vs_renewable.png) | ![Continent Comparison](png/45_continent_comparison.png) |
| Latin America | G20: Fossil vs Renewable | Continent Comparison |

### Renewable Energy

| | | |
|---|---|---|
| ![Solar Growth](png/07_solar_growth.png) | ![Wind Growth](png/08_wind_growth.png) | ![Solar vs Wind](png/42_solar_vs_wind.png) |
| Solar Explosion (Top 10) | Wind Power Surge (Top 10) | Solar vs Wind: Who Bets on What? |
| ![Renewable Heatmap](png/26_renewable_heatmap.png) | ![Solar+Wind Global](png/44_solar_wind_global.png) | ![Renewables Growth](png/53_renewables_growth.png) |
| Renewable Share Heatmap | Global Solar+Wind Rise | Fastest Renewable Growth |
| ![Denmark Wind](png/15_denmark_wind.png) | ![Australia Solar](png/16_australia_solar.png) | ![Chile Clean](png/31_chile_clean.png) |
| Denmark: Wind Champion | Australia: Solar Revolution | Chile: Solar+Wind Surprise |

### Fossil Fuels & Emissions

| | | |
|---|---|---|
| ![Fossil Dependency](png/06_fossil_dependency.png) | ![Global Fossil Decline](png/43_global_fossil_decline.png) | ![Global Coal](png/52_global_coal.png) |
| Most Fossil Dependent | Global Fossil Share | Global Coal: Still Growing |
| ![Coal Phase-Out](png/70_coal_phaseout_4.png) | ![Coal China India](png/98_coal_china_india_rest.png) | ![GHG Top 15](png/87_ghg_top15.png) |
| Coal Phase-Out: 4 Speeds | Coal: China+India vs World | GHG Emitters Ranked |

### Nuclear Energy

| | | |
|---|---|---|
| ![Nuclear Global](png/51_nuclear_global.png) | ![France Nuclear](png/13_france_nuclear.png) | ![Nuclear Race](png/60_nuclear_race.png) |
| Nuclear: Rise, Fall, Renaissance | France: The Nuclear Exception | Nuclear Race: 4 Strategies |
| ![Nuclear Heatmap](png/99_nuclear_heatmap.png) | ![Japan Fukushima](png/21_japan_fukushima.png) | ![Nuclear Share](png/30_nuclear_share.png) |
| Nuclear Heatmap (25 Years) | Japan: Before & After Fukushima | Nuclear Share by Country |

### Country Profiles

| | | |
|---|---|---|
| ![US Energy](png/12_us_energy_evolution.png) | ![China Energy](png/92_china_energy_mix.png) | ![India Energy](png/91_india_energy_mix.png) |
| How America Powers Itself | China's Transformation | India: Coal Under Pressure |
| ![Turkey](png/34_turkey_energy.png) | ![UK Coal](png/90_uk_coal_collapse.png) | ![Norway Paradox](png/18_norway_paradox.png) |
| Turkey: Energy Crossroads | UK: How to Kill Coal | Norway: Clean Grid, Fossil Economy |
| ![Saudi Solar](png/22_saudi_solar.png) | ![Russia](png/93_russia_energy_mix.png) | ![Brazil](png/20_brazil_renewable.png) |
| Saudi Arabia: Oil to Solar? | Russia: Gas Superpower | Brazil: Renewable Powerhouse |

### Rankings & Analysis

| | | |
|---|---|---|
| ![Electricity Treemap](png/27_electricity_treemap.png) | ![Continent Treemap](png/100_continent_treemap.png) | ![Carbon Intensity](png/84_carbon_intensity_heatmap.png) |
| Electricity Generation Treemap | Electricity by Continent | Carbon Intensity Heatmap |
| ![GDP vs Electricity](png/10_gdp_vs_electricity.png) | ![Energy Inequality](png/41_energy_inequality.png) | ![China vs World Solar](png/85_china_vs_world_solar.png) |
| Wealth vs Electricity | Energy Inequality: 50x Gap | China vs World: Solar |
| ![Fossil Heatmap](png/75_fossil_heatmap.png) | ![Hydro vs Nuclear](png/66_hydro_vs_nuclear.png) | ![Oil Prod vs Cons](png/67_oil_prod_vs_cons.png) |
| Fossil Dependency Heatmap | Hydro vs Nuclear Baseload | Oil: Producers vs Consumers |

---

## Data

- **Source:** [Our World in Data — Energy Dataset](https://github.com/owid/energy-data)
- **Coverage:** 56 countries, 130+ indicators, 170,000+ records
- **Time Range:** 1990–2024

## Theme

All charts use the **energtx dark theme**:

| Element | Color |
|---------|-------|
| Background | `#0B1120` |
| Panel | `#0F172A` |
| Grid | `#1E293B` |
| Text | `#E2E8F0` |
| Accent | `#00E5A0` |

Fonts: **Space Grotesk** (titles) + **Inter** (body)

## Chart Types

- Bar charts & lollipop charts (rankings)
- Line charts (time series comparisons)
- Stacked area charts (energy mix evolution)
- Heatmaps (multi-country × year matrices)
- Treemaps (proportional generation)
- Scatter plots (correlation analysis)
- Dumbbell charts (before/after comparisons)
- Grouped bar charts (head-to-head)

## R Packages

```r
tidyverse, ggplot2, showtext, ggtext, scales,
ggrepel, treemapify, patchwork, countrycode, glue
```

## File Structure

```
energtx-infographics/
├── 00_setup.R          # Theme, fonts, colors, helpers
├── 01_data.R           # OWID data download & prep
├── 02_generate.R       # Batch 1: Charts 01-55
├── 04_batch2.R         # Batch 2: Charts 56-100
├── 03_post_texts.md    # Bluesky post captions
├── post_to_bsky.py     # Bluesky posting script
└── png/                # 100 infographic PNGs (1200×675)
```

## Links

- **Platform:** [energtx.com](https://energtx.com)
- **Datasets:** [energtx.com/datasets](https://energtx.com/datasets)
- **Visualizations:** [energtx.com/visualizations](https://energtx.com/visualizations)
- **Blog:** [energtx.com/blog](https://energtx.com/blog)
- **Bluesky:** [@energtx.bsky.social](https://bsky.app/profile/energtx.bsky.social)
- **Contact:** [info@energtx.com](mailto:info@energtx.com)

---

<p align="center">
  <b>energtx.com</b> · info@energtx.com<br>
  Global energy data platform — electricity, renewables, CO₂, energy prices across 56 countries
</p>
