# Spec-driven chart batches (June 2026)

These charts were produced with the spec-driven renderer **`../07_spec_generate.R`**
(reads a `specs.json`, renders each entry to a 3600×2025 WebP in the energtx dark
theme) and published with **`../post_spec_batches.py`** (atproto → @energtx.bsky.social).

Data source: Our World in Data energy dataset (`owid-energy-data.csv`, gitignored).

## Reproduce

```bash
# render one batch (root must contain data/owid-energy-data.csv + specs.json + images/)
Rscript 07_spec_generate.R /path/to/workdir
# post the rendered images + captions
python3 post_spec_batches.py --send --from 1 --to 5
```

## Spec → PNG mapping

Each spec file's internal `id` maps to the published `png/` filename:

**`2026-06_batch1_global.json`** — global energy transition
- 001 → `png/151_solar_share_decade.png`
- 002 → `png/152_fossil_share_change.png`
- 003 → `png/153_renewable_elec_leaders.png`
- 004 → `png/154_europe_coal_phaseout.png`
- 005 → `png/155_g7_energy_mix_1990_2023.png`

**`2026-06_batch2_formats.json`** — richer chart types (heatmap/slope/scatter/stacked/line)
- 006 → `png/156_europe_wind_heatmap.png`
- 007 → `png/157_nuclear_share_split.png`
- 008 → `png/158_energy_vs_renewable_scatter.png`
- 009 → `png/159_g20_electricity_stack.png`
- 010 → `png/160_grid_carbon_intensity.png`

**`2026-06_batch3_owid.json`** — fresh OWID angles
- 161 → `png/161_electrification_of_energy.png`
- 162 → `png/162_lowcarbon_elec_leaders.png`
- 163 → `png/163_energy_productivity_change.png`
- 164 → `png/164_gas_bridge_slope.png`
- 165 → `png/165_asia_electricity_surge.png`

**`2026-06_batch4_scale.json`** — absolute energy scale (raw TWh, not shares)
- 166 → `png/166_largest_energy_consumers.png`
- 167 → `png/167_total_energy_china_overtakes.png`
- 168 → `png/168_energy_demand_change.png`
- 169 → `png/169_largest_electricity_producers.png`
- 170 → `png/170_energy_vs_economy_scatter.png`

Post captions for all of the above are in `../03_post_texts.md`.
