# energtx.com site pipeline (charts 196–200)

These five charts are **not** built from the OWID CSV in the repo root. They come from
the energtx.com website itself.

## Pipeline

1. `01_scrape.R` downloads all 106 `https://energtx.com/country/<slug>` pages and parses
   the embedded Next.js RSC payload into a tidy table:
   **235,061 records · 106 countries · 174 indicators · 1990–2025**
   → `energtx_country_data.csv` (not committed; ~20 MB, regenerate with the script)
2. `02_fingerprints.R` → 196 — Nightingale roses of the 2024 electricity mix
3. `03_originals.R` → 197 connected-scatter, 198 ridgeline (and a first-pass bump)
4. `04_bump_pro.R` → 199 — sigmoid bump chart with size encoding
5. `05_poster.R` → 200 — composed poster: KPI strip + 24 stacked-area panels

Scripts were run from `~/Desktop/8_10_2026_energtx_radial`; `root <-` at the top of each
file points there. Change it, or run them from a folder containing the scraped CSV.

## Data notes

- Electricity-mix series (`Share of electricity from <source> (%)`) are attributed to
  **Ember** on the site. Shares sum to 100% per country-year (99.98–100.01 verified).
- **2024**, not 2025: the mix is complete for 105 countries in 2024 but only 77 in 2025.
- Country counts in the poster KPI strip are unweighted — Luxembourg counts the same
  as China.
- Chart 190 in this repo says 22 countries are above 25% wind+solar in 2024; chart 200
  says 20. Not a contradiction — 190 uses OWID's wider country list, 200 uses the
  105 countries energtx covers.
