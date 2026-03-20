# ============================================================
# energtx Infographic Generator — Data Download & Prep
# ============================================================

source("C:/Users/bartu/Desktop/energtx-infographics/00_setup.R")

# -- Download OWID Energy Data --
data_file <- "C:/Users/bartu/Desktop/energtx-infographics/owid-energy-data.csv"
if (!file.exists(data_file)) {
 download.file(
   "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv",
   data_file, mode = "wb"
 )
 cat("Downloaded OWID energy data.\n")
}

raw <- read_csv(data_file, show_col_types = FALSE)

# -- 56 energtx countries --
etx_countries <- c(
 "United States", "China", "India", "Japan", "Germany", "United Kingdom",
 "France", "Brazil", "Canada", "South Korea", "Russia", "Australia",
 "Italy", "Spain", "Mexico", "Indonesia", "Turkey", "Saudi Arabia",
 "Netherlands", "Switzerland", "Sweden", "Norway", "Poland", "Belgium",
 "Argentina", "South Africa", "Nigeria", "Egypt", "Thailand", "Vietnam",
 "Malaysia", "Philippines", "Pakistan", "Bangladesh", "Chile", "Colombia",
 "Iran", "Iraq", "United Arab Emirates", "Israel", "Denmark", "Finland",
 "Austria", "Portugal", "Czech Republic", "Romania", "Greece", "Ireland",
 "New Zealand", "Hungary", "Ukraine", "Kazakhstan", "Peru", "Morocco",
 "Kenya", "Algeria"
)

# -- Filter & clean --
energy <- raw %>%
 filter(country %in% etx_countries, year >= 1990) %>%
 mutate(
   iso3 = countrycode(country, "country.name", "iso3c"),
   continent = countrycode(country, "country.name", "continent")
 )

# -- Latest year with good data --
latest_year <- energy %>%
 filter(!is.na(primary_energy_consumption)) %>%
 pull(year) %>%
 max(na.rm = TRUE)

latest <- energy %>%
 filter(year == latest_year)

cat("Data ready:", nrow(energy), "rows,", latest_year, "latest year.\n")
