#!/usr/bin/env Rscript
# ============================================================
# energtx.com ulke sayfalarindan veri hasadi
# Kaynak: https://energtx.com/country/<slug>  (Next.js RSC payload)
# Cikti : energtx_country_data.csv  (tidy: country, indicator, year, value, unit)
# ============================================================
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(readr); library(purrr) })
root <- path.expand("~/Desktop/8_10_2026_energtx_radial"); setwd(root)
dir.create("html", showWarnings = FALSE)

slugs <- readLines("slugs.txt") |> str_trim() |> (\(x) x[nzchar(x)])()
cat("ulke sayisi:", length(slugs), "\n")

# ---- 1. indir (yerelde varsa tekrar indirme) ----
for (s in slugs) {
  f <- file.path("html", paste0(s, ".html"))
  if (file.exists(f) && file.size(f) > 50000) next
  ok <- try(curl::curl_download(paste0("https://energtx.com/country/", s), f, quiet = TRUE), silent = TRUE)
  if (inherits(ok, "try-error")) cat("HATA:", s, "\n") else cat(".", sep = "")
  Sys.sleep(0.25)
}
cat("\nindirme bitti\n")

# ---- 2. RSC yukunden zaman serisi kayitlarini ayikla ----
pat <- 'indicator_name\\\\":\\\\"(.*?)\\\\",\\\\"year\\\\":\\\\"(\\d{4})\\\\",\\\\"value\\\\":\\\\"([-0-9.eE]*)\\\\",\\\\"unit\\\\":\\\\"(.*?)\\\\"'

parse_one <- function(s) {
  f <- file.path("html", paste0(s, ".html"))
  if (!file.exists(f)) return(NULL)
  h <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "")
  m <- str_match_all(h, pat)[[1]]
  if (nrow(m) == 0) return(NULL)
  # ulke adi <title> etiketinden
  nm <- str_match(h, "<title>([^<|—]+)")[, 2] |> str_trim()
  tibble(slug = s, country = nm, indicator = m[, 2], year = as.integer(m[, 3]),
         value = suppressWarnings(as.numeric(m[, 4])), unit = m[, 5])
}

dat <- map_dfr(slugs, parse_one) |> filter(!is.na(value)) |> distinct()
cat("kayit:", nrow(dat), "| ulke:", n_distinct(dat$slug), "| gosterge:", n_distinct(dat$indicator), "\n")
write_csv(dat, "energtx_country_data.csv")
cat("yazildi: energtx_country_data.csv\n")
