box::use(
  dplyr[arrange, coalesce, filter, mutate, pull, rename, right_join, select],
  janitor[clean_names],
  readxl[read_excel],
  stringr[str_replace],
  tibble[tibble],
)

ghg_data_path <- "app/static/data/edgar_2024_ghg_booklet_2024.xlsx"
country_codes_change <- tibble(
  cc = c("CHE", "ESP", "FRA", "ISR", "ITA", "SDN"),
  country = c("Switzerland", "Spain", "France", "Israel", "Italy", "Sudan")
)

#' @export
continents <- read_excel("app/static/data/country_and_continent_codes.xlsx")

#' @export
ghg_totals_by_country <- read_excel(
  ghg_data_path,
  "ghg_totals_by_country"
) |>
  clean_names()

#' @export
ghg_totals_globe_map <- country_codes_change |>
  right_join(
    ghg_totals_by_country |>
      rename(cc = edgar_country_code),
    by = "cc"
  ) |>
  mutate(country = coalesce(country.x, country.y)) |>
  select(-country.x, -country.y) |>
  arrange(cc) |>
  filter(!cc %in% c("AIR", "SEA", "EU27", "GLB"))

#' @export
ghg_by_sector_and_country <- read_excel(
  ghg_data_path,
  "ghg_by_sector_and_country"
) |>
  clean_names()

#' @export
ghg_sector_globe_map <- country_codes_change |>
  right_join(
    ghg_by_sector_and_country |>
      rename(cc = edgar_country_code),
    by = "cc"
  ) |>
  mutate(country = coalesce(country.x, country.y)) |>
  select(-country.x, -country.y) |>
  arrange(cc) |>
  filter(!cc %in% c("AIR", "SEA", "EU27", "GLB"))

#' @export
ghg_per_gdp_by_country <- read_excel(
  ghg_data_path,
  "ghg_per_gdp_by_country"
) |>
  clean_names()

#' @export
ghg_gdp_globe_map <- country_codes_change |>
  right_join(
    ghg_per_gdp_by_country |>
      rename(cc = edgar_country_code),
    by = "cc"
  ) |>
  mutate(country = coalesce(country.x, country.y)) |>
  select(-country.x, -country.y) |>
  arrange(cc) |>
  filter(!cc %in% c("AIR", "SEA", "EU27", "GLB"))

#' @export
ghg_per_capita_by_country <- read_excel(
  ghg_data_path,
  "ghg_per_capita_by_country"
) |>
  clean_names()

#' @export
ghg_capita_globe_map <- country_codes_change |>
  right_join(
    ghg_per_capita_by_country |>
      rename(cc = edgar_country_code),
    by = "cc"
  ) |>
  mutate(country = coalesce(country.x, country.y)) |>
  select(-country.x, -country.y) |>
  arrange(cc) |>
  filter(!cc %in% c("AIR", "SEA", "EU27", "GLB"))

#' @export
lulucf_macroregions <- read_excel(
  ghg_data_path,
  "lulucf_macroregions"
) |>
  clean_names()

#' @export
ghg_tspc_years <- ghg_totals_by_country |>
  select(- edgar_country_code, - country) |>
  names() |>
  str_replace("x", "")

#' @export
ghg_gdplulucf_years <- ghg_per_gdp_by_country |>
  select(- edgar_country_code, - country) |>
  names() |>
  str_replace("x", "")

#' @export
edgar_cc <- ghg_totals_by_country |>
  select(edgar_country_code, country) |>
  rename(cc = edgar_country_code)

#' @export
globe_cc <- country_codes_change |>
  right_join(edgar_cc, by = "cc") |>
  mutate(country = coalesce(country.x, country.y)) |>
  select(-country.x, -country.y) |>
  arrange(cc) |>
  filter(!cc %in% c("AIR", "SEA", "EU27", "GLB")) |>
  right_join(continents, by = "cc")

#' @export
sectors <- ghg_by_sector_and_country |>
  select(sector) |>
  unique() |>
  pull()

#' @export
substances <- ghg_by_sector_and_country |>
  select(substance) |>
  unique() |>
  pull()
