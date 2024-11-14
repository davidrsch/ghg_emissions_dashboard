box::use(
  dplyr[pull, select],
  janitor[clean_names],
  readxl[read_excel],
  stringr[str_replace],
)

ghg_data_path <- "app/static/data/edgar_2024_ghg_booklet_2024.xlsx"
#' @export
ghg_totals_by_country <- read_excel(
  ghg_data_path,
  "ghg_totals_by_country"
) |>
  clean_names()

#' @export
ghg_by_sector_and_country <- read_excel(
  ghg_data_path,
  "ghg_by_sector_and_country"
) |>
  clean_names()

#' @export
ghg_per_gdp_by_country <- read_excel(
  ghg_data_path,
  "ghg_per_gdp_by_country"
) |>
  clean_names()

#' @export
ghg_per_capita_by_country <- read_excel(
  ghg_data_path,
  "ghg_per_capita_by_country"
) |>
  clean_names()

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
  select(edgar_country_code) |>
  pull()

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
