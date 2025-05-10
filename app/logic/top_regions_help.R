box::use(
  dplyr[across, arrange, bind_rows, desc, filter, group_by, mutate, rename],
  dplyr[select, summarise, ungroup],
  stats[na.omit],
  stringr[str_replace],
  tidyr[contains, matches, pivot_longer],
)

box::use(
  app / logic / data[ghg_by_sector_and_country],
  app / logic / data[ghg_per_capita_by_country],
  app / logic / data[ghg_per_gdp_by_country],
  app / logic / data[ghg_totals_by_country],
)

#' @export
get_regions_emissions <- function(
  arrange_regions,
  actual_year,
  selected_sector,
  selected_substance,
  total_data = ghg_totals_by_country,
  capita_data = ghg_per_capita_by_country,
  gpd_data = ghg_per_gdp_by_country,
  sector_substance_data = ghg_by_sector_and_country
) {
  if (arrange_regions == "Total emissions") {
    data_to <- total_data |>
      select(country, contains(actual_year)) |>
      rename(emission = contains(actual_year))
  } else if (arrange_regions == "Per capita") {
    data_to <- capita_data |>
      select(country, contains(actual_year)) |>
      rename(emission = contains(actual_year))
  } else if (arrange_regions == "GDP") {
    data_to <- gpd_data |>
      select(country, contains(actual_year)) |>
      rename(emission = contains(actual_year))
  } else if (arrange_regions == "Sector") {
    data_to <- sector_substance_data |>
      filter(sector == selected_sector) |>
      select(country, sector, contains(actual_year)) |>
      group_by(country, sector) |>
      summarise(
        emission = sum(.data[[paste0("x", actual_year)]], na.rm = TRUE)
      ) |>
      ungroup()
  } else if (arrange_regions == "Substance") {
    data_to <- sector_substance_data |>
      filter(substance == selected_substance) |>
      select(country, substance, contains(actual_year)) |>
      group_by(country, substance) |>
      summarise(
        emission = sum(.data[[paste0("x", actual_year)]], na.rm = TRUE)
      ) |>
      ungroup()
  } else if (arrange_regions == "Sector & Substance") {
    data_to <- sector_substance_data |>
      filter(
        sector == selected_sector,
        substance == selected_substance
      ) |>
      select(country, sector, substance, contains(actual_year)) |>
      group_by(country, sector, substance) |>
      summarise(
        emission = sum(.data[[paste0("x", actual_year)]], na.rm = TRUE)
      ) |>
      ungroup()
  }
  # data_to
  data_to |>
    arrange(desc(emission)) |>
    select(country, emission) |>
    na.omit() |>
    mutate(emission = as.numeric(format(round(emission, 2), nsmall = 2)))
}

#' @export
get_regions_emissions_label <- function(
  arrange_regions,
  selected_sector,
  selected_substance
) {
  # label
  if (arrange_regions == "Total emissions") {
    "Total emissions by country"
  } else if (arrange_regions == "Per capita") {
    "Per capita emissions by country"
  } else if (arrange_regions == "GDP") {
    "Emissions per GDP by country"
  } else if (arrange_regions == "Sector") {
    paste0("Emissions in ", selected_sector, " sector by country")
  } else if (arrange_regions == "Substance") {
    paste0(selected_substance, " emissions by country")
  } else if (arrange_regions == "Sector & Substance") {
    paste0(
      selected_substance,
      " emissions in ",
      selected_sector,
      " sector by country"
    )
  }
}

#' @export
get_country_he <- function(
  arrange_regions,
  actual_country,
  selected_sector,
  selected_substance,
  total_data,
  capita_data,
  gpd_data,
  sector_substance_data
) {
  if (arrange_regions == "Total emissions") {
    data_to <- total_data |>
      filter(country == actual_country) |>
      select(country, matches("x\\d{4}"))
  } else if (arrange_regions == "Per capita") {
    data_to <- capita_data |>
      filter(country == actual_country) |>
      select(country, matches("x\\d{4}"))
  } else if (arrange_regions == "GDP") {
    data_to <- gpd_data |>
      filter(country == actual_country) |>
      select(country, matches("x\\d{4}"))
  } else if (arrange_regions == "Sector") {
    data_to <- sector_substance_data |>
      filter(country == actual_country) |>
      filter(sector == selected_sector) |>
      select(country, sector, matches("x\\d{4}")) |>
      group_by(country, sector) |>
      summarise(across(matches("x\\d{4}"), sum)) |>
      ungroup()
  } else if (arrange_regions == "Substance") {
    data_to <- sector_substance_data |>
      filter(country == actual_country) |>
      filter(substance == selected_substance) |>
      select(country, substance, matches("x\\d{4}")) |>
      group_by(country, substance) |>
      summarise(across(matches("x\\d{4}"), sum)) |>
      ungroup()
  } else if (arrange_regions == "Sector & Substance") {
    data_to <- sector_substance_data |>
      filter(country == actual_country) |>
      filter(
        sector == selected_sector,
        substance == selected_substance
      ) |>
      select(country, sector, substance, matches("x\\d{4}")) |>
      group_by(country, sector, substance) |>
      summarise(across(matches("x\\d{4}"), sum)) |>
      ungroup()
  }

  # data_to
  data_to |>
    pivot_longer(cols = matches("x\\d{4}")) |>
    rename(
      year = name,
      emission = value
    ) |>
    mutate(year = str_replace(year, "x", ""))
}

#' @export
get_countries_he <- function(
  arrange_regions,
  countries,
  selected_sector,
  selected_substance,
  total_data,
  capita_data,
  gpd_data,
  sector_substance_data
) {
  # data
  countries |>
    lapply(function(x) {
      get_country_he(
        arrange_regions,
        x,
        selected_sector,
        selected_substance,
        total_data,
        capita_data,
        gpd_data,
        sector_substance_data
      )
    }) |>
    bind_rows()
}

#' @export
get_plot_title <- function(
  arrange_regions,
  selected_sector,
  selected_substance
) {
  # label
  if (arrange_regions == "Total emissions") {
    "Total emissions in MT"
  } else if (arrange_regions == "Per capita") {
    "Per capita emissions in t"
  } else if (arrange_regions == "GDP") {
    "Emissions per GDP in t/kUSD"
  } else if (arrange_regions == "Sector") {
    paste0("Emissions in ", selected_sector, " in Mt")
  } else if (arrange_regions == "Substance") {
    paste0(selected_substance, " emissions in Mt")
  } else if (arrange_regions == "Sector & Substance") {
    paste0(
      selected_substance,
      " emissions in ",
      selected_sector,
      " in Mt"
    )
  }
}
