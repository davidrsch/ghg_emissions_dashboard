box::use(
  dplyr[arrange, contains, desc, filter, group_by, mutate, rename, select],
  dplyr[summarise, ungroup],
)

box::use(
  app/logic/data[ghg_by_sector_and_country, ghg_per_capita_by_country, ghg_per_gdp_by_country],
  app/logic/data[ghg_totals_by_country],
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
      summarise(emission = sum(.data[[paste0("x", actual_year)]], na.rm = TRUE)) |>
      ungroup()
  } else if (arrange_regions == "Substance") {
    data_to <- sector_substance_data |>
      filter(substance == selected_substance) |>
      select(country, substance, contains(actual_year)) |>
      group_by(country, substance) |>
      summarise(emission = sum(.data[[paste0("x", actual_year)]], na.rm = TRUE)) |>
      ungroup()
  } else if (arrange_regions == "Sector & Substance") {
    data_to <- sector_substance_data |>
      filter(
        sector == selected_sector,
        substance == selected_substance
      ) |>
      select(country, sector, substance, contains(actual_year)) |>
      group_by(country, sector, substance) |>
      summarise(emission = sum(.data[[paste0("x", actual_year)]], na.rm = TRUE)) |>
      ungroup()
  }
  data_to <- data_to |>
    arrange(desc(emission)) |>
    select(country, emission) |>
    mutate(emission = as.numeric(format(round(emission, 2), nsmall = 2)))
  return(data_to)
}

#' @export
get_regions_emissions_label <- function(arrange_regions, selected_sector, selected_substance) {

  if (arrange_regions == "Total emissions") {
    label <- "Total emissions by country"
  } else if (arrange_regions == "Per capita") {
    label <- "Per capita emissions by country"
  } else if (arrange_regions == "GDP") {
    label <- "Emissions per GDP by country"
  } else if (arrange_regions == "Sector") {
    label <- paste0("Emissions in ", selected_sector, " sector by country")
  } else if (arrange_regions == "Substance") {
    label <- paste0(selected_substance, " emissions by country")
  } else if (arrange_regions == "Sector & Substance") {
    label <- paste0(selected_substance, " emissions in ", selected_sector, " sector by country")
  }
  return(label)
}
