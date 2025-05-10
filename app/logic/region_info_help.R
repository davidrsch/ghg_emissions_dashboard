box::use(
  dplyr[arrange, desc, filter, group_by, join_by, left_join, mutate],
  dplyr[rename, select, summarise, ungroup],
  plotly[config, layout, plot_ly],
  shiny.fluent[Stack, Text],
  stats[reorder],
  stringr[str_to_title],
  tibble[deframe],
)

box::use(
  app / logic / data[ghg_by_sector_and_country],
  app / logic / data[ghg_per_capita_by_country],
  app / logic / data[ghg_per_gdp_by_country],
  app / logic / data[ghg_totals_by_country],
  app / logic / key_metrics_help[get_value_of_country_in_year],
)

#' @export
get_region_kpi_ui <- function(
  data,
  country_code,
  complementary_region,
  year,
  complementary_year
) {
  if (data == "ghg_totals") {
    description <- "Total GHG Emissions in Mt: "
    data_to_use <- ghg_totals_by_country
    unit <- "Mt"
  } else if (data == "ghg_per_capita") {
    description <- "Per Capita Emissions in t: "
    data_to_use <- ghg_per_capita_by_country
    unit <- "t"
  } else if (data == "ghg_per_gdp") {
    description <- "Emissions per GDP (kUSD):  "
    data_to_use <- ghg_per_gdp_by_country
    unit <- "t"
  }
  value <- get_value_of_country_in_year(
    data_to_use,
    country_code,
    year
  ) |>
    round(x = _, digits = 2)
  complementary_value <- get_value_of_country_in_year(
    data_to_use,
    complementary_region,
    complementary_year
  ) |>
    round(x = _, digits = 2)

  change_color <- ifelse(
    value > complementary_value,
    "#d83b01",
    ifelse(
      value == complementary_value,
      "#cdd801",
      "#107c10"
    )
  )

  # ui
  Stack(
    horizontal = TRUE,
    horizontalAlign = "space-between",
    verticalAlign = "center",
    Text(
      description,
      variant = "medium"
    ),
    Text(
      paste0(
        value,
        unit
      ),
      style = list(color = change_color),
      variant = "xxLarge"
    ),
    style = "margin-top: 10px;"
  )
}

#' @export
get_plot_data <- function(type, code, year) {
  data <- ghg_by_sector_and_country |>
    filter(edgar_country_code == code)
  if (type == "sector") {
    data <- data |>
      group_by(country, sector)
  } else if (type == "substance") {
    data <- data |>
      group_by(country, substance)
  }

  data <- data |>
    summarise(
      emission = sum(.data[[paste0("x", year)]], na.rm = TRUE)
    ) |>
    ungroup()

  # data
  if (type == "sector") {
    data |>
      arrange(sector)
  } else if (type == "substance") {
    data |>
      arrange(substance)
  }
}

#' @export
get_region_plot <- function(
  type,
  country_code,
  complementary_region,
  year,
  complementary_year
) {
  primary_data <- get_plot_data(type, country_code, year)
  secondary_data <- get_plot_data(
    type,
    complementary_region,
    complementary_year
  )
  if (type == "sector") {
    primary_data <- primary_data |>
      left_join(
        secondary_data |>
          rename(emission_2 = emission) |>
          select(-country),
        by = join_by(sector)
      )
  } else if (type == "substance") {
    primary_data <- primary_data |>
      left_join(
        secondary_data |>
          rename(emission_2 = emission) |>
          select(-country),
        by = join_by(substance)
      )
  }
  primary_data <- primary_data |>
    mutate(
      color = emission - emission_2,
      color = ifelse(
        color > 0,
        "#d83b01",
        ifelse(
          color == 0,
          "#cdd801",
          "#107c10"
        )
      )
    ) |>
    select(-emission_2)

  colors_to <- primary_data |>
    select(-c(country, emission))
  names(colors_to) <- c("name", "value")
  colors_to <- deframe(colors_to)

  if (type == "sector") {
    plot <- primary_data |>
      plot_ly(
        x = ~emission,
        y = ~ reorder(sector, desc(sector)),
        color = ~sector,
        colors = colors_to,
        type = "bar",
        orientation = "h"
      )
    x_axis <- "Emission by sector"
  } else if (type == "substance") {
    plot <- primary_data |>
      plot_ly(
        x = ~emission,
        y = ~ reorder(substance, desc(substance)),
        color = ~substance,
        colors = colors_to,
        type = "bar",
        orientation = "h"
      )
    x_axis <- "Emission by substance"
  }

  # plot
  plot |>
    config(displayModeBar = "always") |>
    layout(
      showlegend = FALSE,
      title = list(
        text = paste0(
          "<span style='font-size:1rem;'>",
          str_to_title(type),
          " contribution of ",
          country_code,
          " in ",
          year,
          "</span>"
        )
      ),
      margin = list(t = 80),
      xaxis = list(title = x_axis),
      yaxis = list(title = country_code)
    )
}
