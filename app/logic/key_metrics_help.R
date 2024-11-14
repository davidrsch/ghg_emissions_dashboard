box::use(
  shiny.fluent[FontIcon, Stack, Text],
  shiny[div],
)

#' @export
get_value_of_country_in_year <- function(data, country_code, year) {
  country_values <- which(data["edgar_country_code"] == country_code)
  value_in_year <- data[country_values, paste0("x", year)][[1]]
  return(value_in_year)
}

#' @export
get_country_name <- function(data, country_code) {
  country_values <- which(data["edgar_country_code"] == country_code)
  country_name <- data[country_values, "country"][[1]]
  return(country_name)
}

#' @export
get_change_color <- function(change) {
  change_color <- ifelse(
    change == "",
    "#DADAD9",
    ifelse(
      change > 0,
      "#d83b01",
      ifelse(
        change == 0,
        "#cdd801",
        "#107c10"
      )
    )
  )
  return(change_color)
}

#' @export
get_change_symbol <- function(change) {
  change_symbol <- ifelse(
    change == "",
    "CalculatorSubstract",
    ifelse(
      change > 0,
      "ChevronUpMed",
      ifelse(
        change == 0,
        "CalculatorSubstract",
        "ChevronDownMed"
      )
    )
  )
  return(change_symbol)
}

#' @export
get_change_format <- function(change) {
  if (change == "") {
    ""
  } else {
    paste0(round(change, 2), "%")
  }
}

#' @export
get_value_format <- function(value) {
  if (value < 1000) {
    paste0(round(value, digits = 2))
  } else {
    paste0(round(value / 1000, digits = 2), "k")
  }
}

#' @export
get_primary_ui <- function(data, actual_year, country_code, years, unit) {
  exist_actual_year <- is.element(actual_year, years)
  country <- get_country_name(data, country_code)

  if (exist_actual_year) {
    prev_year <- as.character(as.numeric(actual_year) - 1)
    exist_prev_year <- is.element(prev_year, years)
    actual_value <- get_value_of_country_in_year(data, country_code, actual_year)

    if (exist_prev_year & !is.na(actual_value)) {
      prev_value <- get_value_of_country_in_year(data, country_code, prev_year)
      change <- actual_value / prev_value - 1
    } else {
      change <- ""
    }

    if (!is.na(actual_value)) {
      actual_value <- paste0(
        get_value_format(actual_value),
        unit
      )
    } else {
      actual_value <- "No data"
    }
    
  } else {
    actual_value <- "No data"
    change <- ""
  }

  change_color <- get_change_color(change)
  change_symbol <- get_change_symbol(change)
  change <- get_change_format(change)

  ui_to_show <- div(
    Text(
      country,
      variant = "medium"
    ),
    div(
      Stack(
        horizontal = TRUE,
        horizontalAlign = "end",
        verticalAlign = "center",
        tokens = list(childrenGap = "10px"),
        FontIcon(
          iconName = change_symbol,
          style = list(fontSize = 20)
        ),
        Text(
          change,
          variant = "medium",
          style = list(color = change_color)
        ),
        Text(
          actual_value,
          variant = "xxLarge",
          style = list(color = change_color)
        )
      ),
      style = paste0("color:", change_color, ";")
    )
  )
  return(ui_to_show)
}

#' @export
get_secondary_ui <- function(data, actual_year, country_code, years, unit) {
  exist_actual_year <- is.element(actual_year, years)
  country <- get_country_name(data, country_code)

  if (exist_actual_year) {
    prev_year <- as.character(as.numeric(actual_year) - 1)
    exist_prev_year <- is.element(prev_year, years)
    actual_value <- get_value_of_country_in_year(data, country_code, actual_year)

    if (exist_prev_year & !is.na(actual_value)) {
      prev_value <- get_value_of_country_in_year(data, country_code, prev_year)
      change <- actual_value / prev_value - 1
    } else {
      change <- ""
    }

    if (!is.na(actual_value)) {
      actual_value <- paste0(
        get_value_format(actual_value),
        unit
      )
    } else {
      actual_value <- "No data"
    }
    
  } else {
    actual_value <- "No data"
    change <- ""
  }

  change_color <- get_change_color(change)
  change_symbol <- get_change_symbol(change)
  change <- get_change_format(change)

  ui_to_show <- div(
    Text(
      country,
      variant = "xSmall"
    ),
    div(
      Stack(
        horizontal = TRUE,
        horizontalAlign = "end",
        verticalAlign = "center",
        tokens = list(childrenGap = "8px"),
        FontIcon(
          iconName = change_symbol,
          style = list(fontSize = 20)
        ),
        Text(
          change,
          variant = "xSmall",
          style = list(color = change_color)
        ),
        Text(
          actual_value,
          variant = "mediumPlus",
          style = list(color = change_color)
        )
      ),
      style = paste0("color:", change_color, ";")
    )
  )
  return(ui_to_show)
}
