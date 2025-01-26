box::use(
  dplyr[last],
  purrr[discard],
  shiny.fluent[ComboBox.shinyInput, Dropdown.shinyInput, updateComboBox.shinyInput],
  shiny.fluent[updateDropdown.shinyInput],
  shiny[div, getDefaultReactiveDomain, moduleServer, NS, observeEvent, tags, HTML],
  stringr[str_detect, str_replace, str_to_lower],
)

box::use(
  app/logic/data[edgar_cc, ghg_tspc_years, sectors, substances],
  app/logic/get_options[get_options],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    ComboBox.shinyInput(
      ns("kpi_years"),
      label = "Year",
      value = list(
        key = last(ghg_tspc_years),
        text = last(ghg_tspc_years)
      ),
      useComboBoxAsMenuWidth = TRUE,
      options = get_options(ghg_tspc_years),
      calloutProps = list(
        styles = list(
          root = list(
            "max-height" = "300px!important"
          )
        )
      )
    ),
    ComboBox.shinyInput(
      ns("kpi_primary_region"),
      label = "Primary region",
      value = list(
        key = "GLB",
        text = "GLB (GLOBAL)"
      ),
      allowFreeform = TRUE,
      useComboBoxAsMenuWidth = TRUE,
      options = get_options(edgar_cc),
      calloutProps = list(
        doNotLayer = TRUE, 
        styles = list(
          root = list(
            "max-height" = "300px!important"
          )
        )
      ),
      `data-test` = "kpi_primary_region"
    ),
    ComboBox.shinyInput(
      ns("kpi_secondary_region"),
      label = "Secondary region",
      value = list(
        key = "EU27",
        text = "EU27 (Europe)"
      ),
      allowFreeform = TRUE,
      useComboBoxAsMenuWidth = TRUE,
      options = get_options(edgar_cc),
      calloutProps = list(
        styles = list(
          root = list(
            "max-height" = "300px!important"
          )
        )
      ),
      `data-test` = "kpi_secondary_region"
    ),
    Dropdown.shinyInput(
      ns("arrange_regions"),
      label = "Regions by:",
      value = "Total emissions",
      options = get_options(
        c("Total emissions", "Per capita", "GDP", "Sector", "Substance", "Sector & Substance")
      ),
      calloutProps = list(
        styles = list(
          root = list(
            "max-height" = "300px!important"
          )
        )
      )
    ),
    Dropdown.shinyInput(
      ns("arrange_regions_sectors"),
      label = "Sector:",
      value = "Power Industry",
      options = get_options(sectors),
      styles = list(
        root = list(
          "visibility" = "hidden",
          "display" = "none",
          "font-weight" = "400"
        )
      ),
      calloutProps = list(
        styles = list(
          root = list(
            "max-height" = "300px!important"
          )
        )
      )
    ),
    Dropdown.shinyInput(
      ns("arrange_regions_substance"),
      label = "Substance:",
      value = "CO2",
      options = get_options(substances),
      styles = list(
        root = list(
          "visibility" = "hidden",
          "display" = "none",
          "font-weight" = "400"
        )
      ),
      calloutProps = list(
        styles = list(
          root = list(
            "max-height" = "300px!important"
          )
        )
      )
    )
  )
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Implement fuzy search in primary region combobox
    observeEvent(input$kpi_primary_region_query, {
      query <- input$kpi_primary_region_query
      all_options <- get_options(edgar_cc)
      
      if (query == "") {
        query <- "GLB (GLOBAL)"
      }

      # Filter options based on the query
      if (!is.null(query)) {
        filtered_options <- all_options |>
          lapply(function(x) {
            query_to <- query |>
              str_replace("\\(", "") |>
              str_replace("\\)", "") |>
              str_to_lower()
            text <- x$text |> 
              str_replace("\\(", "") |>
              str_replace("\\)", "") |>
              str_to_lower()
            if (str_detect(text, query_to)) {
              x
            } else {
              NULL
            }
          }
        )
        filtered_options <- filtered_options |>
          discard(is.null)
      } else {
        filtered_options <- all_options
      }
      
      # Update the ComboBox with filtered options
      updateComboBox.shinyInput(
        session = getDefaultReactiveDomain(),
        "kpi_primary_region",
        options = filtered_options
      )
    })

    # Update combo box to include all options once one it's selected
    observeEvent(input$kpi_primary_region, {
      all_options <- get_options(edgar_cc)
      
      updateComboBox.shinyInput(
        session = getDefaultReactiveDomain(),
        "kpi_primary_region",
        options = all_options
       )
    })

    # Implement fuzy search in secondary region combobox
    observeEvent(input$kpi_secondary_region_query, {
      query <- input$kpi_secondary_region_query
      all_options <- get_options(edgar_cc)
      
      if (query == "") {
        query <- "EU27 (Europe)"
      }

      # Filter options based on the query
      if (!is.null(query)) {
        filtered_options <- all_options |>
          lapply(function(x) {
            query_to <- query |>
              str_replace("\\(", "") |>
              str_replace("\\)", "") |>
              str_to_lower()
            text <- x$text |> 
              str_replace("\\(", "") |>
              str_replace("\\)", "") |>
              str_to_lower()
            if (str_detect(text, query_to)) {
              x
            } else {
              NULL
            }
          }
        )
        filtered_options <- filtered_options |>
          discard(is.null)
      } else {
        filtered_options <- all_options
      }
      
      # Update the ComboBox with filtered options
      updateComboBox.shinyInput(
        session = getDefaultReactiveDomain(),
        "kpi_secondary_region",
        options = filtered_options
      )
    })

    # Update combo box to include all options once one it's selected
    observeEvent(input$kpi_secondary_region, {
      all_options <- get_options(edgar_cc)
      
      updateComboBox.shinyInput(
        session = getDefaultReactiveDomain(),
        "kpi_secondary_region",
        options = all_options
       )
    })

    observeEvent(input$arrange_regions, {
      if (is.element(input$arrange_regions, c("Sector", "Sector & Substance"))) {
        updateDropdown.shinyInput(
          session = getDefaultReactiveDomain(),
          "arrange_regions_sectors",
          styles = list(
            root = list(
              "visibility" = "show",
              "display" = "block",
              "label" = list(
                "font-weight" = "400"
              )
            )
          ),
          calloutProps = list(
            styles = list(
              root = list(
                "max-height" = "300px!important"
              )
            )
          )
        )
      } else {
        updateDropdown.shinyInput(
          session = getDefaultReactiveDomain(),
          "arrange_regions_sectors",
          styles = list(
            root = list(
              "visibility" = "hidden",
              "display" = "none"
            )
          ),
          calloutProps = list(
            styles = list(
              root = list(
                "max-height" = "300px!important"
              )
            )
          )
        )
      }

      if (is.element(input$arrange_regions, c("Substance", "Sector & Substance"))) {
        updateDropdown.shinyInput(
          session = getDefaultReactiveDomain(),
          "arrange_regions_substance",
          styles = list(
            root = list(
              "visibility" = "show",
              "display" = "block",
              "label" = list(
                "font-weight" = "400"
              )
            )
          ),
          calloutProps = list(
            styles = list(
              root = list(
                "max-height" = "300px!important"
              )
            )
          )
        )
      } else {
        updateDropdown.shinyInput(
          session = getDefaultReactiveDomain(),
          "arrange_regions_substance",
          styles = list(
            root = list(
              "visibility" = "hidden",
              "display" = "none"
            )
          ),
          calloutProps = list(
            styles = list(
              root = list(
                "max-height" = "300px!important"
              )
            )
          )
        )
      }

    })

    return(input)
  })
}
