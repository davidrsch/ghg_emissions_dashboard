box::use(
  dplyr[last],
  shiny.fluent[ComboBox.shinyInput, Dropdown.shinyInput, updateDropdown.shinyInput],
  shiny[div, getDefaultReactiveDomain, moduleServer, NS, observeEvent],
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
        text = "GLB"
      ),
      useComboBoxAsMenuWidth = TRUE,
      options = get_options(edgar_cc),
      calloutProps = list(
        styles = list(
          root = list(
            "max-height" = "300px!important"
          )
        )
      )
    ),
    ComboBox.shinyInput(
      ns("kpi_secondary_region"),
      label = "Secondary region",
      value = list(
        key = "EU27",
        text = "EU27"
      ),
      useComboBoxAsMenuWidth = TRUE,
      options = get_options(edgar_cc),
      calloutProps = list(
        styles = list(
          root = list(
            "max-height" = "300px!important"
          )
        )
      )
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
