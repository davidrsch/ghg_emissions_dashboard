box::use(
  dplyr[last],
  shiny.fluent[ComboBox.shinyInput, Dropdown.shinyInput],
  shiny.fluent[updateDropdown.shinyInput],
  shiny[div, getDefaultReactiveDomain, moduleServer, NS, observeEvent, reactiveVal],
)

box::use(
  app/logic/data[edgar_cc, ghg_tspc_years, sectors, substances],
  app/logic/get_options[get_options],
  app/view/combobox_search,
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
    combobox_search$ui(
      ns("kpi_primary_region"),
      cb_label = "Primary region",
      default_key = "GLB",
      default_text = "GLB (GLOBAL)",
      cb_options = get_options(edgar_cc),
      is_visible = TRUE
    ),
    combobox_search$ui(
      ns("kpi_secondary_region"),
      cb_label = "Secondary region",
      default_key = "EU27",
      default_text = "EU27 (EU27)",
      cb_options = get_options(edgar_cc),
      is_visible = TRUE
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

    combobox_visibility <- reactiveVal(TRUE)
    combobox_pdft_value <- reactiveVal(NULL)
    combobox_sdft_value <- reactiveVal(NULL)
    combobox_search$server(
      "kpi_primary_region",
      value = combobox_pdft_value,
      cb_label = "Primary region",
      default_text = "GLB (GLOBAL)",
      cb_options = get_options(edgar_cc),
      is_visible = combobox_visibility
    )

    combobox_search$server(
      "kpi_secondary_region",
      value = combobox_sdft_value,
      cb_label = "Secondary region",
      default_text = "EU27 (EU27)",
      cb_options = get_options(edgar_cc),
      is_visible = combobox_visibility
    )

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
