box::use(
  dplyr[last],
  shiny.fluent[ComboBox.shinyInput],
  shiny[div, moduleServer, NS],
)

box::use(
  app/logic/data[edgar_cc, ghg_tspc_years],
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
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(input)
  })
}
