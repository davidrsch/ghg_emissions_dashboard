box::use(
  shiny.fluent[Dropdown.shinyInput],
  shiny.fluent[updateDropdown.shinyInput],
  shiny[div, getDefaultReactiveDomain, moduleServer, NS, observeEvent],
  stringr[str_split_fixed],
)

box::use(
  app / logic / data[sectors, substances],
  app / logic / get_options[get_options],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    Dropdown.shinyInput(
      ns("emissions_by"),
      label = "Regions by:",
      value = "Total emissions",
      options = get_options(
        c(
          "Total emissions",
          "Per capita",
          "GDP",
          "Sector",
          "Substance",
          "Sector & Substance"
        )
      ),
      calloutProps = list(
        styles = list(
          root = list(
            "max-height" = "300px!important"
          )
        )
      ),
      `data-test` = paste0(str_split_fixed(id, "-", 2)[2], "-emissions_by")
    ),
    Dropdown.shinyInput(
      ns("emissions_by_sectors"),
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
      ),
      `data-test` = paste0(
        str_split_fixed(id, "-", 2)[2],
        "-emissions_by_sector"
      )
    ),
    Dropdown.shinyInput(
      ns("emissions_by_substance"),
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
      ),
      `data-test` = paste0(
        str_split_fixed(id, "-", 2)[2],
        "-emissions_by_substance"
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$emissions_by, {
      if (is.element(input$emissions_by, c("Sector", "Sector & Substance"))) {
        updateDropdown.shinyInput(
          session = getDefaultReactiveDomain(),
          "emissions_by_sectors",
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
          "emissions_by_sectors",
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

      if (
        is.element(input$emissions_by, c("Substance", "Sector & Substance"))
      ) {
        updateDropdown.shinyInput(
          session = getDefaultReactiveDomain(),
          "emissions_by_substance",
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
          "emissions_by_substance",
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
  })
}
