box::use(
  shiny.fluent[fluentPage, Pivot, PivotItem],
  shiny[div, moduleServer, NS],
  shinyjs[useShinyjs],
)

box::use(
  app/view/inputs,
  app/view/key_metrics,
  app/view/sidebar,
  app/view/top_regions,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    useShinyjs(),
    Pivot(
      PivotItem(
        headerText = "Wellcome",
        "Global Greenhouse Gas Emissions Overview"
      ),
      PivotItem(
        headerText = "A",
        sidebar$ui(
          id = ns("sidebar_A"),
          name = "a_sidebar",
          title = "Inputs",
          sidebar_content = inputs$ui(ns("inputs")),
          sidebar_bgc = "#ffff",
          main_content = div(
            key_metrics$ui(ns("keymetrics")),
            top_regions$ui(ns("regions_tp"))
          ),
          main_bgc = "#DADAD9"
        )
      )
    ),
    style = "background-color:#ffff"
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sidebar_controls <- sidebar$server("sidebar_A")
    inputs_to <- inputs$server("inputs")
    key_metrics$server("keymetrics", inputs_to)
    top_regions$server("regions_tp", inputs_to, sidebar_controls)
  })
}
