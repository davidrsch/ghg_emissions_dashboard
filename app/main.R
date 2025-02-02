box::use(
  shiny.fluent[fluentPage, Pivot, PivotItem],
  shiny[div, moduleServer, NS],
  shinyjs[useShinyjs],
)

box::use(
  app/view/compare/compare,
  app/view/overview/inputs,
  app/view/overview/key_metrics,
  app/view/overview/sector_substance,
  app/view/overview/top_regions,
  app/view/planet/planet,
  app/view/tool_modules/sidebar,
  app/view/wellcome/wellcome,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    useShinyjs(),
    Pivot(
      PivotItem(
        headerText = "Wellcome",
        wellcome$ui(id = ns("wellcome"))
      ),
      PivotItem(
        headerText = "Overview",
        sidebar$ui(
          id = ns("sidebar_A"),
          name = "a_sidebar",
          title = "Inputs",
          sidebar_content = inputs$ui(ns("inputs")),
          sidebar_bgc = "#ffff",
          main_content = div(
            style = "position: absolute; top: 0; width: calc(100% - 16px);",
            key_metrics$ui(ns("keymetrics")),
            top_regions$ui(ns("regions_tp")),
            sector_substance$ui(ns("sector_metrics"))
          ),
          main_bgc = "#DADAD9"
        )
      ),
      PivotItem(
        headerText = "Compare",
        compare$ui(ns("compare"))
      ),
      PivotItem(
        headerText = "Explore the Planet",
        planet$ui(ns("planet"))
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
    countries <- top_regions$server("regions_tp", inputs_to, sidebar_controls)
    sector_substance$server("sector_metrics", inputs_to, sidebar_controls, countries, input)
    compare$server("compare")
    planet$server("planet")
  })
}
