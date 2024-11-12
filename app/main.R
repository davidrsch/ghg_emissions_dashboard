box::use(
  shiny.fluent[fluentPage, Pivot, PivotItem],
  shiny[div, moduleServer, NS, renderUI, tags],
  shinyjs[useShinyjs],
)

box::use(
  app/view[...],
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
          sidebar_content = "Hello sidebar",
          sidebar_bgc = "#ffff",
          main_content = "Hello main",
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
    sidebar$server("sidebar_A")

  })
}
