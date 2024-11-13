box::use(
  shiny.fluent[Stack, Text],
  shiny[div, moduleServer, NS, renderUI, uiOutput],
)

box::use(
  app/logic/data[ghg_per_capita_by_country, ghg_totals_by_country, ghg_totals_years],
  app/logic/key_metrics_help[get_primary_ui, get_secondary_ui],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  Stack(
    horizontal = TRUE,
    tokens = list(padding = "15px", childrenGap = "10px"),
    div(
      Text("Total GHG Emissions:"),
      uiOutput(ns("tghge")),
      class = "card ms-depth-8 ms-sm4",
      style = "background-color: #ffff; text-align: right;"
    ),
    div(
      Text("Per capita Emissions:"),
      uiOutput(ns("pcghge")),
      class = "card ms-depth-8 ms-sm4",
      style = "background-color: #ffff; text-align: right;"
    ),
    div(
      Text("Emissions per GDP:"),
      class = "card ms-depth-8 ms-sm4",
      style = "background-color: #ffff; text-align: right;"
    )
  )
}

#' @export
server <- function(id, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$tghge <- renderUI({
      p_ui <- get_primary_ui(
        ghg_totals_by_country,
        inputs$kpi_years$key,
        inputs$kpi_primary_region$key,
        ghg_totals_years,
        "Mt"
      )
      s_ui <- get_secondary_ui(
        ghg_totals_by_country,
        inputs$kpi_years$key,
        inputs$kpi_secondary_region$key,
        ghg_totals_years,
        "Mt"
      )
      ui_to_show <- div(
        p_ui,
        s_ui
      )
      return(ui_to_show)
    })

    output$pcghge <- renderUI({
      p_ui <- get_primary_ui(
        ghg_per_capita_by_country,
        inputs$kpi_years$key,
        inputs$kpi_primary_region$key,
        ghg_totals_years,
        "t"
      )
      s_ui <- get_secondary_ui(
        ghg_per_capita_by_country,
        inputs$kpi_years$key,
        inputs$kpi_secondary_region$key,
        ghg_totals_years,
        "t"
      )
      ui_to_show <- div(
        p_ui,
        s_ui
      )
      return(ui_to_show)
    })
  })
}
