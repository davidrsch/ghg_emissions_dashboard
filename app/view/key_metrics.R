box::use(
  shiny.fluent[Stack, Text],
  shiny[div, moduleServer, NS, renderUI, uiOutput],
)

box::use(
  app/logic/data[ghg_gdplulucf_years, ghg_per_capita_by_country, ghg_per_gdp_by_country],
  app/logic/data[ghg_totals_by_country, ghg_tspc_years],
  app/logic/key_metrics_help[get_primary_ui, get_secondary_ui],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  Stack(
    horizontal = TRUE,
    tokens = list(childrenGap = "10px"),
    styles = list(
      root = list(
        "padding-top" = "10px",
        "padding-left" = "2px",
        "padding-right" = "2px",
        "padding-bottom" = "10px"
      )
    ),
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
      Text("Emissions per GDP (kUSD):"),
      uiOutput(ns("gdpghge")),
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
        inputs$`kpi_primary_region-searchable_cb`$key,
        ghg_tspc_years,
        "Mt"
      )
      s_ui <- get_secondary_ui(
        ghg_totals_by_country,
        inputs$kpi_years$key,
        inputs$`kpi_secondary_region-searchable_cb`$key,
        ghg_tspc_years,
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
        inputs$`kpi_primary_region-searchable_cb`$key,
        ghg_tspc_years,
        "t"
      )
      s_ui <- get_secondary_ui(
        ghg_per_capita_by_country,
        inputs$kpi_years$key,
        inputs$`kpi_secondary_region-searchable_cb`$key,
        ghg_tspc_years,
        "t"
      )
      ui_to_show <- div(
        p_ui,
        s_ui
      )
      return(ui_to_show)
    })

    output$gdpghge <- renderUI({
      p_ui <- get_primary_ui(
        ghg_per_gdp_by_country,
        inputs$kpi_years$key,
        inputs$`kpi_primary_region-searchable_cb`$key,
        ghg_gdplulucf_years,
        "t"
      )
      s_ui <- get_secondary_ui(
        ghg_per_gdp_by_country,
        inputs$kpi_years$key,
        inputs$`kpi_secondary_region-searchable_cb`$key,
        ghg_gdplulucf_years,
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
