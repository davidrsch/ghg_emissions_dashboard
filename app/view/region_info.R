box::use(
  plotly[plotlyOutput, renderPlotly],
  shiny[div, moduleServer, NS, observeEvent, reactiveVal, renderUI, uiOutput],
)

box::use(
  app/logic/data[edgar_cc],
  app/logic/get_options[get_options],
  app/logic/region_info_help[get_region_kpi_ui, get_region_plot],
  app/view/combobox_search,
)

#' @export
ui <- function(id, key, text) {
  ns <- NS(id)
  div(
    combobox_search$ui(
      ns("region_info"),
      cb_label = "Region",
      default_key = key,
      default_text = text,
      cb_options = get_options(edgar_cc),
      is_visible = TRUE
    ),
    uiOutput(ns("total_ghg")),
    uiOutput(ns("percapita_ghg")),
    uiOutput(ns("gdp_ghg")),
    div(style = "height: 10px"),
    plotlyOutput(ns("sector_contribution")),
    div(style = "height: 10px"),
    plotlyOutput(ns("substance_contribution")),
    class = "card ms-depth-8 ms-sm12 ms-md6",
    style = "background-color: #ffff; text-align: left;"
  )
}

#' @export
server <- function(id, text, complementary_region) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    combobox_visibility <- reactiveVal(TRUE)

    combobox_search$server(
      "region_info",
      cb_label = "Region",
      default_text = text,
      cb_options = get_options(edgar_cc),
      is_visible = combobox_visibility
    )

    selected_key <- reactiveVal()

    observeEvent(input$`region_info-searchable_cb`, {
      selected_key(input$`region_info-searchable_cb`$key)
    })

    output$total_ghg <- renderUI({
      get_region_kpi_ui(
        data = "ghg_totals",
        country_code = input$`region_info-searchable_cb`$key,
        complementary_region = complementary_region()
      )
    })

    output$percapita_ghg <- renderUI({
      get_region_kpi_ui(
        data = "ghg_per_capita",
        country_code = input$`region_info-searchable_cb`$key,
        complementary_region = complementary_region()
      )
    })

    output$gdp_ghg <- renderUI({
      get_region_kpi_ui(
        data = "ghg_per_gdp",
        country_code = input$`region_info-searchable_cb`$key,
        complementary_region = complementary_region()
      )
    })

    output$sector_contribution <- renderPlotly({
      get_region_plot("sector", input$`region_info-searchable_cb`$key, complementary_region())
    })

    output$substance_contribution <- renderPlotly({
      get_region_plot("substance", input$`region_info-searchable_cb`$key, complementary_region())
    })

    return(selected_key)
  })
}
