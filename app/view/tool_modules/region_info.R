box::use(
  plotly[plotlyOutput, renderPlotly],
  shiny[div, moduleServer, NS, observeEvent, reactiveVal, renderUI, uiOutput],
)

box::use(
  app/logic/data[edgar_cc, ghg_tspc_years],
  app/logic/get_options[get_options],
  app/logic/region_info_help[get_region_kpi_ui, get_region_plot],
  app/view/tool_modules/combobox_search,
)

#' @export
ui <- function(id, key_region, text_region, visible_region, key_year, text_year, visible_year) {
  ns <- NS(id)
  div(
    combobox_search$ui(
      ns("region_info"),
      cb_label = "Region",
      default_key = key_region,
      default_text = text_region,
      cb_options = get_options(edgar_cc),
      is_visible = visible_region
    ),
    combobox_search$ui(
      ns("period_info"),
      cb_label = "Period",
      default_key = key_year,
      default_text = text_year,
      cb_options = get_options(ghg_tspc_years),
      is_visible = visible_year
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
server <- function(
  id,
  value_region,
  text_region,
  complementary_region,
  visible_region,
  value_year,
  text_year,
  complementary_year,
  visible_year
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    combobox_search$server(
      "region_info",
      value = value_region,
      cb_label = "Region",
      default_text = text_region,
      cb_options = get_options(edgar_cc),
      is_visible = visible_region
    )

    combobox_search$server(
      "period_info",
      value = value_year,
      cb_label = "Period",
      default_text = text_year,
      cb_options = get_options(ghg_tspc_years),
      is_visible = visible_year
    )

    selected_region <- reactiveVal()
    observeEvent(input$`region_info-searchable_cb`, {
      selected_region(input$`region_info-searchable_cb`$key)
    })

    selected_year <- reactiveVal()
    observeEvent(input$`period_info-searchable_cb`, {
      selected_year(input$`period_info-searchable_cb`$key)
    })

    output$total_ghg <- renderUI({
      get_region_kpi_ui(
        data = "ghg_totals",
        country_code = input$`region_info-searchable_cb`$key,
        complementary_region = complementary_region(),
        year = input$`period_info-searchable_cb`$key,
        complementary_year = complementary_year()
      )
    })

    output$percapita_ghg <- renderUI({
      get_region_kpi_ui(
        data = "ghg_per_capita",
        country_code = input$`region_info-searchable_cb`$key,
        complementary_region = complementary_region(),
        year = input$`period_info-searchable_cb`$key,
        complementary_year = complementary_year()
      )
    })

    output$gdp_ghg <- renderUI({
      get_region_kpi_ui(
        data = "ghg_per_gdp",
        country_code = input$`region_info-searchable_cb`$key,
        complementary_region = complementary_region(),
        year = input$`period_info-searchable_cb`$key,
        complementary_year = complementary_year()
      )
    })

    output$sector_contribution <- renderPlotly({
      get_region_plot(
        type = "sector",
        country_code = input$`region_info-searchable_cb`$key,
        complementary_region = complementary_region(),
        year = input$`period_info-searchable_cb`$key,
        complementary_year = complementary_year()
      )
    })

    output$substance_contribution <- renderPlotly({
      get_region_plot(
        type = "substance",
        country_code = input$`region_info-searchable_cb`$key,
        complementary_region = complementary_region(),
        year = input$`period_info-searchable_cb`$key,
        complementary_year = complementary_year()
      )
    })

    return(
      list(
        region = selected_region,
        year = selected_year
      )
    )

  })
}
