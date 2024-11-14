box::use(
  DT[datatable, dataTableProxy, DTOutput, formatCurrency, renderDT, replaceData, selectRows],
  shiny.fluent[PrimaryButton.shinyInput, Stack, Text],
  shiny[div, moduleServer, NS, observeEvent, renderUI, uiOutput],
  utils[tail],
)

box::use(
  app/logic/top_regions_help[get_regions_emissions, get_regions_emissions_label],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  Stack(
    horizontal = TRUE,
    tokens = list(childrenGap = "10px"),
    styles = list(
      root = list(
        "padding-left" = "2px",
        "padding-right" = "2px",
        "padding-bottom" = "10px"
      )
    ),
    div(
      uiOutput(ns("tpt_label")),
      DTOutput(ns("top_regions_table")),
      Stack(
        horizontal = TRUE,
        horizontalAlign = "center",
        PrimaryButton.shinyInput(ns("deselect_btn"), text = "Deselect All Rows"),
      ),
      class = "card ms-depth-8 ms-sm4",
      style = "background-color: #ffff; text-align: left;"
    )
    # div(
    #   Text("Total GHG Emissions:"),
    #   uiOutput(ns("tghge")),
    #   class = "card ms-depth-8 ms-sm6",
    #   style = "background-color: #ffff; text-align: right;"
    # )
  )
}

#' @export
server <- function(id, inputs) {
  moduleServer(id, function(input, output, session) {

    output$top_regions_table <- renderDT({
      data <- get_regions_emissions(
        inputs$arrange_regions,
        inputs$kpi_years$key,
        inputs$arrange_regions_sectors,
        inputs$arrange_regions_substance
      )
      datatable(
        data,
        filter = "top",
        options = list(
          lengthChange = FALSE,
          dom = "tip",
          pageLength = 10,
          pagingType = "simple",
          columnDefs = list(
            list(className = "dt-center", targets = 0),
            list(className = "dt-left", targets = 1),
            list(className = "dt-right", targets = 2)
          )
        ),
        selection = list(mode = "multiple", target = "row", selected = 1:10)
      ) |>
        formatCurrency(
          columns = "emission",
          currency = "",
          interval = 3,
          mark = ",",
          digits = 2
        )
    })

    top_regions_table_proxy <- dataTableProxy("top_regions_table")

    observeEvent(input$top_regions_table_rows_selected, {
      selected <- input$top_regions_table_rows_selected

      if (length(selected) > 10) {
        selected <- tail(selected, 10)
        replaceData(
          top_regions_table_proxy,
          get_regions_emissions(
            inputs$arrange_regions,
            inputs$kpi_years$key,
            inputs$arrange_regions_sectors,
            inputs$arrange_regions_substance
          ),
          resetPaging = FALSE
        )
        selectRows(top_regions_table_proxy, selected)
      }
    })

    output$tpt_label <- renderUI({
      Text(
        get_regions_emissions_label(
          inputs$arrange_regions,
          inputs$arrange_regions_sectors,
          inputs$arrange_regions_substance
        )
      )
    })

    observeEvent(input$deselect_btn, {
      replaceData(
        top_regions_table_proxy,
        get_regions_emissions(
          inputs$arrange_regions,
          inputs$kpi_years$key,
          inputs$arrange_regions_sectors,
          inputs$arrange_regions_substance
        ),
        resetPaging = FALSE
      )
    })

  })
}
