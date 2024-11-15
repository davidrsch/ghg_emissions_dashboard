box::use(
  dplyr[tibble],
  DT[datatable, dataTableProxy, DTOutput, formatCurrency, renderDT, replaceData, selectRows],
  grDevices[colorRampPalette],
  plotly[layout, plot_ly, plotlyOutput, renderPlotly],
  shiny.fluent[PrimaryButton.shinyInput, Stack, Text],
  shiny[div, moduleServer, NS, observeEvent, reactiveVal, renderUI, uiOutput],
  utils[tail],
)

box::use(
  app/logic/top_regions_help[get_countries_he, get_plot_title, get_regions_emissions],
  app/logic/top_regions_help[get_regions_emissions_label],
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
    ),
    div(
      plotlyOutput(ns("top_emissions_chart"), height = "100%"),
      class = "card ms-depth-8 ms-sm8",
      style = "background-color: #ffff; overflow-y: auto;"
    )
  )
}

#' @export
server <- function(id, inputs, sidebar_controls) {
  moduleServer(id, function(input, output, session) {

    top_data <- reactiveVal(tibble(country = NA, emission = NA))

    observeEvent(
      c(
        inputs$arrange_regions,
        inputs$kpi_years$key,
        inputs$arrange_regions_sectors,
        inputs$arrange_regions_substance
      ),
      {
        top_data(
          get_regions_emissions(
            inputs$arrange_regions,
            inputs$kpi_years$key,
            inputs$arrange_regions_sectors,
            inputs$arrange_regions_substance
          )
        )
      }
    )

    output$top_regions_table <- renderDT({
      datatable(
        top_data(),
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
          top_data(),
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
        top_data(),
        resetPaging = FALSE
      )
    })

    observeEvent(
      c(
        sidebar_controls$hide_sidebar_left,
        sidebar_controls$show_sidebar_right,
        inputs$arrange_regions,
        inputs$arrange_regions_sectors,
        inputs$arrange_regions_substance
      ),
      {
        output$top_emissions_chart <- renderPlotly({
          countries <- top_data()[input$top_regions_table_rows_selected, "country"][[1]]
          if (all(is.na(countries))) {
            plot_ly(type = "scatter", mode = "text") |>
              layout(
                title = "No Data Available",
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE),
                annotations = list(
                  text = "No data available",
                  x = 0.5,
                  y = 0.5,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(size = 20)
                )
              )
          } else {
            plot_data <- get_countries_he(
              inputs$arrange_regions,
              countries,
              inputs$arrange_regions_sectors,
              inputs$arrange_regions_substance
            )
            unique_countries <- length(countries)
            colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(unique_countries)
            plot_title <- get_plot_title(
              inputs$arrange_regions,
              inputs$arrange_regions_sectors,
              inputs$arrange_regions_substance
            ) 
            y_axis_label <- ifelse(
              is.element(
                inputs$arrange_regions,
                c("Total emissions", "Sector", "Substance", "Sector & Substance")
              ),
              "Emissions in Mt",
              "Emissions in t"
            )
            plot_ly(
              plot_data,
              x = ~year,
              y = ~emission,
              color = ~country,
              colors = colors,
              type = "scatter",
              mode = "lines+markers"
            ) |>
              layout(
                title = plot_title,
                xaxis = list(title = "Date"),
                yaxis = list(title = y_axis_label),
                legend = list(title = list(text = "Countries")),
                autosize = TRUE
              )
          }
        })
      }
    )

  })
}
