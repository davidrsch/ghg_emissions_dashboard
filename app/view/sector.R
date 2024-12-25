box::use(
  dplyr[arrange, desc, filter, group_by, mutate, summarise, tibble, ungroup],
  plotly[event_data, event_register, layout, plot_ly, plotlyOutput, renderPlotly],
  shiny.fluent[Stack],
  shiny[div, getDefaultReactiveDomain, moduleServer, NS, observeEvent, reactiveVal, renderText, textOutput],
  shinyjs[addClass, removeClass],
)

box::use(
  app/logic/data[ghg_by_sector_and_country],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  Stack(
    id = ns("sector_stack"),
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
      plotlyOutput(ns("stacked_sectors"), height = "100%"),
      class = "card ms-depth-8 ms-sm12 ms-xl6",
      style = "background-color: #ffff; text-align: left;"
    )
  )
}

#' @export
server <- function(id, inputs, sidebar_controls, countries) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_sectors <- reactiveVal(
      tibble(
        sector = unique(ghg_by_sector_and_country$sector),
        visible = 1
      )
    )

    observeEvent(
      c(
        sidebar_controls$hide_sidebar_left,
        sidebar_controls$show_sidebar_right,
        countries,
        inputs$kpi_years$key
      ),
      {
        output$stacked_sectors <- renderPlotly({
          if (all(is.na(countries()))) {
            p <- plot_ly(type = "scatter", mode = "text") |>
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
            plot_data <- ghg_by_sector_and_country |>
              filter(country %in% countries()) |>
              group_by(country, sector) |>
              summarise(
                emission = sum(.data[[paste0("x", inputs$kpi_years$key)]], na.rm = TRUE)
              ) |>
              ungroup() |>
              group_by(country) |>
              mutate(total_emission = sum(emission)) |>
              ungroup() |>
              arrange(desc(total_emission), country)
            p <- plot_data |>
              plot_ly(
                x = ~factor(country, levels = unique(country)), 
                y = ~emission, 
                color = ~sector,
                type = 'bar',
                visible = ~ifelse(
                  sector %in% selected_sectors()[which(selected_sectors()$visible == 0), "sector"],
                   "legendonly", TRUE
                  )
              ) |>
              layout(
                barmode = 'stack',
                title = paste0(
                  "Sector contribution of selected countries in ",
                  inputs$kpi_years$key
                ),
                xaxis = list(title = "Country"),
                yaxis = list(title = "Emission by sector")
              )
            event_register(p, "plotly_legendclick")
            #event_register(p, "plotly_legenddoubleclick")
          }
        })
      }
    )

  })
}
