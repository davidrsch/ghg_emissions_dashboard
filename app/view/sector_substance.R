box::use(
  dplyr[arrange, desc, filter, group_by, mutate, rename, select, summarise],
  dplyr[tibble, ungroup],
  plotly[layout, plot_ly, plotlyOutput, renderPlotly],
  shiny.fluent[Stack],
  shiny[div, moduleServer, NS, observeEvent, reactiveVal, renderUI, uiOutput],
  tibble[deframe],
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
      div(
        div(
          plotlyOutput(ns("stacked_sectors"), height = "100%"),
          style = "flex-grow: 1; overflow: hidden; max-width: calc(100% - 165px);"
        ),
        div(
          uiOutput(ns("sector_plot_filters")),
          style = "width: 165px; margin-left: 8px;"
        ),
        style = "display: flex; align-items: start; box-sizing: border-box; width: 100%;"
      ),
      class = "card ms-depth-8 ms-sm12 ms-xl6",
      style = "background-color: #ffff; text-align: left;"
    ),
    div(
      div(
        div(
          plotlyOutput(ns("stacked_substance"), height = "100%"),
          style = "flex-grow: 1; overflow: hidden; max-width: calc(100% - 165px);"
        ),
        div(
          uiOutput(ns("substance_plot_filters")),
          style = "width: 165px; margin-left: 8px;"
        ),
        style = "display: flex; align-items: start; box-sizing: border-box; width: 100%;"
      ),
      class = "card ms-depth-8 ms-sm12 ms-xl6",
      style = "background-color: #ffff; overflow-y: auto;"
    )
  )
}

#' @export
server <- function(id, inputs, sidebar_controls, countries, main_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    colors_sector <- tibble(
      sector = c(
        "Waste", "Transport", "Processes", "Power Industry", "Industrial Combustion",
        "Fuel Exploitation", "Buildings", "Agriculture"
      ),
      color = c(
        "#b3b3b3", "#e5c494", "#ffd92f", "#a6d854", "#e78ac3", "#8493b8",
        "#fc8d62", "#66c2a5"
      ),
      hover_color = c(
        "#808080", "#ffcd82", "#ffd000", "#ace64f", "#f861be", "#7892cf",
        "#ff510c", "#15d498"
      )
    )

    colors_substance <- tibble(
      substance = c("GWP_100_AR5_N2O", "GWP_100_AR5_F-gases", "GWP_100_AR5_CH4", "CO2"),
      color = c("#e78ac3", "#8da0cb", "#fc8d62", "#66c2a5"),
      hover_color = c("#f861be", "#7892cf", "#ff510c", "#15d498")
    )

    # Create styled filter for sectors plot
    output$sector_plot_filters <- renderUI({
      div(
        apply(colors_sector, MARGIN = 1, FUN = function(sector) {
          shiny.fluent::Checkbox.shinyInput(
            inputId = ns(paste0("filter_sector_", gsub(" ", "_", sector[[1]]))),
            label = sector[[1]],
            value = TRUE,
            styles = list(
              checkbox = list(
                borderColor = sector[[2]],
                backgroundColor = sector[[2]],
                borderRadius = "0px"
              ),
              root = list(
                selectors = list(
                  ":hover .ms-Checkbox-checkbox" = list(
                    borderColor = sector[[3]],
                    background = sector[[3]]
                  )
                )
              )
            )
          )
        })
      )
    })

    # Create styled filter for sectors plot
    output$substance_plot_filters <- renderUI({
      div(
        apply(colors_substance, MARGIN = 1, FUN = function(substance) {
          shiny.fluent::Checkbox.shinyInput(
            inputId = ns(paste0("filter_substance_", substance[[1]])),
            label = substance[[1]],
            value = TRUE,
            styles = list(
              checkbox = list(
                borderColor = substance[[2]],
                backgroundColor = substance[[2]],
                borderRadius = "0px"
              ),
              root = list(
                selectors = list(
                  ":hover .ms-Checkbox-checkbox" = list(
                    borderColor = substance[[3]],
                    background = substance[[3]]
                  )
                )
              )
            )
          )
        })
      )
    })

    # Get selected sectors
    selected_sector <- reactiveVal()
    observeEvent(
      c(
        input$filter_sector_Waste, input$filter_sector_Transport, input$filter_sector_Processes,
        input$filter_sector_Power_Industry, input$filter_sector_Industrial_Combustion,
        input$filter_sector_Fuel_Exploitation, input$filter_sector_Buildings,
        input$filter_sector_Agriculture
      ),
      {
        selected <- colors_sector$sector[
          c(
            input$filter_sector_Waste, input$filter_sector_Transport, input$filter_sector_Processes,
            input$filter_sector_Power_Industry, input$filter_sector_Industrial_Combustion,
            input$filter_sector_Fuel_Exploitation, input$filter_sector_Buildings,
            input$filter_sector_Agriculture
          )
        ]
        selected_sector(selected)
      }
    )

    # Get selected substance
    selected_substance <- reactiveVal()
    observeEvent(
      c(
        input$filter_substance_GWP_100_AR5_N2O, input$`filter_substance_GWP_100_AR5_F-gases`,
        input$filter_substance_GWP_100_AR5_CH4, input$filter_substance_CO2
      ),
      {
        selected <- colors_substance$substance[
          c(
            input$filter_substance_GWP_100_AR5_N2O, input$`filter_substance_GWP_100_AR5_F-gases`,
            input$filter_substance_GWP_100_AR5_CH4, input$filter_substance_CO2
          )
        ]
        selected_substance(selected)
      }
    )

    observeEvent(
      c(
        sidebar_controls$hide_sidebar_left,
        sidebar_controls$show_sidebar_right,
        countries,
        inputs$kpi_years$key,
        selected_substance,
        selected_sector
      ),
      {

        output$stacked_sectors <- renderPlotly({
          if (all(is.na(countries()))) {
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
            ghg_by_sector_and_country |>
              filter(country %in% countries()) |>
              filter(substance %in% selected_substance()) |>
              filter(sector %in% selected_sector()) |>
              group_by(country, sector) |>
              summarise(
                emission = sum(.data[[paste0("x", inputs$kpi_years$key)]], na.rm = TRUE)
              ) |>
              ungroup() |>
              group_by(country) |>
              mutate(total_emission = sum(emission)) |>
              ungroup() |>
              arrange(desc(total_emission), country) |>
              plot_ly(
                x = ~factor(country, levels = unique(country)),
                y = ~emission,
                color = ~sector,
                colors = colors_sector |>
                  rename(
                    name = sector,
                    value = color
                  ) |>
                  select(name, value) |>
                  deframe(),
                type = "bar"
              ) |>
              layout(
                showlegend = FALSE,
                barmode = "stack",
                title = paste0(
                  "Sector contribution of selected countries in ",
                  inputs$kpi_years$key
                ),
                xaxis = list(title = "Country"),
                yaxis = list(title = "Emission by sector")
              )
          }
        })

      }
    )

    observeEvent(
      c(
        sidebar_controls$hide_sidebar_left,
        sidebar_controls$show_sidebar_right,
        countries,
        inputs$kpi_years$key,
        selected_substance,
        selected_sector
      ),
      {

        output$stacked_substance <- renderPlotly({
          if (all(is.na(countries()))) {
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
            ghg_by_sector_and_country |>
              filter(country %in% countries()) |>
              filter(sector %in% selected_sector()) |>
              filter(substance %in% selected_substance()) |>
              group_by(country, substance) |>
              summarise(
                emission = sum(.data[[paste0("x", inputs$kpi_years$key)]], na.rm = TRUE)
              ) |>
              ungroup() |>
              group_by(country) |>
              mutate(total_emission = sum(emission)) |>
              ungroup() |>
              arrange(desc(total_emission), country) |>
              plot_ly(
                x = ~factor(country, levels = unique(country)),
                y = ~emission,
                color = ~substance,
                colors = colors_substance |>
                  rename(
                    name = substance,
                    value = color
                  ) |>
                  select(name, value) |>
                  deframe(),
                type = "bar"
              ) |>
              layout(
                showlegend = FALSE,
                barmode = "stack",
                title = paste0(
                  "Substance contribution of selected countries in ",
                  inputs$kpi_years$key
                ),
                xaxis = list(title = "Country"),
                yaxis = list(title = "Emission by susbstance")
              )
          }
        })

      }
    )

  })
}
