box::use(
  dplyr[distinct, filter, pull, rename, select],
  plotly[config, layout, plot_ly, plotlyOutput, renderPlotly],
  shiny.fluent[Dropdown.shinyInput, Toggle.shinyInput, updateDropdown.shinyInput],
  shiny[div, getDefaultReactiveDomain, moduleServer, NS, observeEvent, req],
  stats[na.omit],
)

box::use(
  app/logic/data[continents,  ghg_capita_globe_map, ghg_gdp_globe_map, ghg_sector_globe_map],
  app/logic/data[ghg_totals_globe_map, globe_cc],
  app/logic/get_options[get_options],
  app/logic/top_regions_help[get_countries_he, get_plot_title],
  app/view/emissions_by,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "ms-Grid-row",
    style = "display: flex;",
    div(
      Dropdown.shinyInput(
        ns("selected_regions"),
        label = "Macro regions",
        value = unique(continents$continent_code),
        options = continents |>
          select(continent_code, continent_name) |>
          rename(
            cc = continent_code,
            country = continent_name
          ) |>
          distinct() |>
          get_options(),
        multiSelect = TRUE,
        calloutProps = list(
          styles = list(
            root = list(
              "max-height" = "300px!important"
            )
          )
        )
      ),
      Dropdown.shinyInput(
        ns("selected_countries"),
        label = "Country",
        value = "",
        options = globe_cc |>
          get_options(),
        multiSelect = TRUE,
        calloutProps = list(
          styles = list(
            root = list(
              "max-height" = "300px!important"
            )
          )
        )
      ),
      class = "ms-Grid-col ms-sm1 ms-md2",
      style = "padding-left: 20px;"
    ),
    div(
      plotlyOutput(ns("map"), height = "50rem"),
      class = "ms-Grid-col ms-sm10 ms-md8"
    ),
    div(
      Toggle.shinyInput(label = "3D Globe", ns("is_3d_globe"), value = TRUE),
      emissions_by$ui(ns("emissions_by")),
      class = "ms-Grid-col ms-sm1 ms-md2",
      style = "padding-right: 20px;"
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$selected_regions, {
      selected <- globe_cc |>
        filter(continent_code %in% input$selected_regions) |>
        select(cc) |>
        pull()
      updateDropdown.shinyInput(
        session = getDefaultReactiveDomain(),
        "selected_countries",
        value = selected
      )
    })

    emissions_by$server("emissions_by")

    output$map <- renderPlotly({
      req(input$selected_countries)
      
      # Extract selected keys properly
      selected_countries <- globe_cc |>
        filter(cc %in% input$selected_countries) |>
        select(country) |>
        pull() |>
        na.omit()

      # Get the plot data
      filtered_data <- get_countries_he(
        input$`emissions_by-emissions_by`,
        selected_countries,
        input$`emissions_by-emissions_by_sectors`,
        input$`emissions_by-emissions_by_substance`,
        total_data = ghg_totals_globe_map,
        capita_data = ghg_capita_globe_map,
        gpd_data = ghg_gdp_globe_map,
        sector_substance_data = ghg_sector_globe_map
      ) |>
        filter(year == "2023") |>
        rename(
          cc = country,
          value = emission
        )
      
      plot_title <- get_plot_title(
        input$`emissions_by-emissions_by`,
        input$`emissions_by-emissions_by_sectors`,
        input$`emissions_by-emissions_by_substance`
      )
      
      # Get the plot type
      globe_type <- ifelse(input$is_3d_globe, "orthographic", "natural earth")

      # Create Plotly map
      plot_ly(
        data = filtered_data,
        type = "choropleth",
        locations = filtered_data$cc,
        locationmode = "country names",
        z = filtered_data$value,
        colorscale = "Redor",
        reversescale = FALSE
      ) |>
        config(displayModeBar = "always") |>
        layout(
          title = paste0(plot_title, ", year 2023"),
          margin = list(t = 80),
          geo = list(
            showframe = TRUE,
            showcoastlines = TRUE,
            projection = list(type = globe_type)
          )
        )
    })

  })
}
