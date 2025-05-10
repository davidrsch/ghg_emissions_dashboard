box::use(
  dplyr[distinct, filter, pull, rename, select],
  plotly[animation_slider, config, event_data, layout, plot_ly, plotlyOutput],
  plotly[renderPlotly],
  shiny.fluent[Dropdown.shinyInput, Toggle.shinyInput],
  shiny.fluent[updateDropdown.shinyInput],
  shiny[div, getDefaultReactiveDomain, isolate, moduleServer, NS],
  shiny[observeEvent, reactiveVal, req, tagAppendAttributes],
  stats[na.omit],
  stringr[str_split_fixed],
)

box::use(
  app / logic / data[continents, ghg_capita_globe_map],
  app / logic / data[ghg_gdp_globe_map],
  app / logic / data[ghg_sector_globe_map],
  app / logic / data[ghg_totals_globe_map, globe_cc],
  app / logic / get_options[get_options],
  app / logic / top_regions_help[get_countries_he, get_plot_title],
  app / view / tool_modules / emissions_by,
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
        ),
        `data-test` = paste0(
          str_split_fixed(id, "-", 2)[2],
          "-selected_regions"
        )
      ),
      Dropdown.shinyInput(
        ns("selected_countries"),
        label = "Country",
        value = globe_cc$cc,
        options = globe_cc |>
          get_options(),
        multiSelect = TRUE,
        calloutProps = list(
          styles = list(
            root = list(
              "max-height" = "300px!important"
            )
          )
        ),
        `data-test` = paste0(
          str_split_fixed(id, "-", 2)[2],
          "-selected_countries"
        )
      ),
      class = "ms-Grid-col ms-sm1 ms-md2",
      style = "padding-left: 20px;"
    ),
    div(
      plotlyOutput(ns("map"), height = "50rem"),
      class = "ms-Grid-col ms-sm10 ms-md8"
    ) |>
      tagAppendAttributes(
        `data-test` = paste0(
          str_split_fixed(id, "-", 2)[2],
          "-map"
        )
      ),
    div(
      Toggle.shinyInput(
        label = "3D Globe",
        ns("is_3d_globe"),
        value = TRUE,
        `data-test` = paste0(
          str_split_fixed(id, "-", 2)[2],
          "-is_3d_globe"
        )
      ),
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

    camera_settings <- reactiveVal(NULL)

    observeEvent(input$is_3d_globe, {
      if (input$is_3d_globe) {
        lon <- 0.2398806
        lat <- -1.139812
        scale <- 0.8705506
      } else {
        lon <- 0.6595555
        lat <- 0.5113528
        scale <- 1
      }
      camera_settings(
        list(
          lon = lon,
          lat = lat,
          scale = scale
        )
      )
    })

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
      fig <- plot_ly(
        data = filtered_data,
        type = "choropleth",
        locations = filtered_data$cc,
        locationmode = "country names",
        frame = ~year,
        z = filtered_data$value,
        colorscale = "Redor",
        reversescale = FALSE,
        source = ns("globe")
      ) |>
        config(displayModeBar = "always") |>
        layout(
          title = plot_title,
          margin = list(t = 80),
          geo = list(
            showframe = TRUE,
            showcoastlines = TRUE,
            projection = list(type = globe_type)
          )
        ) |>
        animation_slider(active = length(unique(filtered_data$year)) - 1)

      last_camera <- isolate(camera_settings())
      if (!is.null(last_camera)) {
        if (globe_type == "orthographic") {
          fig <- fig |>
            layout(
              geo = list(
                projection = list(
                  rotation = list(
                    lon = last_camera$lon,
                    lat = last_camera$lat
                  ),
                  scale = last_camera$scale
                )
              )
            )
        } else if (globe_type == "natural earth") {
          fig <- fig |>
            layout(
              geo = list(
                center = list(
                  lon = last_camera$lon,
                  lat = last_camera$lat
                ),
                projection = list(
                  scale = last_camera$scale
                )
              )
            )
        }
      }

      return(fig)
    })

    observeEvent(event_data("plotly_relayout", ns("globe")), {
      data_event <- event_data("plotly_relayout", ns("globe"))
      if (input$is_3d_globe) {
        lon <- data_event$geo.projection.rotation.lon
        lat <- data_event$geo.projection.rotation.lat
        scale <- data_event$geo.projection.scale
      } else {
        lon <- data_event$geo.center.lon
        lat <- data_event$geo.center.lat
        scale <- data_event$geo.projection.scale
      }
      if (!is.null(scale)) {
        camera_settings(
          list(
            lon = lon,
            lat = lat,
            scale = scale
          )
        )
      } else {
        camera_settings(
          list(
            lon = lon,
            lat = lat,
            scale = 0.7
          )
        )
      }
    })
  })
}
