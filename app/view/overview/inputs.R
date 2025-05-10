box::use(
  dplyr[last],
  shiny.fluent[ComboBox.shinyInput, Text],
  shiny[div, hr, moduleServer, NS, reactiveVal],
  stringr[str_split_fixed],
)

box::use(
  app / logic / data[edgar_cc, ghg_tspc_years],
  app / logic / get_options[get_options],
  app / view / tool_modules / combobox_search,
  app / view / tool_modules / emissions_by,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    Text("KPIs", variant = "large"),
    hr(),
    ComboBox.shinyInput(
      ns("kpi_years"),
      label = "Year",
      value = list(
        key = last(ghg_tspc_years),
        text = last(ghg_tspc_years)
      ),
      useComboBoxAsMenuWidth = TRUE,
      options = get_options(ghg_tspc_years),
      calloutProps = list(
        styles = list(
          root = list(
            "max-height" = "300px!important"
          )
        )
      ),
      `data-test` = paste0(str_split_fixed(id, "-", 2)[2], "-kpi_years")
    ),
    combobox_search$ui(
      ns("kpi_primary_region"),
      cb_label = "Primary region",
      default_key = "GLB",
      default_text = "GLB (GLOBAL)",
      cb_options = get_options(edgar_cc),
      is_visible = TRUE
    ),
    combobox_search$ui(
      ns("kpi_secondary_region"),
      cb_label = "Secondary region",
      default_key = "EU27",
      default_text = "EU27 (EU27)",
      cb_options = get_options(edgar_cc),
      is_visible = TRUE
    ),
    div(style = "height: 10px;"),
    Text("Top emissors", variant = "large"),
    hr(),
    emissions_by$ui(
      ns("emissions_by")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    combobox_visibility <- reactiveVal(TRUE)
    combobox_pdft_value <- reactiveVal(NULL)
    combobox_sdft_value <- reactiveVal(NULL)
    combobox_search$server(
      "kpi_primary_region",
      value = combobox_pdft_value,
      cb_label = "Primary region",
      default_text = "GLB (GLOBAL)",
      cb_options = get_options(edgar_cc),
      is_visible = combobox_visibility
    )

    combobox_search$server(
      "kpi_secondary_region",
      value = combobox_sdft_value,
      cb_label = "Secondary region",
      default_text = "EU27 (EU27)",
      cb_options = get_options(edgar_cc),
      is_visible = combobox_visibility
    )

    emissions_by$server("emissions_by")

    input
  })
}
