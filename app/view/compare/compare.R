box::use(
  dplyr[last],
  shiny.fluent[Dropdown.shinyInput, Stack],
  shiny[div, moduleServer, NS, observeEvent, reactiveVal],
)

box::use(
  app/logic/data[edgar_cc, ghg_tspc_years],
  app/logic/get_options[get_options],
  app/view/tool_modules/combobox_search,
  app/view/tool_modules/region_info,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "ms-Grid-row",
    style = "display: flex;",
    div(class = "ms-Grid-col ms-sm1 ms-md2"),
    div(
      div(
        div(
          Dropdown.shinyInput(
            ns("between"),
            label = "Compare:",
            value = "Region",
            options = list(
              list(
                key = "Region",
                text = "Region"
              ),
              list(
                key = "Period",
                text = "Period"
              )
            ),
            calloutProps = list(
              styles = list(
                root = list(
                  "max-height" = "300px!important"
                )
              )
            )
          ),
          combobox_search$ui(
            ns("select_region"),
            cb_label = "Region",
            default_key = "EU27",
            default_text = "EU27 (EU27)",
            cb_options = get_options(edgar_cc),
            is_visible = FALSE
          )
        ),
        style = "width: 20%; margin-left: 40%; margin-right: 40%;"
      ),
      div(
        Stack(
          horizontal = TRUE,
          gap = 10,
          region_info$ui(
            ns("first_region"),
            key_region = "GLB",
            text_region = "GLB (GLOBAL)",
            visible_region = TRUE,
            key_year = last(ghg_tspc_years),
            text_year = last(ghg_tspc_years),
            visible_year = FALSE
          ),
          region_info$ui(
            ns("second_region"),
            key_region = "EU27",
            text_region = "EU27 (EU27)",
            visible_region = TRUE,
            key_year = last(ghg_tspc_years),
            text_year = last(ghg_tspc_years),
            visible_year = FALSE
          )
        ),
        style = "margin-top: 20px; padding: 20px; background-color: #DADAD9;"
      ),
      class = "ms-Grid-col ms-sm10 ms-md8"
    ),
    div(class = "ms-Grid-col ms-sm1 ms-md2")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    combobox_region_visibility <- reactiveVal(FALSE)
    combobox_regions_visibility <- reactiveVal(TRUE)
    combobox_years_visibility <- reactiveVal(FALSE)
    region_default_value <- reactiveVal(NULL)
    first_region_default_value <- reactiveVal(
      list(
        key = "GLB",
        text = "GLB (GLOBAL)"
      )
    )
    second_region_default_value <- reactiveVal(
      list(
        key = "EU27",
        text = "EU27 (EU27)"
      )
    )
    first_year_default_value <- reactiveVal(NULL)
    second_year_default_value <- reactiveVal(NULL)

    combobox_search$server(
      "select_region",
      value = region_default_value,
      cb_label = "Region",
      default_text = "GLB (GLOBAL)",
      cb_options = get_options(edgar_cc),
      is_visible = combobox_region_visibility
    )

    observeEvent(input$between, {
      if (input$between == "Region") {
        combobox_region_visibility(FALSE)
        combobox_regions_visibility(TRUE)
        combobox_years_visibility(FALSE)
        first_region_default_value(
          list(
            key = "GLB",
            text = "GLB (GLOBAL)"
          )
        )
        second_region_default_value(
          list(
            key = "EU27",
            text = "EU27 (EU27)"
          )
        )
        first_year_default_value(
          list(
            key = last(ghg_tspc_years),
            text = last(ghg_tspc_years)
          )
        )
        second_year_default_value(
          list(
            key = last(ghg_tspc_years),
            text = last(ghg_tspc_years)
          )
        )
      } else {
        combobox_region_visibility(TRUE)
        combobox_regions_visibility(FALSE)
        combobox_years_visibility(TRUE)
        region_default_value(
          list(
            key = "GLB",
            text = "GLB (GLOBAL)"
          )
        )
        first_year_default_value(
          list(
            key = last(ghg_tspc_years),
            text = last(ghg_tspc_years)
          )
        )
        second_year_default_value(
          list(
            key = (as.numeric(last(ghg_tspc_years)) - 1),
            text = (as.numeric(last(ghg_tspc_years)) - 1)
          )
        )
      }
    })

    observeEvent(input$`select_region-searchable_cb`, {
      if (combobox_region_visibility()) {
        first_region_default_value(
          list(
            key = input$`select_region-searchable_cb`$key,
            text = input$`select_region-searchable_cb`$text
          )
        )
        second_region_default_value(
          list(
            key = input$`select_region-searchable_cb`$key,
            text = input$`select_region-searchable_cb`$text
          )
        )
      }
    })

    first_region_key <- region_info$server(
      "first_region",
      value_region = first_region_default_value,
      text_region = "GLB (GLOBAL)",
      complementary_region = second_region_key$region,
      visible_region = combobox_regions_visibility,
      value_year = first_year_default_value,
      text_year = last(ghg_tspc_years),
      complementary_year = second_region_key$year,
      visible_year = combobox_years_visibility
    )
    second_region_key <- region_info$server(
      "second_region",
      value_region = second_region_default_value,
      text_region = "EU27 (EU27)",
      complementary_region = first_region_key$region,
      visible_region = combobox_regions_visibility,
      value_year = second_year_default_value,
      text_year = last(ghg_tspc_years),
      complementary_year = first_region_key$year,
      visible_year = combobox_years_visibility
    )

  })
}
