box::use(
  shiny.fluent[Dropdown.shinyInput, Stack],
  shiny[div, moduleServer, NS, observeEvent, reactiveVal],
)

box::use(
  app/logic/data[edgar_cc],
  app/logic/get_options[get_options],
  app/view/combobox_search,
  app/view/region_info,
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
            default_key = "GLB",
            default_text = "GLB (GLOBAL)",
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
            key = "GLB",
            text = "GLB (GLOBAL)"
          ),
          region_info$ui(
            ns("second_region"),
            key = "EU27",
            text = "EU27 (EU27)"
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
    combobox_search$server(
      "select_region",
      cb_label = "Region",
      default_text = "GLB (GLOBAL)",
      cb_options = get_options(edgar_cc),
      is_visible = combobox_region_visibility
    )

    observeEvent(input$between, {
      if (input$between == "Region") {
        combobox_region_visibility(FALSE)
      } else {
        combobox_region_visibility(TRUE)
      }
    })

    first_region_key <- region_info$server(
      "first_region",
      text = "GLB (GLOBAL)",
      complementary_region = second_region_key
    )
    second_region_key <- region_info$server(
      "second_region",
      text = "EU27 (EU27)",
      complementary_region = first_region_key
    )

  })
}
