box::use(
  purrr[discard],
  shiny.fluent[ComboBox.shinyInput, updateComboBox.shinyInput],
  shiny[getDefaultReactiveDomain, moduleServer, NS, observeEvent],
  stringr[str_detect, str_replace, str_split_i, str_to_lower],
)

#' @export
ui <- function(id, cb_label, default_key, default_text, cb_options) {
  ns <- NS(id)
  ComboBox.shinyInput(
    ns("searchable_cb"),
    label = cb_label,
    value = list(
      key = default_key,
      text = default_text
    ),
    allowFreeform = TRUE,
    useComboBoxAsMenuWidth = TRUE,
    options = cb_options,
    calloutProps = list(
      styles = list(
        root = list(
          "max-height" = "300px!important"
        )
      )
    ),
    `data-test` = str_split_i(id, "-", -1)
  )
}

#' @export
server <- function(id, default_text, cb_options) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Implement fuzy search in the combobox
    observeEvent(input$searchable_cb_query, {
      query <- input$searchable_cb_query
      all_options <- cb_options

      if (query == "") {
        query <- default_text
      }

      # Filter options based on the query
      if (!is.null(query)) {
        filtered_options <- all_options |>
          lapply(function(x) {
            query_to <- query |>
              str_replace("\\(", "") |>
              str_replace("\\)", "") |>
              str_to_lower()
            text <- x$text |>
              str_replace("\\(", "") |>
              str_replace("\\)", "") |>
              str_to_lower()
            if (str_detect(text, query_to)) {
              x
            } else {
              NULL
            }
          })
        filtered_options <- filtered_options |>
          discard(is.null)
      } else {
        filtered_options <- all_options
      }

      # Update the ComboBox with filtered options
      updateComboBox.shinyInput(
        session = getDefaultReactiveDomain(),
        "searchable_cb",
        options = filtered_options
      )
    })

    # # Update combo box to include all options once one it's selected
    observeEvent(input$searchable_cb, {
      all_options <- cb_options

      updateComboBox.shinyInput(
        session = getDefaultReactiveDomain(),
        "searchable_cb",
        options = all_options
      )
    })

  })
}
