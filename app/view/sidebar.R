box::use(
  shiny.fluent[IconButton.shinyInput, Text, updateIconButton.shinyInput],
  shiny[div, getDefaultReactiveDomain, moduleServer, NS, observeEvent],
  shinyjs[addClass, hide, removeClass, show],
)

#' @export
ui <- function(id, name, title, sidebar_content, sidebar_bgc, main_content, main_bgc) {
  ns <- NS(id)
  div(
    class = "ms-Grid-row",
    style = "display: flex; flex-wrap: wrap;",
    div(
      id = ns("sidebar"),
      class = "ms-Grid-col ms-sm12 ms-xl2",
      style = paste0("background-color:", sidebar_bgc, "; padding: 10px;"),
      div(
        style = "display:flex",
        Text(title, variant = "large"),
        div(style = "flex-grow: 1"),
        div(
          div(
            class = "ms-hiddenLgDown",
            IconButton.shinyInput(
              ns("hide_sidebar_left"),
              iconProps = list(iconName = "ChevronLeftMed"),
              `data-testid` = paste0("hide_sidebar", name)
            )
          ),
          div(
            class = "ms-hiddenXlUp",
            IconButton.shinyInput(
              ns("hide_sidebar_up"),
              iconProps = list(iconName = "ChevronUpMed"),
              `data-testid` = paste0("hide_sidebar", name)
            )
          )
        )
      ),
      sidebar_content
    ),
    div(
      id = ns("main"),
      class = "ms-Grid-col ms-sm12 ms-xl10",
      style = paste0("background-color:", main_bgc),
      div(
        class = "ms-hiddenLgDown",
        IconButton.shinyInput(
          ns("show_sidebar_right"),
          iconProps = list(iconName = "ChevronRightMed"),
          `data-testid` = paste0("show_sidebar", name),
          styles = list(
            root = list(
              "position" = "absolute",
              "top" = "0",
              "left" = "0",
              "z-index" = "1",
              "visibility" = "hidden"
            )
          )
        )
      ),
      div(
        class = "ms-hiddenXlUp",
        IconButton.shinyInput(
          ns("show_sidebar_down"),
          iconProps = list(iconName = "ChevronDownMed"),
          `data-testid` = paste0("show_sidebar", name),
          styles = list(
            root = list(
              "position" = "absolute",
              "top" = "0",
              "right" = "0",
              "z-index" = "1",
              "visibility" = "hidden"
            )
          )
        )
      ),
      main_content
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(c(input$hide_sidebar_left, input$hide_sidebar_up), {
      hide("sidebar")
      removeClass("main", "ms-xl10")
      updateIconButton.shinyInput(
        session = getDefaultReactiveDomain(),
        "show_sidebar_right",
        styles = list(
          root = list(
            "position" = "absolute",
            "top" = "0",
            "left" = "0",
            "z-index" = "1",
            "visibility" = "show"
          )
        )
      )
      updateIconButton.shinyInput(
        session = getDefaultReactiveDomain(),
        "show_sidebar_down",
        styles = list(
          root = list(
            "position" = "absolute",
            "top" = "0",
            "right" = "0",
            "z-index" = "1",
            "visibility" = "show"
          )
        )
      )
    })

    observeEvent(c(input$show_sidebar_right, input$show_sidebar_down), {
      show("sidebar")
      addClass("main", "ms-xl10")
      updateIconButton.shinyInput(
        session = getDefaultReactiveDomain(),
        "show_sidebar_right",
        styles = list(
          root = list(
            "position" = "absolute",
            "top" = "0",
            "left" = "0",
            "z-index" = "1",
            "visibility" = "hidden"
          )
        )
      )
      updateIconButton.shinyInput(
        session = getDefaultReactiveDomain(),
        "show_sidebar_down",
        styles = list(
          root = list(
            "position" = "absolute",
            "top" = "0",
            "right" = "0",
            "z-index" = "1",
            "visibility" = "hidden"
          )
        )
      )
    })
    
    return(input)
  })
}
