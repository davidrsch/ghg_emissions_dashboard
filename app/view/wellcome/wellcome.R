box::use(
  shiny[div, h1, h2, p, tags],
)

#' @export
ui <- function(id) {
  div(
    class = "ms-Grid-row",
    style = "display: flex;",
    div(class = "ms-Grid-col ms-sm1 ms-md2"),
    div(
      h1(
        "Welcome to the Greenhouse Gas Emissions Insights Dashboard",
        style = "text-align: center;"
      ),
      p(
        "This dashboard is an independently developed tool designed to provide a dynamic",
        " exploration of global greenhouse gas  (GHG) emission trends. Built using publicly",
        " available data from the 2024 Emissions Database for Global Atmospheric Research",
        " (EDGAR), it offers accessible visualizations and insights into worldwide emission",
        " patterns up to 2023.",
        style = "font-size: large;"
      ),
      h2("What to Explore:"),
      tags$ul(
        tags$li(
          "Global Emission Trends: Track how emissions have evolved over the past decades,",
          " revealing patterns and significant global shifts."
        ),
        tags$li(
          "Country Comparisons: See how major emitters—including China, the U.S., and the ",
          "EU—compare in terms of emissions per capita, by sector, and relative to economic ",
          "output."
        ),
        tags$li(
          "Sectoral Contributions: Break down the major sources of emissions, from energy ",
          "production to industrial activities and land use."
        ),
        style = "font-size: large;"
      ),
      h2("Data and Sources"),
      p(
        "The content here uses estimates from the EDGAR 2024 report. This effort does not ",
        "reflect official representations from the European Commission or its agencies but is ",
        "intended as a user-friendly exploration based on publicly released information.",
        style = "font-size: large;"
      ),
      h2("Features for You:"),
      tags$ul(
        tags$li("Interactive Visualization: Hover, zoom, and filter data for tailored insights."),
        tags$li(
          "Export Options: Download data summaries or visual assets for personal or",
          " educational use."
        ),
        tags$li("Custom Comparisons: Build country-to-country or sector-specific comparisons."),
        style = "font-size: large;"
      ),
      h2("Authorship"),
      p(
        "This dashboard was conceptualized and developed by David Díaz Rodríguez, with the aim",
        " of making complex greenhouse gas emission data more accessible and interactive for a ",
        "broader audience. It leverages modern data visualization techniques to enhance ",
        "understanding and awareness.",
        style = "font-size: large;"
      ),
      h2("Citation Section"),
      p(
        "If using information from this dashboard, please refer to the original source that can",
        " be found at:",
        style = "font-size: large;"
      ),
      tags$cite(
        "EDGAR 2024 Global Greenhouse Gas Emissions Data, Emissions Database for Global ",
        "Atmospheric Research (EDGAR), Joint Research Centre (JRC) of the European ",
        "Commission. Data available at: ",
        tags$a(href = "https://edgar.jrc.ec.europa.eu", "https://edgar.jrc.ec.europa.eu"),
        style = "font-size: large;"
      ),
      h2("A Word of Caution"),
      p(
        "While the data underpinning this dashboard comes from trusted sources, this tool is",
        " an independent creation. For rigorous research or policymaking, please consult the ",
        "original EDGAR reports directly.",
        style = "font-size: large;"
      ),
      p(
        "I hope this dashboard inspires exploration and informed discussions about reducing ",
        "emissions and combating climate change!",
        style = "font-size: large;"
      ),
      class = "ms-Grid-col ms-sm10 ms-md8"
    ),
    div(class = "ms-Grid-col ms-sm1 ms-md2")
  )
}
