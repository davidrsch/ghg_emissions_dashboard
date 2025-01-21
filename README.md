# ghg_emissions_dashboard
Welcome to the Greenhouse Gas Emissions Insights Dashboard repository! This project provides a dynamic, interactive visualization tool built using data from the 2024 Emissions Database for Global Atmospheric Research (EDGAR). The dashboard is designed to make complex data on global greenhouse gas (GHG) emissions accessible, engaging, and insightful.

You can see the dependencies in the dependecies.R file

Launch application:

```r
shiny::runGitHub(repo = "davidrsch/ghg_emissions_dashboard", ref = "main")
```

<!--- or try it at connect.posit.cloud. --->

## Features
* Global Emissions Trends: Explore year-over-year changes in total global emissions.
* Country Comparisons: Compare emissions across different nations and regions, including the top global emitters.
* Sectoral Analysis: Break down emissions by sectors such as energy, industry, and agriculture.
* Interactive Visualizations: Hover over charts, filter data, and customize views to generate insights.
* Data Export: Download data visualizations and summaries for further use.

## Data Sources
This dashboard uses the 2024 EDGAR GHG dataset published by the Joint Research Centre (JRC) of the European Commission. The dataset includes country-level and sectoral emissions estimates. For more information, visit:
https://edgar.jrc.ec.europa.eu.

## Usage
The dashboard is an independent tool created for educational, exploratory, and analytical purposes. It does not represent the official views of the European Commission. Users should cite the data as specified in official source, which can be found at:

> EDGAR 2024 Global Greenhouse Gas Emissions Data, Emissions Database for Global Atmospheric Research (EDGAR), Joint Research Centre (JRC) of the European Commission.
