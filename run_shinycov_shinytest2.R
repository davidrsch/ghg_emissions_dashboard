# shiny.cov shinytest2 demo for the ghg app.
# Flow: cleanup any prior setup -> setup -> AppDriver -> interact -> collect -> report.
Sys.setenv(NOT_CRAN = "true")
app_dir <- getwd()

cat("== cleanup (undo Cypress demo setup) ==\n")
try(shiny.cov::cleanup(app_dir), silent = TRUE)

cat("\n== setup() ==\n")
shiny.cov::setup(app_dir)

cat("\n== launch AppDriver ==\n")
app <- shiny.cov::AppDriver$new(app_dir, name = "ghg-shinytest2-demo", timeout = 60000, load_timeout = 60000)

cat("\n== switch to Overview tab ==\n")
app$click(selector = "#Pivot0-Tab1")
Sys.sleep(5)

cat("\n== read all values (logs get_value per input, get_text per output) ==\n")
vals <- app$get_values()

cat("\n== verify one output ==\n")
msg <- app$get_text("#app-keymetrics-tghge")
cat("keymetrics-tghge:", substr(msg, 1, 200), "\n")

cat("\n== waiting for periodic background save ==\n")
Sys.sleep(4)

cat("\n== stop ==\n")
app$stop()

cat("\n== collect + report ==\n")
cov <- shiny.cov::collect(app_dir)
covr::to_cobertura(cov, "cobertura.xml")

cat("\n== cleanup ==\n")
shiny.cov::cleanup(app_dir)
