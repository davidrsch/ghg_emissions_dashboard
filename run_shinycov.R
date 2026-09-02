# shiny.cov multi-source coverage demo for the ghg app.
#
# Runs BOTH the Cypress adapter (SHINYCOV_SOURCE=cypress) and the
# shinytest2 adapter (SHINYCOV_SOURCE=shinytest2) against the same
# instrumented app, then collects them together so the report shows one
# column per test source in the Source view.
#
# Flow:
#   cleanup -> setup -> [Cypress server + cypress run + stop]
#   -> [shinytest2 AppDriver + interact + stop] -> collect -> report
#   -> to_cobertura -> cleanup

Sys.setenv(NOT_CRAN = "true")
app_dir <- getwd()

cat("== cleanup (undo any prior setup) ==\n")
try(shiny.cov::cleanup(app_dir), silent = TRUE)

cat("\n== setup() ==\n")
shiny.cov::setup(app_dir)

cat("\n== Cypress (SHINYCOV_SOURCE=cypress) ==\n")
# Start the instrumented Shiny app via the Cypress adapter's launcher.
# It sets SHINYCOV_SOURCE=cypress itself, so this run writes
# .shiny.cov/coverage.cypress.rds on shutdown.
system(
  "(node node_modules/shiny.cov-cypress/src/server.js > .shiny.cov/cypress-server.log 2>&1 & echo $! > .shiny.cov/cypress-server.pid)"
)
server_pid <- trimws(readLines(".shiny.cov/cypress-server.pid"))
on.exit(try(system(paste0("kill -TERM ", server_pid),
                   ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE),
        add = TRUE)

ready <- FALSE
for (i in 1:60) {
  ok <- system("curl -s -o /dev/null http://127.0.0.1:3333",
               ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (ok) { ready <- TRUE; break }
  Sys.sleep(2)
}
if (!ready) {
  cat("--- Cypress server log ---\n")
  cat(readLines(".shiny.cov/cypress-server.log", warn = FALSE), sep = "\n")
  stop("shiny.cov: Cypress Shiny server did not become ready on 127.0.0.1:3333")
}

cy_status <- system(
  "cd tests && npx cypress run --browser chrome --spec cypress/e2e/shinycov-demo.cy.js"
)
if (cy_status != 0) warning("shiny.cov: Cypress run exited with status ", cy_status)

# Graceful stop so the app writes coverage.cypress.rds via its shutdown hooks.
system(paste0("kill -TERM ", server_pid),
       ignore.stdout = TRUE, ignore.stderr = TRUE)
Sys.sleep(6)

cat("\n== shinytest2 (SHINYCOV_SOURCE=shinytest2) ==\n")
# SHINYCOV_OUTPUT was set by setup() in this same session, so the child
# app process inherits it; shiny.cov::AppDriver tags SHINYCOV_SOURCE=shinytest2.
app <- shiny.cov::AppDriver$new(app_dir, name = "ghg-shinytest2-demo",
                                timeout = 60000, load_timeout = 60000)
app$click(selector = "#Pivot0-Tab1")
Sys.sleep(5)
msg <- tryCatch(app$get_text("#app-keymetrics-tghge"),
                error = function(e) paste("<error>", conditionMessage(e)))
cat("keymetrics-tghge:", substr(msg, 1, 200), "\n")
Sys.sleep(4)
app$stop()

cat("\n== collect + report ==\n")
cov <- shiny.cov::collect(app_dir)
print(shiny.cov::source_counts(cov))
shiny.cov::report(cov, app_dir = app_dir,
                  file = "coverage-report/index.html", browse = FALSE)
shiny.cov::to_cobertura(cov, "cobertura.xml")

cat("\n== cleanup ==\n")
shiny.cov::cleanup(app_dir)
