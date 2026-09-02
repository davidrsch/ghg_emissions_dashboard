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

# Write a clean cobertura with ONE <line> per physical line (min hit count).
# covr::to_cobertura() duplicates every line into method-level <lines> blocks
# (its method loop isn't scoped to the current file), and codecov misreads
# those duplicates as 100% coverage.
df <- covr::tally_coverage(cov, by = "line")
agg <- stats::aggregate(df$value, by = list(filename = df$filename, line = df$line), FUN = min)
names(agg)[3] <- "value"
agg <- agg[order(agg$filename, agg$line), ]

files <- unique(agg$filename)
n <- nrow(agg)
covered <- sum(agg$value > 0)
rate <- covered / n

xml <- c('<?xml version="1.0" encoding="UTF-8"?>')
xml <- c(xml, sprintf('<coverage line-rate="%s" branch-rate="0" lines-covered="%d" lines-valid="%d" branches-covered="0" branches-valid="0" complexity="0" version="1.0" timestamp="%s">', rate, covered, n, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
xml <- c(xml, '  <sources></sources>', '  <packages>', sprintf('    <package name="shiny.cov" line-rate="%s" branch-rate="0" complexity="0">', rate), '      <classes>')
for (f in files) {
  sub <- agg[agg$filename == f, ]
  f_covered <- sum(sub$value > 0)
  f_rate <- f_covered / nrow(sub)
  xml <- c(xml, sprintf('        <class name="%s" filename="%s" line-rate="%s" branch-rate="0" complexity="0">', basename(f), f, f_rate))
  xml <- c(xml, '          <methods/>', '          <lines>')
  for (j in seq_len(nrow(sub))) {
    xml <- c(xml, sprintf('            <line number="%d" hits="%d" branch="false"/>', sub$line[j], sub$value[j]))
  }
  xml <- c(xml, '          </lines>', '        </class>')
}
xml <- c(xml, '      </classes>', '    </package>', '  </packages>', '</coverage>')
writeLines(xml, "cobertura.xml")

cat("\n== cleanup ==\n")
shiny.cov::cleanup(app_dir)
