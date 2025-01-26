box::use(
  dplyr[all_of, mutate, rename, rowwise],
  tibble[is_tibble, tibble],
)

#' @export
get_options <- function(names, disabled_op = NULL) {
  if (is_tibble(names)) {
    options <- names |>
      rowwise() |>
      mutate(country = paste0(cc, " (", country, ")")) |>
      rename(key = cc, text = country)
    split_by <- dim(options)[1]
  } else {
    options <- tibble(key = names, text = names)
    split_by <- length(names)
  }

  if (!is.null(disabled_op)) {
    if (disabled_op != "") {
      options <- options |>
        mutate(
          disabled = is.element(key, all_of(disabled_op))
        )
    }
  }

  options <- options |>
    split(seq_len(split_by)) |>
    unname() |>
    lapply(function(x) {
      as.list(x)
    })
}
