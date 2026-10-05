disclaimer_section <- function(text = NULL) {
  cv_div(
    "cv-disclaimer",
    paste0(
      if (is.null(text)) "" else paste0(text, "\n\n"),
      "Last updated on ", Sys.Date(), "."
    )
  ) |>
    paste(collapse = "\n")
}
