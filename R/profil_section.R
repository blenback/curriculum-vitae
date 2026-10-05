#' Page header: name, profile text and (HTML only) the toolbar
#' @param author Name shown as the CV title.
#' @param toolbar Markdown for the action buttons, e.g. from toolbar_section().
profil_section <- function(
  github_repo = NULL,
  branch = "main",
  author = NULL,
  toolbar = NULL
) {
  profile_data <- read_cv_data_remote(github_repo, "profile", branch)

  # The profile data is a list with one item containing text
  text <- clean_text(profile_data[[1]]$text)

  paste(
    c(
      "::: {.cv-header}",
      cv_div("cv-name", author),
      toolbar,
      cv_div("cv-profile", text),
      ":::",
      ""
    ),
    collapse = "\n"
  )
}

#' HTML-only action buttons (download the PDF, print)
#' @description
#' Raw HTML, so the Typst output drops it automatically.
#' @param pdf_file The PDF's file name (format typst `output-file` in _quarto.yml).
toolbar_section <- function(pdf_file = "benjamin-black-cv.pdf") {
  icon_svg <- function(name) {
    paste(readLines(file.path("assets", "icons", paste0(name, ".svg")), warn = FALSE), collapse = "")
  }
  paste(
    c(
      "```{=html}",
      '<nav class="cv-toolbar" aria-label="CV actions">',
      sprintf(
        '<a class="cv-button cv-button-primary" href="%s" download>%s<span>Download PDF</span></a>',
        pdf_file,
        sub("<svg ", '<svg class="cv-icon" aria-hidden="true" ', icon_svg("file-arrow-down"))
      ),
      sprintf(
        '<a class="cv-button" href="%s" target="_blank" rel="noopener">%s<span>View PDF</span></a>',
        pdf_file,
        sub("<svg ", '<svg class="cv-icon" aria-hidden="true" ', icon_svg("file-lines"))
      ),
      "</nav>",
      "```"
    ),
    collapse = "\n"
  )
}
