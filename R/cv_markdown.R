#' Format-neutral CV building blocks
#' @description
#' Every section function returns Pandoc Markdown built from these helpers:
#' fenced divs and spans with `cv-*` classes, never raw HTML or Typst.
#' filters/cv.lua turns that structure into the HTML layout or into calls to
#' the Typst functions in typst/typst-template.typ, so the same content feeds
#' both outputs.

#' TRUE for NULL, NA, empty or whitespace-only values
is_blank <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x[[1]]) || !nzchar(trimws(x[[1]]))
}

#' Collapse the stray line breaks (incl. Windows \r\n) some YAML values carry
clean_text <- function(x) {
  if (is_blank(x)) return("")
  trimws(gsub("\\s*\r?\n\\s*", " ", as.character(x)))
}

#' Inline icon from assets/icons/<name>.svg (see scripts/build-icons.R)
cv_icon <- function(name) {
  sprintf('[]{.cv-icon icon="%s"}', name)
}

#' A labelled link with a leading icon, or NULL when there is no URL
cv_link <- function(url, label, icon) {
  if (is_blank(url)) return(NULL)
  sprintf("[%s %s](%s)", cv_icon(icon), label, trimws(url))
}

#' Link whose icon + label is picked from a `link_type` value in the data
cv_typed_link <- function(url, type = "website") {
  switch(
    if (is_blank(type)) "website" else type,
    github       = cv_link(url, "GitHub", "github"),
    gitlab       = cv_link(url, "GitLab", "gitlab"),
    bitbucket    = cv_link(url, "Bitbucket", "bitbucket"),
    cran         = cv_link(url, "CRAN", "r-project"),
    presentation = cv_link(url, "Slides", "file-powerpoint"),
    link         = cv_link(url, "Link", "link"),
    cv_link(url, "Website", "globe")
  )
}

#' A fenced div holding `content`, or nothing when `content` is blank
cv_div <- function(class, content) {
  if (is_blank(content)) return(character(0))
  c(paste0("::: ", class), content, ":::")
}

#' One timeline entry (a row in a CV section)
#' @param title,org,location Single-line Markdown.
#' @param start,end Date labels shown in the date column (plain text).
#' @param body Character vector of Markdown paragraphs.
#' @param links Character vector of links from cv_link().
cv_entry <- function(
  title,
  org = NULL,
  location = NULL,
  start = NULL,
  end = NULL,
  body = NULL,
  links = NULL
) {
  attrs <- ".cv-entry"
  if (!is_blank(start)) attrs <- c(attrs, sprintf('start="%s"', clean_text(start)))
  if (!is_blank(end)) attrs <- c(attrs, sprintf('end="%s"', clean_text(end)))

  body <- Filter(Negate(is_blank), as.list(body))
  links <- Filter(Negate(is.null), as.list(links))

  paste(
    c(
      sprintf("::: {%s}", paste(attrs, collapse = " ")),
      cv_div("cv-title", clean_text(title)),
      cv_div("cv-org", clean_text(org)),
      cv_div("cv-location", clean_text(location)),
      if (length(body)) cv_div("cv-body", paste(unlist(body), collapse = "\n\n")),
      if (length(links)) cv_div("cv-links", paste(unlist(links), collapse = "\n\n")),
      ":::",
      ""
    ),
    collapse = "\n"
  )
}

#' A titled main-column section wrapping a set of cv_entry() strings
#' @param page_break_after Start a new PDF page after this section (ignored
#'   by the HTML page, which is not paginated).
cv_section <- function(id, title, icon, entries, page_break_after = FALSE) {
  attrs <- sprintf('.cv-section #%s title="%s" icon="%s"', id, title, icon)
  if (isTRUE(page_break_after)) attrs <- paste(attrs, 'break-after="true"')
  paste(
    c(sprintf("::: {%s}", attrs), "", unlist(entries), ":::", ""),
    collapse = "\n"
  )
}

#' A titled sidebar block
cv_block <- function(id, title, content) {
  paste(
    c(sprintf('::: {.cv-block #%s title="%s"}', id, title), content, ":::", ""),
    collapse = "\n"
  )
}

#' Sidebar row with a fixed-width icon gutter
cv_item <- function(icon, content) {
  paste0('::: {.cv-item icon="', icon, '"}\n', content, "\n:::\n")
}

#' Bold sidebar label, e.g. "Advanced:" in the skills block
cv_label <- function(text) {
  sprintf("[%s]{.cv-label}", text)
}
