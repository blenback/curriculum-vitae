#' Regenerate the SVG icon set in assets/icons/
#'
#' Run from the repository root: Rscript scripts/build-icons.R
#'
#' Icons are written with fill="currentColor" so they take the surrounding
#' text colour: CSS recolours them in the HTML output, and cv-icon() in
#' typst/typst-template.typ swaps in the theme colour for the PDF. Add a
#' name here (any Font Awesome name known to the fontawesome package) and
#' re-run to make a new icon available to cv_icon().

icons <- c(
  # contact
  "user", "building-columns", "map-location-dot", "envelope", "house",
  "orcid", "linkedin", "github", "x-twitter", "researchgate",
  # section headings
  "graduation-cap", "laptop", "chalkboard-user", "trophy", "newspaper",
  "comment-dots", "file", "code",
  # entry links + details
  "file-lines", "database", "globe", "file-powerpoint", "location-dot",
  "link", "r-project", "gitlab", "bitbucket",
  # HTML toolbar
  "file-arrow-down", "print"
)

out_dir <- file.path("assets", "icons")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (icon in icons) {
  svg <- as.character(fontawesome::fa(icon))
  view_box <- sub('.*viewBox="([^"]+)".*', "\\1", svg)
  path <- sub('.*<path d="([^"]+)".*', "\\1", svg)
  writeLines(
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" viewBox="%s" fill="currentColor"><path d="%s"/></svg>',
      view_box,
      path
    ),
    file.path(out_dir, paste0(icon, ".svg"))
  )
}

message("Wrote ", length(icons), " icons to ", out_dir)
