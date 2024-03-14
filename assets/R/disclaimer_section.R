disclaimer_section <- function(text = NULL) {
  sprintf(
    "## Disclaimer {#disclaimer style='width: var(--sidebar-width); padding-left: var(--sidebar-horizontal-padding);'}\n\n\n\n%sLast updated on %s.\n\n",
    if (is.null(text)) "" else sprintf("%s\n\n", text),
    Sys.Date()
  )
}
