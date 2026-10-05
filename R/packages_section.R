packages_section <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE
) {
  packages_data <- read_cv_data_remote(github_repo, "packages", branch)

  # Newest first
  packages_data <- rev(packages_data)

  entries <- lapply(packages_data, function(x) {
    cv_entry(
      title = x$name,
      start = x$year,
      body = if (!is_blank(x$description)) paste0("*", clean_text(x$description), "*"),
      links = list(cv_typed_link(x$url, x$link_type))
    )
  })

  cv_section(
    "packages",
    sprintf("R Packages (%d)", length(entries)),
    "code",
    entries,
    page_break_after
  )
}
