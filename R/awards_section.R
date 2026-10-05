awards_section <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE
) {
  awards_data <- read_cv_data_remote(github_repo, "awards", branch)

  # Newest first
  awards_data <- rev(awards_data)

  entries <- lapply(awards_data, function(x) {
    cv_entry(
      title = x$name,
      org = x$institute,
      location = x$city,
      start = x$date,
      body = if (!is_blank(x$description)) paste0("*", clean_text(x$description), "*"),
      links = list(cv_typed_link(x$url, x$link_type))
    )
  })

  cv_section("grants", "Grants & Funding", "trophy", entries, page_break_after)
}
