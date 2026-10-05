presentations_section <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  max_entries = NULL
) {
  presentations_data <- read_cv_data_remote(github_repo, "presentations", branch)

  # Most recent first
  presentations_data <- rev(presentations_data)

  if (!is.null(max_entries)) {
    presentations_data <- head(presentations_data, max_entries)
  }

  entries <- lapply(presentations_data, function(entry) {
    # Dates come as ISO ("2021-06-09") or already formatted ("Jun. 2021")
    formatted_date <- tryCatch(
      {
        parsed_date <- as.Date(
          entry$date,
          tryFormats = c("%Y-%m-%d", "%b. %Y", "%b %Y")
        )
        if (is.na(parsed_date)) as.character(entry$date) else format(parsed_date, "%b %Y")
      },
      error = function(e) as.character(entry$date)
    )

    cv_entry(
      title = entry$title,
      org = entry$event,
      location = entry$location,
      start = formatted_date,
      links = list(cv_typed_link(entry$url, "presentation"))
    )
  })

  cv_section("presentations", "Presentations", "comment-dots", entries, page_break_after)
}
