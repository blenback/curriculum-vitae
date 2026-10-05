workshop_section <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE
) {
  events_data <- read_cv_data_remote(github_repo, "events", branch)

  entries <- lapply(events_data, function(entry) {
    cv_entry(
      title = entry$title,
      org = if (!is_blank(entry$description)) paste0("*", clean_text(entry$description), "*"),
      location = entry$location,
      start = entry$start,
      end = entry$end,
      links = list(cv_typed_link(entry$url, "website"))
    )
  })

  cv_section(
    "teaching",
    "Teaching, Training & Session Convening",
    "chalkboard-user",
    entries,
    page_break_after
  )
}
