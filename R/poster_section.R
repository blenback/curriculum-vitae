poster_section <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE
) {
  posters_data <- read_cv_data_remote(github_repo, "posters", branch)

  # Most recent first
  posters_data <- rev(posters_data)

  entries <- lapply(posters_data, function(entry) {
    cv_entry(
      title = entry$title,
      org = entry$event,
      location = entry$location,
      start = entry$date,
      links = list(cv_link(entry$url, "Poster", "file"))
    )
  })

  cv_section(
    "posters",
    sprintf("Poster communications (%d)", length(entries)),
    "file",
    entries,
    page_break_after
  )
}
