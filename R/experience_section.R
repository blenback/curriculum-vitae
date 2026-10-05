#' @param use_bullets Split activities into one bullet per sentence; otherwise
#'   they are shown as a single italic paragraph.
#' @param reverse_order Reverse the order of the data file.
experience_section <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  use_bullets = TRUE,
  reverse_order = TRUE,
  max_entries = NULL
) {
  experience_data <- read_cv_data_remote(github_repo, "experience", branch)

  if (!is.null(max_entries)) {
    experience_data <- head(experience_data, max_entries)
  }
  if (reverse_order) {
    experience_data <- rev(experience_data)
  }

  entries <- lapply(experience_data, function(x) {
    activities <- clean_text(x$activities)
    body <- if (is_blank(activities)) {
      NULL
    } else if (use_bullets) {
      sentences <- strsplit(activities, "(?<=\\.)\\s+", perl = TRUE)[[1]]
      paste0("- ", sentences, collapse = "\n")
    } else {
      paste0("*", activities, "*")
    }

    cv_entry(
      title = x$position,
      org = x$institute,
      location = x$city,
      start = x$start,
      end = x$end,
      body = body
    )
  })

  cv_section("experience", "Professional Experience", "laptop", entries, page_break_after)
}
