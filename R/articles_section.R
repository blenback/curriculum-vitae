articles_section <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  max_entries = NULL
) {
  publications_data <- read_cv_data_remote(github_repo, "publications", branch)

  # Most recent first
  publications_data <- rev(publications_data)

  if (!is.null(max_entries)) {
    publications_data <- head(publications_data, max_entries)
  }

  entries <- lapply(publications_data, function(pub) {
    cv_entry(
      # BibTeX-style case protection, e.g. "({LUCC})"
      title = gsub("[{}]", "", pub$title),
      org = gsub("Black, B\\.", "[Black, B.]{.underline}", clean_text(pub$authors)),
      start = pub$year,
      body = paste0("*", clean_text(pub$journal), "*"),
      links = list(
        cv_link(pub$doi, "Read", "file-lines"),
        cv_link(pub$data_url, "Data", "database"),
        cv_link(pub$code_url, "Code", "code")
      )
    )
  })

  cv_section("publications", "Publications", "newspaper", entries, page_break_after)
}
