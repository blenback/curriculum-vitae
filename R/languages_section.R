languages_section <- function(github_repo = NULL, branch = "main") {
  languages_data <- read_cv_data_remote(github_repo, "languages", branch)

  text <- sapply(
    languages_data,
    function(lang) paste0(cv_label(paste0(lang$subject, ":")), " ", lang$level),
    USE.NAMES = FALSE
  )

  cv_block("languages", "Languages", paste(text, collapse = "\n\n"))
}
