#' Contact Section (Remote)
#' @description
#' Sidebar block with position, affiliation and contact links.
#' @param github_repo GitHub repository in format "owner/repo".
#' @param branch Branch name (default is "main").
#' @return A Markdown string.
contact_section <- function(github_repo = NULL, branch = "main") {
  contact_data <- read_cv_data_remote(github_repo, "contact", branch)

  # Obfuscate the visible address; the mailto: target stays usable
  email_label <- gsub("\\.", "[dot]", sub("@", "[at]", contact_data$email))

  items <- c(
    cv_item("user", contact_data$position),
    cv_item("building-columns", contact_data$institute),
    cv_item("map-location-dot", contact_data$city),
    cv_item(
      "envelope",
      sprintf("[%s](mailto:%s)", gsub("([][])", "\\\\\\1", email_label), contact_data$email)
    ),
    cv_item(
      "house",
      sprintf(
        "[%s](%s)",
        sub("/$", "", sub("https*://", "", contact_data$website)),
        contact_data$website
      )
    ),
    cv_item(
      "orcid",
      sprintf("[%s](https://orcid.org/%s)", contact_data$orcid, contact_data$orcid)
    ),
    cv_item(
      "linkedin",
      sprintf(
        "[%s](https://www.linkedin.com/in/%s)",
        contact_data$linkedin,
        contact_data$linkedin
      )
    ),
    cv_item(
      "github",
      sprintf("[%s](https://github.com/%s)", contact_data$github, contact_data$github)
    ),
    cv_item(
      "x-twitter",
      sprintf("[%s](https://twitter.com/%s)", contact_data$twitter, contact_data$twitter)
    ),
    cv_item(
      "researchgate",
      sprintf("[ResearchGate](%s)", contact_data$researchgate)
    )
  )

  cv_block("contact", "Contact Info", items)
}
