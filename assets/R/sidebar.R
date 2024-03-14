sidebar <- function(
  png = "pictures/cv.png",
  contact = contact_section(),
  skills = skills_section(),
  languages = languages_section(),
  disclaimer = disclaimer_section()
) {
  cat(
    "# Aside\n",
    '```{r, out.extra = \'style="width=226px;" id="picture"\'}',
    "knitr::include_graphics({png})",
     "```",
    contact,
    skills,
    languages,
    disclaimer,
    sep = "\n"
  )
}
