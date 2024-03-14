workshop_section <- function(xlsx = "data/cv.xlsx", sheet = "workshop", page_break_after = FALSE, colour = "#333333") {
  text <- read_excel_sheet(xlsx, sheet)[
    i = .N:1,
    j = sprintf(
      "### %s\n\n%s\n\n%s\n\n%s\n\n::: aside\n%s\n:::\n\n\n\n",
      title, type, city, date, add_item_logo(url, type = "github", colour)
    )
  ]

  if (page_break_after) {
    c(sprintf("## Workshop Experience  {data-icon=chalkboard-teacher .break-after-me}"), text)
  } else {
    c(sprintf("## Workshop Experience  {data-icon=chalkboard-teacher}"), text)
  }
}

