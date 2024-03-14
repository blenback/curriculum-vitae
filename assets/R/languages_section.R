languages_section <- function(xlsx = "data/cv.xlsx", sheet = "languages") {
  # text <- read_excel_sheet(xlsx, sheet)[
  #   j = list(what = paste(
  #     paste(what[-length(what)], collapse = ", "),
  #     tail(what, 1),
  #     sep = " and "
  #   )),
  #   by = "level"
  # ][
  #   j = sprintf(
  #     '- <u style="color: var(--main-color);">*%s:*</u> %s',
  #     capitalise(level),
  #     what
  #   )
  # ]
  

  text <- read_excel_sheet(xlsx, sheet)[
    j = sprintf(
      '- <u style="color: var(--main-color);">*%s:*</u> %s',
      what,
      level
    )
  ]

  sprintf("## Languages {#skills}\n\n%s\n\n", paste(text, collapse = "\n"))
}

