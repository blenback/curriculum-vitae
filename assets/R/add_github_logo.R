add_github_logo <- function(url, colour = "#333333") {
  sub(
    "[GitHub]",
    paste0("[", fontawesome::fa("github", fill = colour), " GitHub repository]"),
    sub(
      pattern = "(.*)https://github.com/(.*)",
      replacement = "\\1[GitHub](https://github.com/\\2)",
      x = url
    )
    ,
    fixed = TRUE
  )
}
