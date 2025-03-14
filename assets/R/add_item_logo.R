
add_item_logo <- function(url, type, colour = "#333333") {
  
  
  
  if(type == "github") {
  string <- sub(
    "[GitHub]", #pattern
    paste0("[", fontawesome::fa("github", fill = colour), " GitHub repository]"), #replacement
    # x (string)
    sub(
      pattern = "(.*)https://github.com/(.*)",
      replacement = "\\1[GitHub](https://github.com/\\2)",
      x = url
    )
    ,
    fixed = TRUE
  )
  }
  
  if(type == "presentation") {
  string <-  sub(
      "[Presentation]", #pattern
      paste0("[", fontawesome::fa("file-powerpoint", fill = colour), " Slides]"), #replacement
    sub(
      pattern = "(.*)https://blenback.github.io/presentations/(.*)",
      replacement = "\\1[Presentation](https://blenback.github.io/presentations/\\2)",
      x = url
    )
    ,
    fixed = TRUE
  )
  }
  
  if(type == "website") {
  string <-  sub(
      "[Website]", #pattern
      paste0("[", fontawesome::fa("globe", fill = colour), " Website]"),
        sub(
        pattern = paste0("(.*)", url, "(.*)"),
        replacement = paste0("\\1[Website](", url, "\\2)"),
        x = url
    )
    ,
    fixed = TRUE
    )
  }
  
  return(string)
}
  