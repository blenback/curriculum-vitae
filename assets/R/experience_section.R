
experience_section <- function(xlsx = "data/cv.xlsx", sheet = "experience", page_break_after = FALSE) {
  
  #OPTION TO SPLIT ACTIVITIES INTO BULLET POINT LIST
  # #read text from xlsx
  # text <- read_excel_sheet(xlsx, sheet)
  # 
  # #Put the unicode character 	"\u2022" in front of each test$activities entry and 
  # text$activities <- sapply(text$activities, function(x) {
  #   
  #   open_bullet <- paste0("\u2022 ", x)
  # 
  #   #replace instances of '.' with the unicode character 	"\n\u2022" except the last instance
  #   open_bullet <- gsub("\\.", ". \n\n\u2022", open_bullet)
  #   
  #   #remove the last instance inthe string of "\n\u2022"
  #   open_bullet <- stringi::stri_replace_last(open_bullet,
  #                                             replacement = "\\.",
  #                                             regex = ". \n\n\u2022")
  #   })
  # 
  # #reformat output with sprintf
  # text <- text[
  #   i = .N:1,
  #   j = sprintf(
  #     "### %s\n\n%s\n\n%s\n\n%s - %s\n\nActivities: \n\n*%s*\n\n\n\n",
  #     position, institute, city, start, end, activities
  #   )
  # ]
  
  text <- read_excel_sheet(xlsx, sheet)[
    i = .N:1,
    j = sprintf(
      "### %s\n\n%s\n\n%s\n\n%s - %s\n\nActivities: *%s*\n\n\n\n",
      position, institute, city, start, end, activities
    )
  ]
  

  if (page_break_after) {
    c("## Professional Experience {data-icon=laptop .break-after-me}", text)
  } else {
    c("## Professional Experience {data-icon=laptop}", text)
  }
}
