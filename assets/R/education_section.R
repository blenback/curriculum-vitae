education_section <- function(xlsx = "data/cv.xlsx", sheet = "education", page_break_after = FALSE) {
  # text <- read_excel_sheet(xlsx, sheet)[
  #   i = .N:1,
  #   j = sprintf(
  #     "### %s\n\n%s\n\n%s\n\n%s - %s\n\n*%s*\n\n\n\n",
  #     degree, university, city, start, end, description
  #   )
  # ]
  
  #load sheet and convert to data.frame
  text <- as.data.frame(read_excel_sheet(xlsx,sheet))
  
  #sort the rows in reverse order
  text <- text[nrow(text):1,]
  
  #loop over rows and construct 'c' formated strings dependent on column entries
  
  #vector string that is consistently used for the first columns 
  base_str <- "### %s\n\n%s\n\n%s\n\n%s - %s\n\n"
  #               Degree  University  City  Start  End
  #loop over rows pasting string
  strings <- apply(text,1, function(x){
    
    #check for non-empty description column
    if(x["description"] != ""){
      base_str <- paste0(base_str, "Description: *%s*\n\n")
    }
    
    #check for non-empty awards column
    if(x["awards"] != ""){
     base_str <- paste0(base_str, "Awards: *%s*\n\n") 
    }
    
    #add new line at the end
    base_str <- paste0(base_str, "\n\n")
    
    #identify names of non-empty entries in x
    col_names <- names(x)[nzchar(x)]

    #browser()
    #create c string
    c_str <- do.call(sprintf, c(fmt = base_str, as.list(x[col_names])))
    
    #return the string
    return(c_str)
  })
  
  if (page_break_after) {
    c("## Education {data-icon=graduation-cap data-concise=true .break-after-me}", strings)
  } else {
    c("## Education {data-icon=graduation-cap data-concise=true}", strings)
  }
}
