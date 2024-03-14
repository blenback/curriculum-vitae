#devtools::install_github("ropensci/bib2df")
library(bib2df)

articles_section <- function(bib = "data/cv.bib", author = "Black", page_break_after = FALSE) {

  #read the bib tex file
  articles <- bib2df(bib)

  #add a date column by pasting together year and month and adding arbitrary day
  articles$DATE <- apply(articles,1, function(x){as.Date(paste0(x$YEAR, "-", x$MONTH, "-01"), "%Y-%b-%d")})
  
  #sort the rows according to descending DATE
  articles <- articles[rev(order(articles$DATE)),]
  
  # # Vector base C formatted string
  #base_str <- "### %s\n\n%s\n\nN/A\n\n %s\n\n::: aside\n\n*[%s](%s)*\n:::"
  #               title,author,       year,             journal,doi,

   base_str <- "### %s\n\n%s\n\nN/A\n\n%s\n\n *%s*\n\n::: aside\n\n%s\n\n:::"
  # #             title,author,place(NA),year, journal              oi 
  

  #loop over entries formatting as C string
  strings <- c(apply(articles,1, function(article){
  
  #vector required fields
  required_fields <- c("TITLE", "AUTHOR","YEAR","JOURNAL", "DOI")
  
  #check which of the required fields are columns in the article
  reqs <- sapply(required_fields, function(field){field %in% names(article)})
  
  #subset to those present
  required_fields <- required_fields[reqs]
  
  #check for non-na entries
  Non_NA_reqs <- sapply(required_fields, function(field){ if(any(!is.na(article[[field]]))){TRUE}else{FALSE}})
  
  #subset to only non-NA entries
  required_fields <- required_fields[Non_NA_reqs]
  
  #extract the entries from article
  fields <- as.list(article[required_fields])
  
  #convert the authors to a single string
  fields$AUTHOR <- paste(fields$AUTHOR, collapse = ", ")
  
  #within the string replace 'Black, B.' with "<u>Black, B.</u>"
  fields$AUTHOR <- gsub("Black, B.", "<u>Black, B.</u>", fields$AUTHOR)
  
  #add an icon to the DOI
  fields$DOI <- paste0("[", fontawesome::fa("scroll", fill = "#333333"), " Read](", fields$DOI, ")")
  
  #convert all entries to character
  fields <- lapply(fields, as.character)
  
  #check which of the custom fields DATA, CODE, PREPRINT or RESULTS are non-empty
  custom_fields <- c("DATA", "CODE", "PREPRINT", "RESULTS")
  
  #check which of the custom fields are columns in the article
  customs <- sapply(custom_fields, function(field){field %in% names(article)})
  
  #subset to only those that are columns
  custom_fields <- custom_fields[customs]
  
  #subset to non-NA entries
  Non_NA_customs <- sapply(custom_fields, function(field){ if(any(!is.na(article[[field]]))){TRUE}else{FALSE}})
  
  #subset to only non-NA entries
  custom_fields <- custom_fields[Non_NA_customs]
  
  #get the entries from the article
  custom_entries <- as.list(article[custom_fields])
  
  #modify the data entry to include an icon and string and link both to the url 
  if("DATA" %in% names(custom_entries)){
    custom_entries$DATA <- paste0(" [ ", fontawesome::fa("database", fill = "#333333"), " Data]", "(", custom_entries$DATA, ")")
  }
  
  #modify the CODE entry
  if("CODE" %in% names(custom_entries)){
    custom_entries$CODE <- paste0(" [ ", fontawesome::fa("code", fill = "#333333"), " Code]", "(", custom_entries$CODE, ")")
  }
  
  # if length of custom fields is 0 then create the basic string
  if(length(custom_fields) == 0){
    c_str <- do.call(sprintf, c(fmt = base_str, fields))
  } else if(length(custom_fields) != 0){
    
    #add spacing entries between each itme in custom entries
    for(i in 1:length(custom_entries)){
      custom_entries <- append(custom_entries, custom_entries[i], after = i)
      custom_entries[[i]] <- c(" ")
    }
    
      #modify the aide portion of the string to remove the closing elements. 
      #base_str <- gsub("aside\n\n*[%s](%s)*\n:::", "aside\n*[%s](%s)*\n\n", base_str, fixed = TRUE)

      #for version with journal name following authors
      base_str <- gsub("aside\n\n%s\n\n:::", "aside\n\n%s\n", base_str, fixed = TRUE)
      
      #add string for each entry in custom fields
      for(i in 1:length(custom_fields)){
        base_str <- paste0(base_str, "%3s%s\n")
      }

      #add the closing portion back to the string
      base_str <- paste0(base_str, "\n\n:::")
      
      #create c string
      c_str <- do.call(sprintf, c(fmt = base_str, append(fields, custom_entries)))
    }

  }, simplify = TRUE))
  

  #count number of articles
  articles_count <- length(strings)

  #prepare section header
  if (page_break_after) {
    return(c(sprintf("## Publications  {data-icon=newspaper .break-after-me}"), strings))
  } else {
    return(c(sprintf("## Publications  {data-icon=newspaper}"), strings))
  }
  
}
  
test <- articles_section()



# articles_section <- function(bib = "data/cv.bib", author = "Black", page_break_after = FALSE, only_first = FALSE) {
# 
#   #read the bib tex file
#   articles <- bib2df(bib)
#   
#   #sort the rows according to year
#   articles <- articles[]
# 
#   

#   # produce entries as C formatted strings
#   text <- data.table::setDT(articles)[
#     j = sprintf(
#       "### %s\n\n%s\n\nN/A\n\n%s %s\n\n::: aside\n\n*[%s](%s)*\n%s\n:::",
#       title, format_bib_author(authors, first, author), year, journal, doi, 
#       ifelse(
#         test = first,
#         yes = '<p style="font-size: 75%;"><sup>&dagger;</sup> As first or co-first author.</p>',
#         no = ""
#       )
#     )
#   ]
#   

#   #count number of articles
#   articles_count <- length(text)
# 
#   #prepare section header
#   if (only_first) {
#     text <- text[grepl("As first or co-first author", text)]
#     articles_count <- sprintf("%s + %s", length(text), articles_count - length(text))
#   }
# 
#   if (page_break_after) {
#     c(
#       sprintf("## Publications  {data-icon=newspaper .break-after-me}"),
#       text
#     )
#   } else {
#     c(
#       sprintf("## Publications  {data-icon=newspaper}"),
#       text
#     )
#   }
# }
# 
# 
# 
# 
# clean_field <- function(pattern, x) {
#   gsub(
#     pattern = sprintf("^%s = ", pattern),
#     replacement = "",
#     x = gsub(
#       pattern = ",$",
#       replacement = "",
#       x = gsub(
#         pattern = "[{}]",
#         replacement = "",
#         x = grep(sprintf("^%s", pattern), x, value = TRUE)
#       )
#     )
#   )
# }
# 
# read_article <- function(.x) {
#   authors <- do.call("rbind", strsplit(unlist(strsplit(clean_field("author", .x), " and ")), ", "))
#   authors <- apply(X = authors[, c(2, 1)], MARGIN = 1, FUN = function(irow) {
#     gsub(" ", "&nbsp;", paste(unique(irow), collapse = " "))
#   })
#   authors <- paste(paste(authors[-length(authors)], collapse = ", "), authors[length(authors)], sep = " and ")
#   
#   data.frame(
#     title = clean_field("title", .x),
#     year = clean_field("year", .x),
#     doi = clean_field("doi", .x),
#     authors = authors,
#     journal = clean_field("journal", .x),
#     first = if (any(grepl("annote", .x))) {
#       grepl("first", clean_field("annote", .x))
#     } else {
#       FALSE
#     },
#     stringsAsFactors = FALSE
#   )
# }
# 
# read_bib <- function(path) {
#   big_file <- paste(readLines(path), collapse = "")
#   big_file <- unlist(strsplit(x = big_file, split = "@", fixed = TRUE))
#   big_file <- big_file[nchar(big_file) != 0]
# 
#   all_bib <- lapply(strsplit(x = big_file, split = "(,\t)|(,  )"), read_article)
#   all_bib <- do.call("rbind.data.frame", all_bib)
#   all_bib[["doi"]] <- ifelse(
#     test = grepl("^http", all_bib[["doi"]]),
#     yes = all_bib[["doi"]],
#     no = paste0("https://www.doi.org/", all_bib[["doi"]])
#   )
# 
#   all_bib[order(all_bib[["year"]], decreasing = TRUE), ]
# }
# 
# format_bib_author <- function(authors, first, author, max = 10) {
#   mapply(
#     iauthors = authors,
#     ifirst = first,
#     FUN = function(iauthors, ifirst) {
#       split_authors <- unlist(strsplit(strsplit(iauthors, ", ")[[1]], " and "))
#       split_authors <- gsub(
#         pattern = author,
#         replacement = paste0("<u>", author, "</u>", if (ifirst) "<sup>&dagger;</sup>" else ""),
#         x = split_authors
#       )
#       pos_author <- grep(author, split_authors)
#       if (length(split_authors) <= max) {
#         paste(
#           paste(split_authors[-length(split_authors)], collapse = ", "),
#           split_authors[length(split_authors)],
#           sep = " and "
#         )
#       } else {
#         switch(
#           EXPR = paste(
#             abs(c(0, length(split_authors)) - pos_author) > ceiling(max / 2),
#             collapse = "--"
#           ),
#           "TRUE--TRUE" = {
#             if (pos_author > ceiling((max - 1) / 2)) {
#               split_authors[pos_author] <- paste0(
#                 split_authors[pos_author],
#                 "<sup>", pos_author, "/", length(split_authors), "</sup>"
#               )
#             }
#             paste0(
#               paste(
#                 c(
#                   split_authors[1:ceiling((max - 1) / 2)],
#                   "*[...]*",
#                   split_authors[pos_author],
#                   "*[...]*",
#                   split_authors[
#                     (length(split_authors) - (max - 1 - ceiling((max - 1) / 2))):(length(split_authors) - 1)
#                   ]
#                 ),
#                 collapse = ", "
#               ),
#               " and ",
#               split_authors[length(split_authors)]
#             )
#           },
#           "TRUE--FALSE" = {
#             if (pos_author > ceiling(max / 2)) {
#               split_authors[pos_author] <- paste0(
#                 split_authors[pos_author],
#                 "<sup>", pos_author, "/", length(split_authors), "</sup>"
#               )
#             }
#             paste0(
#               paste(
#                 c(
#                   split_authors[1:ceiling(max / 2)],
#                   "*[...]*",
#                   split_authors[
#                     (length(split_authors) - (max - 1 - ceiling(max / 2))):(length(split_authors) - 1)
#                   ]
#                 ),
#                 collapse = ", "
#               ),
#               " and ",
#               split_authors[length(split_authors)]
#             )
#           },
#           "FALSE--TRUE" = {
#             if (pos_author > ceiling(max / 2)) {
#               split_authors[pos_author] <- paste0(
#                 split_authors[pos_author],
#                 "<sup>", pos_author, "/", length(split_authors), "</sup>"
#               )
#             }
#             paste0(
#               paste(
#                 c(
#                   split_authors[1:ceiling(max / 2)],
#                   "*[...]*",
#                   split_authors[
#                     (length(split_authors) - (max - 1 - ceiling(max / 2))):(length(split_authors) - 1)
#                   ]
#                 ),
#                 collapse = ", "
#               ),
#               " and ",
#               split_authors[length(split_authors)]
#             )
#           },
#           "FALSE--FALSE" = {
#             paste(
#               paste(split_authors[-length(split_authors)], collapse = ", "),
#               split_authors[length(split_authors)],
#               sep = " and "
#             )
#           }
#         )
#       }
#     }
#   )
# }
# 
# 
# 
