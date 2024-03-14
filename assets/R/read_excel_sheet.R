read_excel_sheet <- function(xlsx, sheet) {
  
  # Read xlsx file and sheet
  data.table::setDT(readxl::read_xlsx(xlsx, sheet))[
    
    # Replace NA with empty string by looping over columns
    j = lapply(
      X = .SD,
      FUN = function(x) {
        x[is.na(x)] <- ""
        x
      }
    )
  ]
}
