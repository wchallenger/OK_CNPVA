#' Blank Worksheet
#' 
#' Creates either a blank worksheet if one does not exists or clears out
#' the worksheet if it already is present in the Openxlsx workbook.
#'
#' @param wb openxlsx workbook object
#' @param sheet name or index of worksheet to create or clear.
#'
#' @return
#' updates the openxlsx workbook object.
#' 
#' @examples
#' 
#' \dontrun{
#' # Add a new worksheet to a new work book
#' wb <- createWorkbook()
#' BlankWorksheet(wb, "TEST")
#' 
#' # Clear out an existing worksheet
#' wb <- read.xlsx("path/to/workbook")
#' BlankWorksheet(wb, "ExistingSheetName")
#' }
#' 
#' @export
BlankWorkSheet <- function(wb, sheet, skipEmptyRows = TRUE) {
  if ((sheet %in% sheets(wb)) == FALSE) {
    addWorksheet(wb, sheet)
  } else {
    # Clear out any existing columns before writing.  we will selectively
    # leave in any columns outside of dat to ensure tests or notes are retained.
    olddat <- read.xlsx(wb, sheet, skipEmptyRows = skipEmptyRows) 
    if (!is.null(olddat)) {
      if (nrow(olddat) > 0) {
        deleteData(wb, sheet, cols = 1:ncol(olddat), rows=1:nrow(olddat), gridExpand = TRUE)
      }
    }
  }
}
