##' Using a simple switch statement, determine the file extension of the data
##' fiel and then read it using an appropriate function, returning a common
##' format (a data.frame).
##' @title Open any of several data files
##' @param rowOfFileInput.data.frame a row of the data.frame object which holds
##'   the information about an uploaded file obtained from a fileInput widget.
##' @returns a data.frame or tibble of the read data file
openDataFile <- function(rowOfFileInput.data.frame) {
  switch(
    tolower(tools::file_ext(datafile$name)),

    ## Textual data formats
    csv = read_csv(datafile$datapath, show_col_types = FALSE),
    txt = read_tsv(datafile$datapath, show_col_types = FALSE),

    ## Spreadsheet formats
    ods = read_ods(datafile$datapath),
    xls = read_xls(datafile$datapath),
    xlsx = read_xlsx(datafile$datapath),

    validate("Improper file format.") # ERROR
  )
}

mimetypes <- c(
  ## Spreadsheets
  "application/vnd.ms-excel",
  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  "application/vnd.oasis.opendocument.spreadsheet",

  ## Textual data formats; only the first two are widely used, and the last five
  ## forms of the CSV MIME type are deprecated and shouldn't be used for
  ## assignment, but should be supported for recognition.
  "text/plain",
  "text/csv",
  "application/csv",
  "application/x-csv",
  "text/comma-separated-values",
  "text/x-comma-separated-values",
  "text/comma-separated-values"
)
