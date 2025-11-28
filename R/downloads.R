#' Create a CSV Download Handler
#'
#' @param data_reactive A reactive expression returning the dataframe to download
#' @param filename_prefix String to prefix the filename (e.g., "Gender_Analysis")
#'
#' @return A downloadHandler function
create_csv_download <- function(data_reactive, filename_prefix) {
  downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Get data from reactive
      df <- data_reactive()
      req(df)
      write.csv(df, file, row.names = FALSE)
    }
  )
}

#' Create a download handler function for a GT table.
#'
#' This function returns a function suitable for use within Shiny's downloadHandler,
#' which generates the GT table and saves it as a PDF.
#'
#' @param gt_table_reactive A reactive expression that returns the gt table object.
#' @param filename_prefix A character string to prepend to the filename.
#'
#' @return A function to be used as the 'content' argument in downloadHandler.
gt_download_pdf <- function(gt_table_reactive, filename_prefix) {

  # The function returned here is the 'content' function for downloadHandler
  function(file) {

    # Ensure the required gt object is available
    gt_table <- gt_table_reactive()
    req(gt_table)

    # 1. Define the filename
    # Create a timestamped, descriptive filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0(filename_prefix, "_", timestamp, ".pdf")

    # 2. Save the GT table as a PDF
    # Note: gtsave requires webshot2 package to be installed for PDF output
    gt::gtsave(
      data = gt_table,
      filename = filename,
      path = tempdir() # Save to a temporary directory first
    )

    # 3. Copy the temporary file to the final download location
    file.copy(file.path(tempdir(), filename), file)
  }
}
