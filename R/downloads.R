# downloads.R — CSV and PDF download handlers

#' Create a CSV download handler
#'
#' @param data_reactive Reactive expression returning the dataframe
#' @param filename_prefix Prefix for the download filename
#' @return A downloadHandler
create_csv_download <- function(data_reactive, filename_prefix) {
  shiny::downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- data_reactive()
      shiny::req(df)
      write.csv(df, file, row.names = FALSE)
    }
  )
}

#' Create PDF download content function for a GT table
#'
#' @param gt_table_reactive Reactive expression returning a gt table
#' @param filename_prefix Prefix for the filename
#' @return A function suitable for downloadHandler's content argument
gt_download_pdf <- function(gt_table_reactive, filename_prefix) {
  function(file) {
    gt_table <- gt_table_reactive()
    shiny::req(gt_table)

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0(filename_prefix, "_", timestamp, ".pdf")

    gt::gtsave(
      data = gt_table,
      filename = filename,
      path = tempdir()
    )

    file.copy(file.path(tempdir(), filename), file)
  }
}
