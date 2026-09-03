#' Add row numbers to the locus table section of a CLC MSI Report
#'
#' @param df The dataframe output from read_msi_locus_section
#' @param regex_table The column name of the final column of each locus
#' length table
#' @param regex_locus Regular expression for identifying the locus name, which
#' is 2 rows above the corresponding length table
#' @param regex_stability Regular expression for identifying the cell containing
#' the stability prediction for a locus
#'
#' @importFrom rlang .data
#'
#' @returns The input dataframe with added columns showing the rownumber of
#' each cell
#'
mutate_msi_row_numbers <- function(df,
                                       regex_table = "Percentage: Baseline",
                                       regex_locus = ".*\\(\\w{1,2}\\)\\d{1,3}",
                                       regex_stability = "Stability") {

  if(!all(colnames(df) == c("x1", "x2", "x3"))){
    stop("Input has non-default column names")
  }

  df_row_numbers <- df |>
    dplyr::filter(!is.na(x1)) |>
    dplyr::mutate(
      row_table_start = dplyr::case_when(
        x3 == regex_table ~dplyr::row_number(),
        TRUE ~NA),
      row_locus = dplyr::case_when(
        grepl(pattern = regex_locus,
              x = x1) ~dplyr::row_number(),
        TRUE~NA),
      row_stability = dplyr::case_when(
        grepl(pattern = regex_stability,
              x = x1) ~dplyr::row_number(),
        TRUE~NA
      )) |>
    tidyr::fill(row_table_start, .direction = "down")

  return(df_row_numbers)

}
