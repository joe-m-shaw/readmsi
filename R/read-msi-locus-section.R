#' Read the section of a CLC MSI Report Excel which contains locus lengths
#'
#' @param filepath The filepath to the CLC MSI Report
#' @param locus_section_title The string which marks the the change from the
#' summary section of the report to the individual locus length tables.
#' @param ... Additional arguments to pass to read_excel if needed
#'
#' @returns The section of the Excel sheet containing the locus length tables
#' with every column formatted as text.
#'
read_msi_locus_section <- function(filepath,
                                   locus_section_title = "Loci Length Distributions",
                                   ...) {

  if(!is.character(locus_section_title)){
    stop("Non-character supplied to locus_section_title")
  }

  df <- readxl::read_excel(filepath,
                           col_names = FALSE,
                           col_types = "text",
                           .name_repair = "unique_quiet",
                           ...) |>
    janitor::clean_names()

  locus_tables_start <- match(locus_section_title, df$x1)

  if(is.na(locus_tables_start)){
    stop("No match for locus section title")
  }

  df_locus_tables <- df[locus_tables_start:nrow(df), 1:3]

  if(nrow(df_locus_tables) == 0) {
    warning("No rows returned")
  }

  return(df_locus_tables)

}
