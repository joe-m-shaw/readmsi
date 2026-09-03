#' Join information in CLC MSI Report locus section
#'
#' @param df The output from mutate_clc_msi_row_numbers
#' @param row_distance_locus_to_table The number of rows between the locus name
#' and the table of length information for that locus
#' @param row_distance_stability_to_table The number of rows between the
#' stability prediction for a locus and the table of length information for that
#' locus.
#'
#' @importFrom rlang .data
#'
#' @returns A list of tables with the first item being the final joined table
#'
join_msi_locus_tables <- function(df,
                                  row_distance_locus_to_table = 2,
                                  row_distance_stability_to_table = 1) {

  df_locus_tables <- df |>
    dplyr::filter(!is.na(row_table_start) &
                    !is.na(x2)) |>
    dplyr::rename(length = x1,
                  percentage_sample = x2,
                  percentage_baseline = x3) |>
    dplyr::filter(!length %in% c("Length", "No matching reads at this locus")) |>
    dplyr::select(length, percentage_sample, percentage_baseline, row_table_start)

  df_locus_names <- df |>
    dplyr::filter(!is.na(row_locus)) |>
    dplyr::rename(locus = x1) |>
    dplyr::select(locus, row_locus) |>
    dplyr::mutate(row_table_for_join = row_locus + row_distance_locus_to_table)

  df_stability <- df |>
    dplyr::filter(!is.na(row_stability)) |>
    dplyr::rename(stability = x1) |>
    dplyr::select(stability, row_stability) |>
    dplyr::mutate(row_table_for_join = row_stability + row_distance_stability_to_table)

  df_data_join <- df_locus_tables |>
    dplyr::full_join(df_locus_names,
                     dplyr::join_by("row_table_start" == "row_table_for_join"),
                     relationship = "many-to-one") |>
    dplyr::full_join(df_stability,
                     dplyr::join_by("row_table_start" == "row_table_for_join"),
                     relationship = "many-to-one") |>
    dplyr::mutate(length = as.double(length),
                  percentage_sample = as.double(percentage_sample),
                  percentage_baseline = as.double(percentage_baseline)) |>
    dplyr::select(locus, stability, length, percentage_sample, percentage_baseline)

  return(df_data_join)

}

