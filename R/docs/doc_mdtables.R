#' This file contains functions for converting various R types to
#' pretty Markdown tables.
#'
#' mdtable_dataframe: Show a dataframe or tibble as markdown.
#' mdtable_list: Show a named list as markdown (names are the column names).
#' mdtable_csv_file: Show a CSV file as markdown.

library(stringr)
library(tibble)

format_table_row <- function(row) {
  # Format the row (a vector, list, etc) to markdown string: 1) Escape
  # pipe characters, 2) add optional word breaks to strings (<wbr>),
  # 3) concatenate strings in row with pipe separator.
  row <- row %>%
    sapply(function(x) str_replace_all(x, "\\|", "\\\\|")) %>%
    sapply(function(x) str_replace_all(x, "_", "_<wbr>")) %>%
    paste(collapse = " | ")
  row <- paste0("| ", row, " |")
  return(row)
}

#' Convert a dataframe or tibble to a markdown table.
#'
#' @param df The dataframe or tibble to convert to markdown.
#' @param rows The rows of df to show. Can be a single number, a vector of
#' numbers, or a sequence (eg. 1:3)
#'
#' @return Markdown string representing the dataframe/tibble.
mdtable_dataframe <- function(df, rows = NULL) {
  df <- as_tibble(df)

  if (!is.null(rows))
    df <- df[rows, ]

  table_header <- format_table_row(colnames(df))
  table_line <- paste0("|", strrep("-|", ncol(df)))

  table_rows <- df %>%
    apply(1, format_table_row) %>%
    paste(collapse = "\n")

  md <- paste(table_header, table_line, table_rows, sep = "\n")
  return(md)
}

#' Convert named list to a markdown table.
#' eg. mdtable_list(list(
#'  columnName1 = c(1, 2, 3),
#'  columnName2 = c("a", "b", "c")
#' ))
mdtable_list <- function(data, rows = NULL) {
  return(mdtable_dataframe(data.frame(data), rows = rows))
}

#' Convert a CSV file to a markdown table
mdtable_csv_file <- function(file, rows = NULL) {
  return(mdtable_dataframe(read.csv(file, fileEncoding="UTF-8-BOM"), rows = rows))
}
