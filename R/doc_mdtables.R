#' This file contains functions for converting various R types to
#' pretty Markdown tables.
#'
#' mdtable_dataframe: Show a dataframe or tibble as markdown.
#' mdtable_list: Show a named list as markdown (names are the column names).
#' mdtable_csv_file: Show a CSV file as markdown.

library(stringr)
library(tibble)
library(knitr)

#' Convert a dataframe or tibble to a markdown table.
#'
#' @param df The dataframe or tibble to convert to markdown.
#' @param rows The rows of df to show. Can be a single number, a vector of
#' numbers, or a sequence (eg. 1:3). NULL to show all rows.
#'
#' @return Markdown string representing the dataframe/tibble.
mdtable_dataframe <- function(df, rows = NULL) {
  df <- tibble::as_tibble(df)

  if (!is.null(rows))
    df <- df[rows, ]

  # Add optional word breaks after underscores, allowing headers to wrap
  # and take up less room horizontally. Effectively, if a table gets too
  # wide this will result in the addition of a horizontal scrollbar instead
  # of trying to squish all the columns. The wide tables become harder to
  # read for the user.
  colnames(df) <- colnames(df) %>%
    sapply(function(x) stringr::str_replace_all(x, "_", "_<wbr>"))

  dt <- knitr::kable(df, escape = FALSE)

  return(dt)
}

#' Convert named list to a markdown table.
mdtable_list <- function(data, rows = NULL) {
  return(mdtable_dataframe(data.frame(data), rows = rows))
}

#' Convert a CSV file to a markdown table
mdtable_csv_file <- function(file, rows = NULL) {
  return(mdtable_dataframe(suppressWarnings(read.csv(file, fileEncoding="UTF-8-BOM")), rows = rows))
}