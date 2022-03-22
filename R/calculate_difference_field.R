#' Calculate the difference between ACS variables
#'
#' @param con A database connection
#' @param variable_name Name of the variable to derive
#' @param lhs_cols Variable or variables to subtract from. Multiple values will be summed.
#' @param rhs_cols Variable or variables to subtract from lhs_cols. Multiple values will be summed.
#' @param geography Geographic level of data
#' @param keys Named list with the structure `list(geoid = X, year = 'year')`, where X is variable name for the GEOID of the geography
#' @param params List of params
#'
#' @return A data frame
#' @export
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom data.table dcast setDT
calculate_difference_field <- function(con,
                                       variable_name,
                                       lhs_cols,
                                       rhs_cols,
                                       geography,
                                       keys,
                                       params
){

  # cmd check
  lhs <- rhs <- moe <- acs_code <- `:=` <- .SD <- NULL
  stopifnot(is.list(keys))
  stopifnot(all(names(keys) %in% c('geoid', 'year')))
  stopifnot(all(c('geoid', 'year') %in% names(keys)))

  # query to pull the variables we need
  query <- cori.utils::glue_list("select * from {`tbl`} where {`params$variable_source_for_calculations`} in ({codes*})",
                           list(tbl = cori.utils::glue_list(params$table_format, list(geo = geography)),
                                codes = c(rhs_cols, lhs_cols)), con)

  # get data
  dta <- DBI::dbGetQuery(con, query)

  data.table::setDT(dta)

  # cast to tidy format
  wide_dta <- data.table::dcast(dta, glue_list("{geoid} + {year} ~ {var_col}",
                                               merge_list(keys, list(var_col = params$variable_source_for_calculations))),
                                value.var = params$value_column)

  # get vector of keys from list (easier than calling unlist multiple times)
  key_vec <- unlist(keys)

  # grouping by the keys, sum across all lhs columns
  wide_dta[, lhs := sum(.SD, na.rm = TRUE), key_vec, .SDcols = lhs_cols]
  # grouping by the keys, sum across all rhs columns
  wide_dta[, rhs := sum(.SD, na.rm = TRUE), key_vec, .SDcols = rhs_cols]
  # grouping by the keys, create a variable using the provided name and subtracting the lhs from the rhs
  wide_dta[, (variable_name) := lhs - rhs, key_vec]

  # select just the data to export
  out <- wide_dta[, c(key_vec, variable_name), with = FALSE]

  # normalize data to
  out_norm <- data.table::melt(out, id.vars = key_vec, value.name = params$value_column)
  out_norm[, moe := NA_real_]
  out_norm[, acs_code := params$calculated_field_code]
  # reorder columns to match input
  out_norm <- out_norm[, names(dta), with = FALSE]

  out_norm
}

