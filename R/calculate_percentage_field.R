#' Calculate a percentage from ACS fields
#'
#' @param con A database connection
#' @param variable_name Name of the variable to derive
#' @param numerator_cols Variable or variables to divide. Multiple values will be summed.
#' @param denominator_cols Variable or variables to divide by. Multiple values will be summed.
#' @param inverse_pct If TRUE, result will be 1 - (sum(numerator_cols)/sum(denominator_cols))
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
#' @importFrom cori.utils merge_list glue_list
calculate_percentage_field <- function(con,
                                       variable_name,
                                       numerator_cols,
                                       denominator_cols,
                                       inverse_pct,
                                       geography,
                                       keys,
                                       params
){

  # cmd check
  numerator <- denominator <- moe <- acs_code <- comparison_year <- base_year <- year <- NULL

  stopifnot(is.list(keys))
  stopifnot(all(names(keys) %in% c('geoid', 'year')))
  stopifnot(all(c('geoid', 'year') %in% names(keys)))

  # query to pull the variables we need
  query <- cori.utils::glue_list("select * from {`tbl`} where {`params$variable_source_for_calculations`} in ({codes*})",
                     list(tbl = cori.utils::glue_list(params$table_format, list(geo = geography)),
                          codes = c(unlist(denominator_cols), unlist(numerator_cols))), con)

  # get data
  dta <- DBI::dbGetQuery(con, query)

  setDT(dta)

  # cast to tidy format
  wide_dta <- data.table::dcast(dta, cori.utils::glue_list("{geoid} + {year} ~ {var_col}",
                                               cori.utils::merge_list(keys, list(var_col = params$variable_source_for_calculations))),
                    value.var = params$value_column)

  # get vector of keys from list (easier than calling unlist multiple times)
  key_vec <- unlist(keys)

  # grouping by the keys, sum across all numerator columns
  wide_dta[, numerator := sum(.SD, na.rm = TRUE), key_vec, .SDcols = unlist(numerator_cols)]
  # grouping by the keys, sum across all denominator columns
  wide_dta[, denominator := sum(.SD, na.rm = TRUE), key_vec, .SDcols = unlist(denominator_cols)]
  # grouping by the keys, create a variable using the provided name and dividing the numerator by the denominator
  wide_dta[, (variable_name) := numerator/denominator, key_vec]

  # if inverse == TRUE, calculate 1 - pct
  # note that you need the double ampersand here, otherwise will error with 0 length argument error
  if (!is.null(inverse_pct) && inverse_pct){
    wide_dta[, (variable_name) := 1 - get(variable_name)]
  }

  # select just the data to export
  out <- wide_dta[, c(key_vec, variable_name), with = FALSE]

  # normalize data to
  out_norm <- data.table::melt(out, id.vars = key_vec, value.name = params$value_column)
  out_norm[, moe := NA_real_]
  out_norm[, acs_code := params$calculated_field_code]
  out_norm <- out_norm[, names(dta), with = FALSE]

  out_norm
}

