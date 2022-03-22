#' Calculate change in a variable from one year to another
#'
#' @param con A database connection
#' @param variable_name Name of the variable to be derived
#' @param variable The ACS variable to calculate change in
#' @param derived_status If TRUE, use MDA derived variable to calculate change
#' @param from_year Base year for comparison
#' @param to_year End year to calculate change against
#' @param geography Geographic level of data
#' @param keys Named list with the structure `list(geoid = X, year = 'year')`, where X is variable name for the GEOID of the geography
#' @param params List of params
#'
#' @return A data frame
#' @export
#'
#' @importFrom data.table melt dcast setnames setDT
#' @importFrom DBI dbGetQuery
#' @importFrom cori.utils glue_list merge_list
calculate_percent_change_field <- function(con,
                                           variable_name,
                                           variable,
                                           derived_status,
                                           from_year,
                                           to_year,
                                           geography,
                                           keys,
                                           params){

  stopifnot(is.list(keys))
  stopifnot(all(names(keys) %in% c('geoid', 'year')))
  stopifnot(all(c('geoid', 'year') %in% names(keys)))

  # check that to and from are at least 5 years apart
  if(abs(to_year - from_year) < 5){
    stop("Comparing overlapping ACS estimates will yield unreliable results. The base and comparison year should be at least 5 years apart.")
  }

  variable_column <- ifelse(is.null(derived_status) || isFALSE(derived_status), "acs_code", "variable")

  table_name <- cori.utils::glue_list(params$table_format, list(geo = geography))

  # read data for original using variable name and from

  query <- glue::glue_sql("select * from {`table_name`} where {`variable_column`} = {variable} and year = {from_year}", .con = con)

  base_data <- DBI::dbGetQuery(con, query)

  if (nrow(base_data) == 0){
    message(sprintf("No data was found for the variable '%s' in the year %s. Confirm that the variable is available in the years requested.", variable, from_year))
    return(NULL)
  }

  data.table::setDT(base_data)

  # transform to wide

  base_data_wide <- data.table::dcast(base_data, glue_list("{geoid} + {year} ~ {var_col}",
                                                           cori.utils::merge_list(keys, list(var_col = variable_column))),
                                      value.var = params$value_column)

  # set name to base_year

  data.table::setnames(base_data_wide, variable, "base_year")

  # read data for comparison

  query <- glue::glue_sql("select * from {`table_name`} where {`variable_column`} = {variable} and year = {to_year}", .con = con)

  comparison_data <- DBI::dbGetQuery(con, query)

  if (nrow(base_data) == 0){
    message(sprintf("No data was found for the variable '%s' in the year %s. Confirm that the variable is available in the years requested.", variable, to_year))
    return(NULL)
  }

  data.table::setDT(comparison_data)

  # transform to wide

  comparison_data_wide <- data.table::dcast(comparison_data, cori.utils::glue_list("{geoid} + {year} ~ {var_col}",
                                                                                   cori.utils::merge_list(keys, list(var_col = variable_column))),
                                            value.var = params$value_column)

  # set name to comparison_year
  data.table::setnames(comparison_data_wide, variable, "comparison_year")
  # browser()
  # join the data
  all_data <- merge(base_data_wide[, c(keys$geoid, 'base_year'), with = FALSE], comparison_data_wide[, c(keys$geoid, 'comparison_year'), with = FALSE], by = keys$geoid)

  # calculate the percent change
  all_data[, (variable_name) := (comparison_year / base_year) - 1]

  # normalize the data
  all_data_norm <- data.table::melt(all_data[, c(keys$geoid, variable_name), with = FALSE], id.vars = keys$geoid, value.name = params$value_column)

  # add variables
  all_data_norm[, year := to_year]
  all_data_norm[, moe := NA_real_]
  all_data_norm[, acs_code := params$calculated_field_code]
  all_data_norm <- all_data_norm[, names(base_data), with = FALSE]

  all_data_norm

}
