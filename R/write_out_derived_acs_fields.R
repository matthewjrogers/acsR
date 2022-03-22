#' Write derived data to the database
#'
#' @param derived_fields A data frame of derived ACS fields returned from an `acsr` function
#' @param con A database connection
#' @param table_name The table name to write to
#' @param geoid Variable name of the geoid field in the table
#'
#' @export
#'
#' @importFrom DBI dbWriteTable
#' @importFrom glue glue_sql
#' @importFrom cori.db execute_on_postgres
write_out_derived_acs_fields <- function(derived_fields,
                                         con,
                                         table_name,
                                         geoid
){

  DBI::dbWriteTable(con, "TEMP_update_table", derived_fields, overwrite = TRUE)

  # prevent writing duplicate data to the table
  cleanup_query <- glue::glue_sql('delete from {`table_name`} f
                          using "TEMP_update_table" t where f.{`geoid`} = t.{`geoid`}
                          and f.variable = t.variable and f.year = t.year
                          ', .con = con)

  cori.db::execute_on_postgres(con, cleanup_query)
  cori.db::execute_on_postgres(con, 'drop table if exists "TEMP_update_table"')

  DBI::dbWriteTable(con, table_name, derived_fields, append = TRUE)
}
