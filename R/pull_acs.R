#' Pull ACS data for a given geography, year, and set of variables
#'
#' @param geo Name of the geography to get ACS data for
#' @param acs_year Year to pull data for
#' @param variables Variables to pull
#'
#' @return A data.table
#' @export
#' @importFrom data.table setnames setDT
#' @importFrom tidycensus get_acs
pull_acs <- function(geo, acs_year, variables){
  # cmd check
  geography <- `:=` <- NULL

  dta <- tidycensus::get_acs(geography = geo, variables = variables, year = acs_year)

  data.table::setDT(dta)

  dta[, geography := (geo)]
  dta[, year := acs_year]
  data.table::setnames(dta, 'variable', 'acs_code')

  dta
}
