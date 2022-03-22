#' Create a data.frame map of year and geography
#'
#' @param years Vector of years
#' @param geographies Vector of geographies
#'
#' @return A data.frame with all combinations of years and geographies
#' @export
#'
make_year_geo_map <- function(years, geographies){
  expand.grid(year = years, geo = gsub("_", " ", names(geographies)), stringsAsFactors = FALSE)
}
