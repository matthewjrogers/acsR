#' Separate full raw data by geography
#'
#' @param source_data Full source data
#' @param codebook ACS codebook
#' @param geo Geography name
#' @param geoid_name Name of the GEOID field
#'
#' @return A list of data frames
#' @export
#'
make_geo_level_data <- function(source_data, codebook, geo, geoid_name){

  # nse note handling
  geography <- acs_code <- label <- variable <- NAME <- estimate <- i.label <- NULL

  subset <- source_data[geography == geo]

  setnames(subset, 'GEOID', geoid_name)

  subset[, NAME := NULL]

  subset[codebook[, .(acs_code, label)], variable := i.label, on = 'acs_code']

  subset[!is.na(estimate), c(geoid_name, "acs_code", 'variable', 'year', "estimate", "moe"), with = FALSE]
}
