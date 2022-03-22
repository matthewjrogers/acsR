#' Process ACS codebook
#'
#' @param codebook_path Path to codebook CSV
#' @param calculated_field_code Code for calculated fields in the codebook
#' @param acs_years Years to pull data for
#' @param step One of 'source' or 'process', which determines whether source or derived fields are kept
#'
#' @return A data.table
#' @export
#'
#' @importFrom data.table fread
process_codebook <- function(codebook_path, calculated_field_code, acs_years, step = c('source', 'process')){
  # nse cmd check
  acs_code <- minimum_year <- N <- .N <- NULL

  acs_step <- match.arg(step, choices = c('source', 'process'))
  # get codebook
  codebook <- data.table::fread(codebook_path)

  if(acs_step == 'source'){
    codebook <- codebook[acs_code != calculated_field_code]

    stopifnot(codebook[, .N, acs_code][N > 1, .N] == 0)

  } else {
    codebook <- codebook[acs_code == calculated_field_code]
  }

  codebook[, minimum_year := ifelse(is.na(minimum_year), min(acs_years), minimum_year)]


  return(codebook)
}
