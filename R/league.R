#' Scrape all available NBL seasons
#'
#' This function gets all season years and IDs for the NBL
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
get_seasons <- function() {
  seasons <- jsonlite::fromJSON('https://apicdn.nbl.com.au/nbl/custom/api/genius?route=leagues/7/competitions&competitionName=NBL&fields=season,competitionId&limit=500&filter%5Btenant%5D=nbl')
  seasons <- seasons$data %>% data.frame()
  seasons <- seasons %>% dplyr::arrange(.data$competitionId)
  return(seasons)
}

#' Scrape matches for single season
#'
#' This function is a scraper for all match data for
#' a single season
#'
#' @param season_id can be the competitionId from in get_seasons()
#'
#' @export
get_matches <- function(season_id) {
  Sys.sleep(2)
  match <- .safely_from_json(paste0('https://prod.services.nbl.com.au/api_cache/nbl/genius?route=competitions/', season_id, '/matches'))
  match <- match$result %>% data.frame()
  return(match)
}
