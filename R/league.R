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
  # match <- .safely_from_json(paste0('https://prod.services.nbl.com.au/api_cache/nbl/genius?route=competitions/', season_id, '/matches'))
  # match <- match$result %>% data.frame()
  
  headers <- c(
    `Accept` = "*/*",
    `Accept-Language` = "en-AU,en-GB;q=0.9,en-US;q=0.8,en;q=0.7",
    `Connection` = "keep-alive",
    `Origin` = "https://nbl.com.au",
    `Referer` = "https://nbl.com.au/",
    `Sec-Fetch-Dest` = "empty",
    `Sec-Fetch-Mode` = "cors",
    `Sec-Fetch-Site` = "same-site",
    `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36",
    `sec-ch-ua` = '"Google Chrome";v="119", "Chromium";v="119", "Not?A_Brand";v="24"',
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = '"macOS"',
    `x-quark-req-src` = "web-nbl-nbl"
  )
  
  match <- httr::GET(url = paste0("https://prod.services.nbl.com.au/api_cache/nbl/genius?route=competitions/", season_id, "/matches&fields=matchId%2Cduration%2CatNeutralVenue%2CextraPeriodsUsed%2Cattendance%2CmatchNumber%2CmatchName%2CleagueId%2CgameNote%2CgameDay%2CcompetitionId%2CroundNumber%2CmatchStatus%2CmatchTime%2CmatchTimeUTC%2CmatchType%2Cvenue%2Ccompetitors&filter%5BmatchStatus%5D=%21DRAFT&sortBy=matchTimeUTC&filter%5Btenant%5D=nbl"), httr::add_headers(.headers=headers)) |> 
    httr::content()
  
  match <- jsonlite::toJSON(match)
  match <- jsonlite::fromJSON(match)
  
  return(match)
}
