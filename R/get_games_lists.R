#' Scrape each game json
#'
#' This functions scrapes each json file for each match number and returns
#' a list of data points
#'
#' @param match_id match id can be found in each game url on nbl.com.au
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
get_games <- function(match_id) {
  # print(paste0("getting match ", match_id))
  
  tryCatch({
  res <- httr::RETRY("GET", paste0("https://fibalivestats.dcd.shared.geniussports.com/data/", match_id, "/data.json"))
  
  # Check the result
  # check_status(res)
  
  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  
  resp <- jsonlite::fromJSON(resp)
  resp$match_id <- match_id
  return(resp)
  }, error=function(e){})
  
}