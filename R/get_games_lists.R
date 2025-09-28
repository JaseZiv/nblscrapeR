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


#' Scrape each game json for season 25-26
#'
#' This functions scrapes each json file for each match number and returns
#' a data frame of required data, including nested data frames
#'
#' @param match_id match id which can only be found in the html elements
#' 
#' @export
get_each_match <- function(match_id) {
   
  Sys.sleep(2)
  # each_game <- jsonlite::fromJSON(paste0("https://prod.rosetta.nbl.com.au/get/match/", match_id))
  headers = c(
    accept = "*/*",
    `accept-language` = "en-AU,en-GB;q=0.9,en-US;q=0.8,en;q=0.7",
    origin = "https://nbl.com.au",
    priority = "u=1, i",
    referer = "https://nbl.com.au/",
    `sec-ch-ua` = '"Chromium";v="140", "Not=A?Brand";v="24", "Google Chrome";v="140"',
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = '"macOS"',
    `sec-fetch-dest` = "empty",
    `sec-fetch-mode` = "cors",
    `sec-fetch-site` = "same-site",
    `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36"
  )
  
  each_game <- httr::GET(url = paste0("https://prod.rosetta.nbl.com.au/get/match/", match_id, "/live/all"), 
                         httr::add_headers(.headers=headers)) |> 
    httr::content(as = "text")
  
  
  each_game <- each_game |> jsonlite::fromJSON()
  # each_game <- each_game$data 
  each_game <- each_game$data
  
  return(each_game)
  
}
