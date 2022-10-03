#' Parse pbp data for each game list
#'
#' Internal function for parse_pbp
#' 
#' @param resp list element from scraped json data
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' 
.each_pbp <- function(resp) {
  
  # resp <- resp[[1]]
  
  pbp <- resp[["pbp"]]
  
  if(rlang::is_null(pbp) | rlang::is_empty(pbp)) {
    pbp <- data.frame()
  } else {
    
    team_meta <- resp[["tm"]]
    
    team_meta_df <- data.frame(match_id = resp[["match_id"]], 
                               team_name = team_meta[[1]][["name"]], 
                               team_short_name = team_meta[[1]][["shortName"]],
                               home_away = 1, 
                               opp_name = team_meta[[2]][["name"]], 
                               opp_short_name = team_meta[[2]][["shortName"]]) %>% 
      dplyr::bind_rows(
        data.frame(match_id = resp[["match_id"]], 
                   team_name = team_meta[[2]][["name"]], 
                   team_short_name = team_meta[[2]][["shortName"]],
                   home_away = 2, 
                   opp_name = team_meta[[1]][["name"]], 
                   opp_short_name = team_meta[[1]][["shortName"]])
      )
    
    
    pbp$match_id <- resp[["match_id"]]
    pbp <- janitor::clean_names(pbp)
    
    
    pbp <- pbp %>% 
      dplyr::left_join(team_meta_df, by = c("match_id", "tno"="home_away")) %>% 
      dplyr::mutate(home_away = dplyr::case_when(
        tno == 1 ~ "home",
        tno == 2 ~ "away",
        tno == 0 ~ NA_character_
      )) %>% 
      dplyr::select(tidyselect::any_of(c("match_id", "action_number", "team_name", "team_short_name", "home_away", "opp_name", "opp_short_name", 
                                         "period_type", "period", "gt", "s1", "s2", "lead", "pno", "first_name", "family_name", "scoreboard_name", 
                                         "shirt_number", "action_type", "sub_type", "success", "qualifier", "previous_action", "scoring")))
    
    if(any(grepl("qualifier", names(pbp)))) {
      pbp <- pbp %>% dplyr::mutate(qualifier = as.list(.data$qualifier))
    }
    
  }
  return(pbp)
  
}



#' Parse pbp data for game lists
#'
#' This functions parses all game list and returns a 
#' data frame of pbp data
#' 
#' @param resp list element(s) from scraped json data
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
parse_pbp <- function(resp) {
  
  results_wide <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_results/results_wide.rds")) %>% 
    dplyr::select(.data$match_id, .data$season)
  
  out <- resp %>% 
    purrr::map_df(.each_pbp)
  
  out <- out %>% dplyr::distinct() %>%
    dplyr::left_join(results_wide, by = "match_id") %>%
    dplyr::select(.data$match_id, .data$season, tidyselect::everything())
  
  return(out)
  
}

