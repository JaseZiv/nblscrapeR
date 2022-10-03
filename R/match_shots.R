#' Parse shooting location data for single game
#'
#' Internal function for parse_match_shot
#' 
#' @param resp list element from scraped json data
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
.parse_each_match_shot <- function(resp) {
  
  # print(paste0("parsing match ", resp[["match_id"]]))
  
  players <- parse_players(resp)
  
  shot_home <- resp[["tm"]][["1"]][["shot"]]
  
  if(rlang::is_null(shot_home) | rlang::is_empty(shot_home)) {
    shots <- data.frame()
  } else {
    shot_home$match_id <- resp[["match_id"]]
    shot_home$home_away <- 1
    
    
    shot_away <- resp[["tm"]][["2"]][["shot"]]
    shot_away$match_id <- resp[["match_id"]]
    shot_away$home_away <- 2
    
    shots <- dplyr::bind_rows(shot_home, shot_away)
    
    shots <- shots %>% 
      dplyr::select(-tidyselect::any_of(c("player", "shirtNumber"))) %>% 
      dplyr::left_join(players %>% dplyr::mutate(pno = as.numeric(.data$pno)), by = c("match_id", "home_away", "p"="pno"))
    
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
    
    shots <- team_meta_df %>% 
      dplyr::left_join(shots, by = c("match_id", "home_away")) %>% 
      janitor::clean_names() %>% 
      dplyr::mutate(home_away = dplyr::case_when(
        .data$home_away == 1 ~ "home",
        .data$home_away == 2 ~ "away",
        .data$home_away == 0 ~ NA_character_
      )) %>% 
      dplyr::select(-tidyselect::any_of(c("p", "pno", "tno"))) 
    
    names(shots) <- gsub("per$", "period", names(shots))
    names(shots) <- gsub("per_type$", "period_type", names(shots))
    
    
  }
  
  return(shots)
}



#' Parse shooting location data for game lists
#'
#' This functions parses all game lists and returns a 
#' data frame of player shooting coordinates and types
#' 
#' @param resp list element(s) from scraped json data
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
parse_match_shot <- function(resp) {
  
  results_wide <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_results/results_wide.rds")) %>% 
    dplyr::select(.data$match_id, .data$season)
  
  out <- resp %>% 
    purrr::map_df(.parse_each_match_shot)
  
  out <- out %>% dplyr::distinct() %>%
    dplyr::left_join(results_wide, by = "match_id") %>%
    dplyr::select(.data$match_id, .data$season, tidyselect::everything())
  
  return(out)
  
}
