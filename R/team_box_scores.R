#' Parse team box score data for each game list
#'
#' Internal function for parse_team_box
#' 
#' @param resp list element from scraped json data
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
.each_team_box <- function(resp) {
  # print(paste0("match id ", resp$match_id))
  rems <- c("logoT", "logoS", "pl", "shot", "lds", "scoring")
  
  
  home <- resp[["tm"]][[1]]
  if(length(home) == 0) {
    df <- data.frame()
  } else {
    home <- home[-which(names(home) %in% rems)]
    home <- data.frame(home) 
    home <- home %>% dplyr::select(-tidyselect::matches("logo|nameInternational|codeInternational|coach|asst|scoring", perl=TRUE))
    names(home) <- gsub("tot_s", "", names(home)) %>% janitor::make_clean_names()
    tryCatch( {home$minutes <- as.character(home$minutes)}, error = function(e) {home$minutes <- NA_character_})
    
    
    away <- resp[["tm"]][[2]]
    away <- away[-which(names(away) %in% rems)]
    away <- data.frame(away) 
    away <- away %>% dplyr::select(-tidyselect::matches("logo|nameInternational|codeInternational|coach|asst", perl=TRUE))
    names(away) <- gsub("tot_s", "", names(away)) %>% janitor::make_clean_names()
    tryCatch( {away$minutes <- as.character(away$minutes)}, error = function(e) {away$minutes <- NA_character_})
    
    home$home_away <- "home"
    home$opp_name <- away$name
    home$opp_short_name <- away$short_name
    home$opp_score <- away$score
    home$opp_full_score <- away$full_score
    home$match_id <- resp$match_id
    
    
    
    away$home_away <- "away"
    away$opp_name <- home$name
    away$opp_short_name <- home$short_name
    away$opp_score <- home$score
    away$opp_full_score <- home$full_score
    away$match_id <- resp$match_id
    
    df <- dplyr::bind_rows(home, away)
    # df <- df %>% 
    #   select(match_id, home_away, name, short_name, score, full_score, opp_name, opp_short_name, opp_score, opp_full_score, tidyr::everything())
    
    # df <- df %>% 
    #   select(tidyselect::matches("match_id|home_away|name|short_name|score|full_score|opp_name|opp_short_name|opp_score|opp_full_score"), tidyr::everything()) %>% 
    #   dplyr::select(match_id, tidyr::everything())
    
    df <- df %>% 
      dplyr::select(tidyselect::any_of(c("match_id", "home_away", "name", "short_name", "code", "score", "full_score", 
                                "opp_name", "opp_short_name", "opp_score", "opp_full_score")), 
             tidyselect::everything()) 
  }
  
  return(df)
}



#' Parse team box score data for game lists
#'
#' This functions parses all game list and returns a 
#' data frame of team box score data
#' 
#' @param resp list element(s) from scraped json data
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
parse_team_box <- function(resp) {
  
  results_wide <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_results/results_wide.rds")) %>% 
    dplyr::select(.data$match_id, .data$season)
  
  out <- resp %>% 
    purrr::map_df(.each_team_box)
  
  out <- out %>% dplyr::distinct() %>%
    dplyr::left_join(results_wide, by = "match_id") %>%
    dplyr::select(.data$match_id, .data$season, tidyselect::everything())
  
  return(out)
  
}
