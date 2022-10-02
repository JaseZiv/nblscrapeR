#' Parse player box score data for each game list
#'
#' This functions parses each game list and returns a 
#' data frame of player box score data
#' 
#' @param resp list element from scraped json data
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
each_player_box <- function(resp) {
  # print(paste0("match id ", resp[["match_id"]]))
  pl_home <- resp[["tm"]][["1"]][["pl"]]
  
  coerce_to_char <- function(x) {
    x[1][["sMinutes"]] <- as.character(x[1][["sMinutes"]])
    return(x)
  }
  
  if(rlang::is_null(pl_home) | rlang::is_empty(pl_home)) {
    player_box <- data.frame()
  } else {
    
    pl_home <- purrr::map(pl_home, coerce_to_char)
    pl_home_df <- dplyr::bind_rows(pl_home, .id = "pno")
    pl_home_df <- pl_home_df %>% dplyr::select(-tidyselect::any_of("comp")) %>% dplyr::distinct()
    pl_home_df$match_id <- resp[["match_id"]]
    pl_home_df$home_away <- 1
    # pl_home_df$minutes <- as.character(pl_home_df$minutes)
    
    pl_away <- resp[["tm"]][["2"]][["pl"]]
    pl_away <- purrr::map(pl_away, coerce_to_char)
    pl_away_df <- dplyr::bind_rows(pl_away, .id = "pno")
    pl_away_df <- pl_away_df %>% dplyr::select(-tidyselect::any_of("comp")) %>% dplyr::distinct()
    pl_away_df$match_id <- resp[["match_id"]]
    pl_away_df$home_away <- 2
    # pl_away_df$minutes <- as.character(pl_away_df$minutes)
    
    pl_df <- dplyr::bind_rows(pl_home_df, pl_away_df)
    
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
    
    player_box <- team_meta_df %>% 
      dplyr::left_join(pl_df, by = c("match_id", "home_away")) %>% 
      janitor::clean_names() %>% 
      dplyr::select(-tidyselect::matches("^eff_"))
    
    names(player_box) <- gsub("^s_", "", names(player_box))
    
    player_box <- player_box %>% 
      dplyr::select(tidyselect::any_of(c("match_id", "team_name", "team_short_name", "home_away", "opp_name", "opp_short_name", "first_name",
                      "family_name", "scoreboard_name", "playing_position", "starter", "shirt_number", "captain", "minutes", "points")),
             tidyselect::everything(),
             -tidyselect::contains("international"), -.data$pno, -tidyselect::contains("initial"))
    
  }
  
  return(player_box)
  
}
