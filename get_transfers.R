# entryid <- 72802
# season_history <- get_entry_season(entryid = entryid) |>
#   rename(id = event)
library(jsonlite)
get_transfers <- function(entryid, season_history) {
  
  
  # Get the four lowest gameweeks
  lowest_season_scores <- season_history |>
    arrange(desc(points)) |>
    filter(points > 0)|>
    tail(4)
  
  # A table  with the gameweek and score
  gw_low <- lowest_season_scores |> select(id, points) |>
    rename(event = id)
  
  
  # Get the lowest scoring gameweeks as list
  low_gw <- lowest_season_scores$id |> sort()
  
  # API CALL
  transfer_history <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/",entryid,"/transfers","/",sep=""))
  
  # Look at transfers in lower gameweeks
  transfer_history <- transfer_history |> 
    tibble() |>
    filter(event %in% low_gw)  
  
  # List of all the player IDs that were transfered in the lowest gameweeks
  all_players <- c(transfer_history$element_in, transfer_history$element_out)
  
  
  # player details API CALL
  
  # api_details <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/element-summary/",player_id,"/", sep = ""))
  
  # Details of the players transfered, access total points in gameweek
  # player_details <- map_dfr(all_players,get_player_details, 
  #                           name = NULL, season = NULL ) |> 
  #   drop_na()
  
  source("get_details.R")
  all_details <- map_dfr(all_players,get_details) |> drop_na()
  
  
  
  # Access player names 
   player_ids <- unique(all_details$element)
   
   player_names <- map_dfr(player_ids, get_player_name) |> 
   rename(element = id)
   
   player_details <- inner_join(all_details, player_names)
  
  # Separate elements transferred in
  
  players_in <- transfer_history |>
    select(element_in, event, time) |>
    rename(element = element_in)
  
  # Separate elements transferred out
  players_out <- transfer_history |>
    select(element_out, event, time) |>
    rename(element = element_out)
  
  
  selected_details <- player_details |>
    #filter(fixture != 111) |>
    filter(round %in% low_gw) |>
    rename(event = round) |>
    select(event,element, playername, total_points )
  
  in_details <- inner_join(selected_details, players_in) |>
    select(-element) |>
    rename("Player_in" = playername,
           "Player_in_points" = total_points)
  
  
  out_details <- inner_join(selected_details, players_out) |>
    select(-element) |>
    rename("Player_out" = playername,
           "Player_out_points" = total_points)
  
  details_table <- inner_join(in_details, out_details)
  
  low_points_details <- inner_join(details_table, gw_low)
  
  final_table <- low_points_details |> 
    select(-time ) |>
    mutate("Points won" = Player_in_points - Player_out_points) |>
    rename(GW = event,
           GW_points = points) |> distinct(.keep_all = TRUE)

  
  
  return(final_table)
  
}

