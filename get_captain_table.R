
get_captain_table <- function(entry_id, season_history) {
  
  # Get the four lowest gameweeks
  lowest_season_scores <- season_history |>
    arrange(desc(points)) |>
    filter(points > 0)|>
    tail(5)
  
  # A table  with the gameweek and score
  gw_low <- lowest_season_scores |> select(id, points) |>
    rename(event = id)
  
  
  # Get the lowest scoring gameweeks as list
  low_gw <- lowest_season_scores$id |> sort()
  
  # Jesper picks
  
  jesper_captain <- get_entry_captain(2960212, low_gw) |> 
    tibble() |> rename(jesper_captain = playername)
  
  #Your captain
  your_captain <- get_entry_captain(entry_id, low_gw) |>
    tibble()|> rename(my_captain = playername)
  
  # Player list 
  
  captains_list <- c(your_captain$id, jesper_captain$id) |>
    unique()
  
  #captain details
  
  source("get_details.R")
  all_details <- map_dfr(captains_list,get_details) |> 
    drop_na() |>
    filter(round %in% low_gw) |> 
    rename(id = element,
           event = round)
  
  # join captain tables with detais
  
  jesper_details <- inner_join(all_details, jesper_captain) |>
    select(-entry, -id) |>
    rename(jesper_captain_points = total_points)
  
  your_captain_details <- inner_join(all_details, your_captain) |>
    select(-entry, -id) |>
    rename(my_captain_points = total_points)
  
  # captain comparison
  result <- inner_join(jesper_details, your_captain_details, by = "event") |>
    mutate(captain_difference = my_captain_points - jesper_captain_points) |>
    rename(GW = event)
  return(result)
  
}


# my_id = 965064
# s_history <- get_entry_season(entryid = my_id)|> 
#   rename(id = event)
# 
# 
# 
# captain_table <- get_captain_table(my_id, s_history)
