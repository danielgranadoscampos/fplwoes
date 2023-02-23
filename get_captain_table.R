
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
  
  # get_top_entry
  top_entry <- get_league_entries(leagueid = 314) |>
    arrange(rank) |> head(1) |> pull(entry) # this is called in get_top_10 also
  
  # world_leader picks
  
  world_leader_captain <- get_entry_captain(top_entry, low_gw) |> 
    tibble() |> rename(world_leader_captain = playername)
  
  #Your captain
  your_captain <- get_entry_captain(entry_id, low_gw) |>
    tibble()|> rename(my_captain = playername)
  
  # Player list 
  
  captains_list <- c(your_captain$id, world_leader_captain$id) |>
    unique()
  
  #captain details
  
  source("get_details.R")
  all_details <- map_dfr(captains_list,get_details) |> 
    drop_na() |>
    filter(round %in% low_gw) |> 
    rename(id = element,
           event = round)
  
  # join captain tables with detais
  
  world_leader_details <- inner_join(all_details, world_leader_captain) |>
    select(-entry, -id) |>
    rename(world_leader_captain_points = total_points)
  
  your_captain_details <- inner_join(all_details, your_captain) |>
    select(-entry, -id) |>
    rename(my_captain_points = total_points)
  
  # captain comparison
  result <- inner_join(world_leader_details, your_captain_details, by = "event") |>
    mutate(captain_difference = my_captain_points - world_leader_captain_points) |>
    rename(GW = event)
  return(result)
  
}


