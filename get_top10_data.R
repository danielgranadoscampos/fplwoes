#library(fplscrapR)
get_top10_data <- function (){
  
  top_entries <- get_league_entries(leagueid = 314) |>
    arrange((rank)) |> 
    head(10)
  
  top_10_ids <- top_entries$entry
  
  top_season_info <- map_dfr(top_10_ids, get_entry_season) |> 
    tibble() #|> select(total_points, event) |>  tibble()
  
  average_top10_scores <- top_season_info |>
    group_by(event) |>
    summarise(total_points = mean(total_points)) |> #name changed
    mutate(ind = "avg_top_10") 
  
  return(average_top10_scores)
}



