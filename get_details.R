library(jsonlite)
get_details <- function(player_id){
  
  api_details <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/element-summary/",player_id,"/", sep = ""))
  
  point_details <- api_details$history |>
    tibble() |>
    select(element, round, total_points)
  
  return(point_details)
}




