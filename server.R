library(shiny)
library(shinydashboard)
library(DT)
library(fplscrapR)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(jsonlite)

# server
server <- function(input, output) {
  
  
  # Basic gameweek info  
  gw_info <- get_round_info() 
  
  overall_average <- sum(gw_info$average_entry_score)
  
  # Overall average box 
  output$overall_average <- renderInfoBox({
    infoBox(
      "Average team score", overall_average, icon = icon("bar-chart"),
      color = "purple"
    )})
  
  # Entry id
  entry_id <- reactive({
    if (input$team_id != ""){
      entry_id <- input$team_id
    } else {
      entry_id <- 72802
    }
    
    return(entry_id)
  })
  
  season_history <- reactive({
    season_history <- get_entry_season(entryid = entry_id())|> 
      rename(id = event)
    return(season_history)
  })
  
  
  # Total points calc
  total_points <- reactive({
    total_points <- sum(season_history()$points)
    return(total_points)
  })
  
  # Total points box
  output$your_points <- renderInfoBox({
    infoBox("Your score", total_points(), icon = icon("futbol"), color = "purple")
  })
  
  # Total bench points calc
  bench_points <- reactive({
    bench_points <- sum(season_history()$points_on_bench)
    return(bench_points)
  })
  
  # Total points box
  output$bench_points <- renderInfoBox({
    infoBox("Total points on bench", bench_points(), 
            icon = icon("chair"), color = "purple")
  })
  
  # Get captains info
  
  source("get_captain_table.R")
  
  captain_table <- reactive({
    get_captain_table(entry_id(), season_history())
  })
  
  # Captains table
  
  output$captain_comparison <- DT::renderDataTable(tibble(captain_table()),
                                                   options = list(scrollX = TRUE,
                                                                  autoWidth = TRUE,
                                                                  paging = FALSE))
  
  # Get transfers tibble
  
  source("get_transfers.R")
  
  transfers_table <- reactive({
    transfers_table <- get_transfers(entry_id(), season_history())
    return(transfers_table)
  })
  
  # Transfers table
  output$gw_table <- DT::renderDataTable(tibble(transfers_table()), 
                                         options = list(scrollX = TRUE,
                                                        autoWidth = TRUE,
                                                        paging = FALSE))
  
  
  plot_data <- reactive({
    
    field_points_data <- season_history() |>
      select(id, points) |>
      mutate(place = "field")
    
    bench_points_data <- season_history() |>
      select(id, points_on_bench) |>
      rename(points = points_on_bench) |>
      mutate(place = "bench")
    
    plot_data <- bind_rows(field_points_data, bench_points_data)
    
    return(plot_data)
    
  })
  #Plot points
  
  output$points_plot <- renderPlot({
    ggplot(plot_data(), aes(fill=place, y=points, x= as.factor(id))) + 
      geom_bar(position="dodge", stat="identity") + 
      theme_minimal()+
      theme(panel.background = element_rect(fill = '#ECF0F5', color = '#ECF0F5'),
            plot.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5")) +
      xlab("Gameweek")
  })
  
  #Average team data
  
 average_team_score <- gw_info |> 
    drop_na() |> 
    tibble() |> 
    mutate(total_points = cumsum(average_entry_score),
           ind = "average_team") |>
    select(id, total_points, ind) |>
    rename(event = id)
 
 # Average top 10 managers data
 
 source("get_top10_data.R")
 
 average_top_points <- get_top10_data()
 
 # My_team_data
 
 my_team_scores <- reactive({
   
   my_team_scores <- season_history() |>
     rename(event = id) |>
     select(event, total_points) |>
     mutate(ind = "my_score") |>
     select(event, total_points, ind)
   
   return(my_team_scores)
   
 }) 
 
 # Top 10 comparison data
 
 top_10_plot_data <- reactive({
   top_10_plot_data <- bind_rows(average_team_score,
             average_top_points,
             my_team_scores())
   return(top_10_plot_data)
   
 })
 # Top 10 comparison plot
 
 output$top10_plot <- renderPlot({
   ggplot(top_10_plot_data(), aes(x = as.factor(event), y = total_points, 
                                  group = ind , color = ind)) +
     geom_line(size = 2) + 
     scale_y_continuous(n.breaks = 12) +
     theme_minimal()+
     theme(panel.background = element_rect(fill = '#ECF0F5', color = '#ECF0F5'),
           plot.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5")) +
     xlab("Gameweek")
 })
  
  
  
}
