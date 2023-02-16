library(shiny)
library(shinydashboard)
library(DT)
library(fplscrapR)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(jsonlite)

header <- dashboardHeader(title = "FPL woes")

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(# infoBoxes with fill=FALSE
  tags$h2("A place to revisit blunders, follies, kneejerks and regrets in FPL"),
  tags$h3("Basic stats"),
  fluidRow(
    textInput("team_id", "Enter your team ID here", placeholder = "72802"),
    infoBoxOutput("overall_average"),
    infoBoxOutput("your_points"),
    infoBoxOutput("bench_points")
  ),
  tags$h3("Captain Faceoff: Your Pick vs. Current Leader's in your Low-Scoring Weeks"),
  fluidRow(
    DT::dataTableOutput("captain_comparison")
  ),
  tags$h3("Low-Scoring Gameweek Struggles: Did Your New Player Outperform the Old?"),
  fluidRow(
    DT::dataTableOutput("gw_table")
  ),
  tags$h3("Points on the Side: Bench Performance vs. Starting Lineup Results"),
  fluidRow(
    plotOutput("points_plot")
  ),
  tags$div(
    tags$p("If you liked this analysis consider..."), tags$a(href = "https://www.buymeacoffee.com/danielu", "buying me a coffee")
  )
  
)


dashboardPage(header, 
              sidebar,
              body, 
              skin = "black")
