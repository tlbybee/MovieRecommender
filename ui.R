## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(Matrix)

source('functions/helpers.R')

genres = readRDS("data/genres.rds")

shinyUI(dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Movie Recommender"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Genre Recommendations", tabName = "genre"),
    menuItem("Item-Based Recommendations", tabName = "ibcf")
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "genre",
            h2("Genre Recommendations"),
            fluidRow(
              box(
                width = 12,
                title = "Select a genre for recommendations",
                status = "info",
                solidHeader = TRUE,
                collapsible = FALSE,
                selectInput(inputId = "genreInputId",
                            label = h3("Select a genre below:"),
                            choices=c("",genres),
                            selected = "selected_genre"
                            
                            )
                # div(class = "dropdown",
                #     uiOutput('genre'))
              )),
            fluidRow(
              useShinyjs(),
              box(
                width = 12,
                status = "info",
                solidHeader = TRUE,
                title = "Recommendations:",
                br(),
                # withBusyIndicatorUI(
                #   actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                # ),
                br(),
                tableOutput("genre_recs")
              )
            )),
    
    tabItem(
      tabName = "ibcf",
      h2("Item-Based Recommendations"),
      #   )
      # ),
      includeCSS("css/movies.css"),
      fluidRow(
        box(
          width = 12,
          title = "Step 1: Rate at least 4 movies, but ideally as many movies as possible",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          div(class = "rateitems",
              uiOutput('ratings'))
        )
      ),
      fluidRow(
        useShinyjs(),
        box(
          width = 12,
          status = "info",
          solidHeader = TRUE,
          title = "Step 2: Discover movies you might like",
          br(),
          withBusyIndicatorUI(
            actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
          ),
          br(),
          tableOutput("results")
        )
      )
    )
  ))
)) 