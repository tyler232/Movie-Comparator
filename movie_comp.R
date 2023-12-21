library(shiny)
library(shinydashboard)
library(tidyverse)
library(httr)
library(jsonlite)
library(dotenv)

dotenv::load_dot_env()
omdb_api_key <- Sys.getenv("OMDB_API_KEY")

# Define UI 
ui <- dashboardPage(
  dashboardHeader(title = "Movie Comparator"),
  dashboardSidebar(
    tags$div(class = "div0",
             style = "font-weight: bold;",
             HTML("&nbsp;&nbsp;Enter Movie Names")),
    textInput("search1", "Movie1: ", "Avengers: Endgame"),
    textInput("search2", "Movie2: ", "Iron Man 2"),
    tags$br(),
    tags$div(class = "div1",
             style = "font-weight: bold;",
             HTML("&nbsp;&nbsp;Enter years if can't find movie")),
    tags$div(class = "div2",
             style = "font-style: italic;",
             HTML("&nbsp;&nbsp; * Optional")),
    tags$div(class = "div3",
             style = "font-style: italic;",
             HTML("&nbsp;&nbsp; * If movie is found don't enter years")),
    tags$div(class = "div4",
             style = "font-style: italic;",
             HTML("&nbsp;&nbsp; * If don't know movie release year leave it blank")),
    tags$div(class = "div5",
             style = "font-style: italic;",
             HTML("&nbsp;&nbsp; * Make sure to enter digit only, such as 2016")),
    textInput("year_range_1", "Movie1 Year: "),
    textInput("year_range_2", "Movie2 Year: "),
    actionButton("submit_btn", "Compare")
  ),
  dashboardBody(
    fluidRow(
      column(tableOutput("rating_table"), width = 8)
    ),
    fluidRow(
      box(plotOutput("rating_plot"), width = 8)
    ),
    fluidRow(
      box(plotOutput("voting_plot"), width = 8)
    )
  ),
  skin = "black"
)

# Define server
server <- function(input, output) {
  # Placeholder Dataframes avoiding error
  df_placeholder_NULL = data.frame(Title = "Movie does not exist", 
                                   Year = NA,
                                   Ratings.Source = NA, 
                                   Ratings.Value = NA,
                                   imdbVotes = NA, 
                                   imdbRating = NA,
                                   Metascore = NA)
  df_placeholder_NoRating = data.frame(Title = "Movie has no rating", 
                                       Year = NA,
                                       Ratings.Source = NA, 
                                       Ratings.Value = NA,
                                       imdbVotes = NA, 
                                       imdbRating = NA,
                                       Metascore = NA)
  
  # create Dataframe from API
  create_df <- function() {
    # Retreive Dataframe from API
    if (is.null(input$year_range_1)) {
      url1 <- paste0("http://www.omdbapi.com/?apikey=", omdb_api_key, "&type=movie&t=", input$search1, 
                   "&plot=full&r=json")
    } else {
      url1 <- paste0("http://www.omdbapi.com/?apikey=", omdb_api_key, "&type=movie&t=", input$search1,
                     "&y=", input$year_range_1, 
                     "&plot=full&r=json")
    }
    response1 <- httr::GET(url1)
    movies1 <- jsonlite::fromJSON(httr::content(response1, "text", encoding = "UTF-8"), flatten = TRUE)
    if (movies1$Response[1] == "False") {
      movies_df1 = df_placeholder_NULL
    } else if (length(movies1$Ratings) == 0) {
      movies_df1 = df_placeholder_NoRating
      movies_df1$Title = movies1[["Title"]]
      movies_df1$Year = movies1[["Year"]]
    } else {
      movies_df1 = data.frame(Title = movies1[[1]], 
                              Year = movies1[[2]],
                              Ratings = movies1[["Ratings"]], 
                              imdbVotes = movies1[["imdbVotes"]], 
                              imdbRating = movies1[["imdbRating"]],
                              Metascore = movies1[["Metascore"]])
    }  
    
    if (is.null(input$year_range_2)) {
      url2 <- paste0("http://www.omdbapi.com/?apikey=", omdb_api_key, "&type=movie&t=", input$search2, 
                     "&plot=full&r=json")
    } else {
      url2 <- paste0("http://www.omdbapi.com/?apikey=", omdb_api_key, "&type=movie&t=", input$search2,
                     "&y=", input$year_range_2, 
                     "&plot=full&r=json")
    }
    response2 <- httr::GET(url2)
    movies2 <- jsonlite::fromJSON(httr::content(response2, "text", encoding = "UTF-8"), flatten = TRUE)
    if (movies2$Response[1] == "False") {
      movies_df2 = df_placeholder_NULL
    } else if (length(movies2$Ratings) == 0) {
      movies_df2 = df_placeholder_NoRating
      movies_df2$Title = movies2[["Title"]]
      movies_df2$Year = movies2[["Year"]]
    } else {
      movies_df2 = data.frame(Title = movies2[[1]], 
                              Year = movies2[[2]],
                              Ratings = movies2[["Ratings"]], 
                              imdbVotes = movies2[["imdbVotes"]], 
                              imdbRating = movies2[["imdbRating"]],
                              Metascore = movies2[["Metascore"]])
    }
    
    movies_df = rbind(movies_df1, movies_df2)
    # Data Cleaning
    movies_df = movies_df %>%
      
      mutate(clean_rating = case_when(
        str_detect(Ratings.Value, "/100") ~ Ratings.Value %>%
          str_remove("/100") %>% as.numeric(),
        str_detect(Ratings.Value, "/10") ~ Ratings.Value %>%
          str_remove("/10") %>% as.numeric() * 10,
        str_detect(Ratings.Value, "/5") ~ Ratings.Value %>%
          str_remove("/5") %>% as.numeric() * 20,
        str_detect(Ratings.Value, "%") ~ Ratings.Value %>%
          str_remove("%") %>% as.numeric(),
        TRUE ~ NA_real_
      ))
    movies_df = movies_df %>%
      mutate(imdbRating = as.numeric(imdbRating)) %>%
      mutate(Metascore = as.numeric(Metascore)) 
    
    # Grouping Process
    movies_df = movies_df %>%
      group_by(Title) %>%
      summarise(Year=first(Year), 
                Average_Rating=mean(clean_rating),
                Votes=first(imdbVotes), 
                imdbRating=first(imdbRating), 
                Metascore=first(Metascore))
    return(movies_df)
  }
  
  create_rate_plot <- function() {
      # Data Reshaping
      movies_df = create_df() %>% 
        mutate(imdbRating = 10 * imdbRating)
      movies_reshaped = movies_df %>%
        select(Title, Average_Rating, imdbRating, Metascore) %>%
        pivot_longer(cols = c("Average_Rating", "imdbRating", "Metascore"),
                     names_to = "Type_of_Rate",
                     values_to = "Rating_Score") %>%
        arrange(desc(Type_of_Rate))
      # Graph
      ggplot(movies_reshaped, aes(x = Type_of_Rate, y = Rating_Score, fill = Title)) + 
        geom_col(width=0.3, position = "dodge") +
        labs(title = "Rating comparison", x = "Rate Source", y = "Rating(out of 100)") + 
        ylim(0, 100) +
        theme(panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major = element_line(color = "black"),
              panel.grid.minor = element_line(color = "gray"),
              plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text = element_text(size = 10, face = "italic"),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical",
              legend.text = element_text(size = 12, face = "italic"))
  }
  
  create_vote_plot <- function() {
      # Data cleaning
      movies_reshaped = create_df() %>%
        select(Title, Votes) %>%
        mutate(Votes = as.numeric(str_replace_all(Votes, ",", "")))
      ggplot(movies_reshaped, aes(x = Title, y = Votes, fill = Title)) + 
        geom_col(width=0.3, position = "dodge") +
        labs(title = "Votes Comparison", x = "Name the of Movies", y = "Votes") +
        theme(panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major = element_line(color = "black"),
              panel.grid.minor = element_line(color = "gray"),
              plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text = element_text(size = 10, face = "italic"),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical",
              legend.text = element_text(size = 12, face = "italic"))
  }
  
  # Reactive Programing Variables
  reactive_table <- eventReactive(input$submit_btn, {
    return(create_df())
  })
  
  reactive_rate_plot <- eventReactive(input$submit_btn, {
    return(create_rate_plot())
  })
  
  reactive_vote_plot <- eventReactive(input$submit_btn, {
    return(create_vote_plot())
  })
  
  # Render
  output$rating_table = renderTable({
    reactive_table()
  })
  output$rating_plot = renderPlot({
    reactive_rate_plot()
  })
  output$voting_plot = renderPlot({
    reactive_vote_plot()
  })
}

# Execute
shinyApp(ui = ui, server = server)
