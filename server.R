## server.R

# load functions and model
#source('functions/cf_algorithm.R') # collaborative filtering
#source('functions/similarity_measures.R') # similarity measures
rec_ibcf = readRDS("data/ibcf_model.rds")
small_data = readRDS("data/small_data.rds")
top_10 = readRDS("data/top_10_info.rds")


# define functions
get_user_ratings = function(value_list) {
  dat = data.table(
    MovieID = sapply(strsplit(names(value_list), "_"),
                     function(x)
                       ifelse(length(x) > 1, x[[2]], NA)),
    Rating = unlist(as.character(value_list))
  )
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  
  n.item = ncol(small_data)
  new.ratings = rep(NA, n.item)
  new.ratings[dat$MovieID] = dat$Rating
  new.user = matrix(
    new.ratings,
    nrow = 1,
    ncol = n.item,
    dimnames = list(user = paste('newUser'),
                    item = colnames(small_data))
  )
  new.Rmat = as(new.user, 'realRatingMatrix')
  new.Rmat
}



# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies,
                  split = "::",
                  fixed = TRUE,
                  useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x)
                            paste0(small_image_url, x, '.jpg?raw=true'))

shinyServer(function(input, output, session) {
  output$genre_recs <- renderUI({
    g = input$genreInputId
    if(g == 'Children\'s'){g ='Children.s'}
    if(g == 'Film-Noir'){g = 'Film.Noir'}
    if(g == 'Sci-Fi'){g = "Sci.Fi"}
    top = top_10[which(top_10[,1]==g),][2:11]
    
    movienames = c()
    for(i in 1:10){
      name = movies$Title[which(movies$MovieID==top[i])]
      movienames = c(movienames,  name)
    }
    
    recom_result <- data.table(
      Rank = 1:10,
      MovieID = top,
      Title = movienames
    ) 
    
    
    num_rows <- 2
    num_movies <- 5 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          
          div(style = "text-align:center",
              a(
                img(src = movies$image_url[which(recom_result$MovieID[(i - 1) * num_movies + j] == movies$MovieID)], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[which(recom_result$MovieID[(i - 1) * num_movies + j] == movies$MovieID)]))
          
        )
      }))) # columns
    }) # rows
    })

##########################################################################################################  
#system 2 code
    # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(
          width = 2,
          div(style = "text-align:center", img(
            src = movies$image_url[(i - 1) * num_movies + j], height = 150
          )),
          div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
          div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(
            paste0("select_", movies$MovieID[(i - 1) * num_movies + j]),
            label = "",
            dataStop = 5
          ))
        ))
      })))
    })
  })
  
  # Calculate recommendations when the submit button is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      user_results = predict(rec_ibcf, user_ratings, n = 10)
      
      top = user_results@items$newUser
      movienames = c()
      for(i in 1:10){
        name = movies$Title[which(movies$MovieID==top[i])]
        movienames = c(movienames,  name)
      }
      
      
      recom_results <- data.table(
        Rank = 1:10,
        MovieID = user_results@items$newUser,
        Title = movienames
      ) 
      #recom_results#,
      #print(test)
      #Predicted_rating =  user_results@ratings$newUser)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          
          div(style = "text-align:center",
              a(
                img(src = movies$image_url[which(recom_result$MovieID[(i - 1) * num_movies + j] == movies$MovieID)], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[which(recom_result$MovieID[(i - 1) * num_movies + j] == movies$MovieID)]))
          
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function