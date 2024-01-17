library(tidyverse)
library(RSelenium)
library(dplyr)
library(stringr)


rD <- rsDriver(browser = "firefox",
               chromever = NULL)

remDr <- rD[["client"]]

#function to click load more in imdb's review page

scroll_navigation <- function() {
  while(TRUE) {
    # Check if the "Load More" button exists
    loadmoreElem <- tryCatch({
      remDr$findElements(using = "xpath", value = '//*[@id="load-more-trigger"]')
    }, error = function(e) NULL)
    
    # Check if the "Load More" button's parent div has the 'ipl-load-more--loaded-all' class
    allLoadedElem <- tryCatch({
      remDr$findElements(using = "css", value = '.ipl-load-more.ipl-load-more--loaded-all')
    }, error = function(e) NULL)
    
    # If the "Load More" button does not exist or all reviews have been loaded, break the loop
    if (length(loadmoreElem) == 0 || length(allLoadedElem) > 0) {
      break
    }
    
    # If the "Load More" button exists, click it
    loadmore <- loadmoreElem[[1]]
    loadmore$clickElement()
    
    # Pause code to ensure webpage loads properly
    #Sys.sleep(1.5)
  }
}

#function to reveal reviews that are too long

dropdown_all <- function() {

  button_class <- "expander-icon-wrapper"  
  
  # Find all buttons with the specified class
  buttons <- remDr$findElements(using = "css selector", value = paste(".", button_class, sep = ""))
  
  # Click each button
  for (button in buttons) {
    button$clickElement()
  }
}

#function to scrape review data from imdb
#link -> imdb review link, movie_title -> movie title, pcuser -> name of computer
getreviewdata <- function(link, movie_title, pcuser){
  
  remDr$navigate(link)
  Sys.sleep(2)
  
  scroll_navigation()
  dropdown_all()
  
  usernamesElem <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'display-name-link')]")
  reviewer_username <- unlist(lapply(usernamesElem, function(x){x$getElementText()}))
  reviewer_username <- head(reviewer_username, 1500)
  
  titleElem <- remDr$findElements(using = 'xpath', "//a[starts-with(@class, 'title')]")
  review_title <- unlist(lapply(titleElem, function(x){x$getElementText()}))
  review_title <- head(review_title, 1500)
  
  dateElem <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'review-date')]")
  review_date <- unlist(lapply(dateElem, function(x){x$getElementText()}))
  review_date <- head(review_date, 1500)
  
  reviewElem <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'text show-more__control')]")
  review_text <- unlist(lapply(reviewElem, function(x){x$getElementText()}))
  review_text <- head(review_text,1500)
  
  #due to some reviews having no ratings 
  all_reviews <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'lister-item-content')]")
  
  rating <- vector("character", length(all_reviews))
  
  for (i in seq_along(all_reviews)) {
    review <- all_reviews[[i]]
    
    ratingElem <- tryCatch({
      review$findChildElement("css", value = "span.rating-other-user-rating")
    }, error = function(e) NULL)
    
    rating[i] <- if (!is.null(ratingElem)) ratingElem$getElementText() %>% str_extract("\\d+") else NA
  }
  
  rating <- head(rating,1500)
  
  movie_name <- paste(movie_title, "Reviews", sep = "")
  assign(movie_name, data.frame(reviewer_username, review_title, review_date, review_text, rating))
  
  View(movie_name)
  
  csv_file_path <- paste("C:/Users/",pcuser,"/Downloads/", movie_name, ".csv", sep="")
  write.csv(get(movie_name), csv_file_path, row.names = FALSE)
}

#Retrieving Morbius review data
getreviewdata('https://www.imdb.com/title/tt5108870/reviews?ref_=tt_urv',"Morbius","Dany")
#Retrieving Twilight (2008) review data 
getreviewdata('https://www.imdb.com/title/tt1099212/reviews?ref_=tt_urv',"Twilight","Dany")
#Retrieving Blair Witch Project review data
getreviewdata('https://www.imdb.com/title/tt0185937/reviews?ref_=tt_urv',"Blair_Witch","Dany")
#Retrieving Prometheus review data
getreviewdata('https://www.imdb.com/title/tt1446714/reviews?ref_=tt_urv',"Prometheus","Dany")
#Retrieving Charlie and the Chocolate Factory review data
getreviewdata('https://www.imdb.com/title/tt0367594/reviews?ref_=tt_urv',"Chocolate_Factory","Dany")
  
  
  
  
  
  
  
  
  










