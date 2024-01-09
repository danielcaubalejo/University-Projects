# Installing packages
install.packages("rvest")
install.packages("dplyr")
install.packages("tidyr")
install.packages("data.table")
install.packages("tm")
install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("textstem")
install.packages("ggplot2")
install.packages("gbm")

# Loading Packages
library(rvest)
library(dplyr)
library(tidyr)
library(data.table)
library(tm)
library(NLP)
library(SnowballC)
library(wordcloud)
library(textstem)
library(ggplot2)
library(gbm)


####################################################################################################################

#### Main Question 1: How is traditional media being affected by streaming services? ####

## Sub Question 1a): How is cable TV performing since the rise of streaming services? ##

##### 1a). Scraping #####

# Netflix users and change over time
# Sources: https://explodingtopics.com/blog/video-streaming-stats

link.explodingtopics = "https://explodingtopics.com/blog/video-streaming-stats"
page.explodingtopcs = read_html(link.explodingtopics)

# Scraping for Netflix users and change over time
year.netflix = page.explodingtopcs %>% html_nodes("table:nth-child(69) tr+ tr td:nth-child(1)") %>% html_text()
netflix.users <- page.explodingtopcs %>% html_nodes("table:nth-child(69) tr+ tr td:nth-child(2)") %>% html_text()
netflix.change.over.previous.year <- page.explodingtopcs %>% html_nodes("table:nth-child(69) tr+ tr td~ td+ td") %>% html_text()

# Creating data frame for Netflix users and change
netflix.users.df <- data.frame(year.netflix, netflix.users, netflix.change.over.previous.year)

# Change data type for Netflix's columns to be numeric & converting into millions
netflix.users.df$netflix.users <- gsub(" million", "", netflix.users.df$netflix.users)
netflix.users.df$netflix.users <- as.numeric(netflix.users.df$netflix.users)
netflix.users.df$netflix.users <- netflix.users.df$netflix.users * 1e6

netflix.users.df$netflix.change.over.previous.year <- gsub("↑| million", "", netflix.users.df$netflix.change.over.previous.year)
netflix.users.df$netflix.change.over.previous.year <- as.numeric(netflix.users.df$netflix.change.over.previous.year)
netflix.users.df$netflix.change.over.previous.year <- netflix.users.df$netflix.change.over.previous.year * 1e6

netflix.users.df$year.netflix <- as.numeric(netflix.users.df$year.netflix)

# Scraping for Amazon users and change over time
link.explodingtopics = "https://explodingtopics.com/blog/video-streaming-stats"
page.explodingtopcs = read_html(link.explodingtopics)

amazon.table = page.explodingtopcs %>% html_nodes("table:nth-child(72) td") %>% html_text()

# We have 3 headers, data divide by 3
amazon.table.n <- length(amazon.table) / 3

# Creating a tibble
amazon.tibble <- tibble(
  key = rep(seq_len(amazon.table.n), each = 3),
  variable = rep(c("year.amazon", "amazon.users", "amazon.change.over.previous.year"), times = amazon.table.n),
  value = amazon.table
  )

# Using Pivot_wider
amazon.tibble.wide <- amazon.tibble %>%
  pivot_wider(names_from = variable, values_from = value)

# Removing the first row and column of our table
amazon.users.df <- amazon.tibble.wide[-1, -1]

# Change data type for Amazon to be numeric & converting into millions
amazon.users.df$amazon.users <- gsub(" million", "", amazon.users.df$amazon.users)
amazon.users.df$amazon.users <- as.numeric(amazon.users.df$amazon.users)
amazon.users.df$amazon.users <- amazon.users.df$amazon.users * 1e6

amazon.users.df$amazon.change.over.previous.year <- gsub("↑| million", "", amazon.users.df$amazon.change.over.previous.year)
amazon.users.df$amazon.change.over.previous.year <- as.numeric(amazon.users.df$amazon.change.over.previous.year)
amazon.users.df$amazon.change.over.previous.year <- amazon.users.df$amazon.change.over.previous.year * 1e6

amazon.users.df$year.amazon <- as.numeric(amazon.users.df$year.amazon)
amazon.users.df <- amazon.users.df[amazon.users.df$year.amazon >= 2013 & amazon.users.df$year.amazon <= 2023, ]

# Cable TV users in the US

link.wikipedia = "https://en.wikipedia.org/wiki/Cable_television_in_the_United_States"
page.wikipedia = read_html(link.wikipedia)

cable.users.df <- page.wikipedia %>% html_nodes("table.wikitable") %>% html_table() %>% .[[1]]

# Removing [#] from the ends of the numbers
cable.users.df$`Cable TV subscribers` <- sub("\\[\\d+\\]", "", cable.users.df$`Cable TV subscribers`)
cable.users.df$`Telephone company TV subscribers` <- sub("\\[\\d+\\]", "", cable.users.df$`Telephone company TV subscribers`)

# Removing month from years
cable.users.df$Year <- as.numeric(sub("\\w+\\.\\s", "", cable.users.df$Year))

# Turn Cable TV into numeric
cable.users.df$`Cable TV subscribers` <- as.numeric(gsub(",", "",cable.users.df$`Cable TV subscribers`))

# Removing unused Telephone Company TV subscribers column
cable.users.df$`Telephone company TV subscribers` <- NULL

# Sub-setting data to be from 2013 to 2023
cable.users.df <- cable.users.df[cable.users.df$Year >= 2013 & cable.users.df$Year <= 2023, ]

# Adding change over time column
cable.users.df$change.over.previous.year <- c(NA, diff(cable.users.df$`Cable TV subscribers`))

##### 1a.) Models ######

# Cable TV users predictions for next 5 years

# Creating subsets to match years 
subset.amazon <- amazon.users.df[1:(nrow(amazon.users.df)-1), ]
subset.netflix <- netflix.users.df[1:(nrow(netflix.users.df)-1), ]

# Making regression model to predict how many cable users in the next 5 years
cable.prediction.model <- lm(cable.users.df$`Cable TV subscribers` ~ cable.users.df$Year + 
                               cable.users.df$change.over.previous.year +
                               subset.amazon$year.amazon + subset.amazon$amazon.change.over.previous.year + 
                               subset.netflix$year.netflix +
                               subset.netflix$netflix.change.over.previous.year)

# Creating a sequence of years to match our predictions
next.years <- data.frame(Year = seq(from = 2023, to = 2031, by = 1))

# Predicting future Cable TV Subscribers
predictions <- predict(cable.prediction.model, newdata = next.years)

# Removing NA from predictions
subset.predictions <- predictions[-1]

# Add predictions and years sequence to a data frame
cable.users.predict.df <- data.frame(Year = next.years, predicted.users = subset.predictions)

# Show only next 5 years
predict.next.5.df <- head(cable.users.predict.df, n = 5)

# Making a correlation matrix to determine how correlated the change in subscribers between Amazon, Netflix and Cable TV is

# Merge data frames based on year column
subscribers.merged.df <- merge(merge(amazon.users.df, netflix.users.df, by.x = "year.amazon", by.y = "year.netflix"), cable.users.df, by.x = "year.amazon", by.y = "Year")

# Getting rid of rows with NA
subscribers.merged.df <- subscribers.merged.df[-1 , ]

# Calculate the correlation matrix based on change over previous year
correlation.matrix <- cor(subscribers.merged.df[, c("amazon.change.over.previous.year", "netflix.change.over.previous.year", "change.over.previous.year")])

# Display the correlation matrix
print(correlation.matrix)

# Amazon v. Netflix has a correlation of 0.27, slightly positively correlated, increase as each other increase
# Amazon v Cable TV has a negative correlation of -0.15, as amazon increases, Cable TV subscribers decrease
# Netflix v Cable TV has a negative correlation of -0.22, as Netflix increases, Cable TV subscribers decrease


####### sub question 1b) Does the increase in subscribers of streaming platform affect the total box revenue?
## Box Office Revenue Over the year
# Sources:
# https://www.statista.com/statistics/187073/tickets-sold-at-the-north-american-box-office-since-1980/
# https://www.the-numbers.com/market/

link.thenumbers = "https://www.the-numbers.com/market/"
page.thenumbers = read_html(link.thenumbers)

# Scraping Annual Ticket sales table
year = page.thenumbers %>% html_nodes("center:nth-child(9) a") %>% html_text()
tickets.sold = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(2)") %>% html_text()
total.box.office = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(3)") %>% html_text()
total.inflation.adj.box.office = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(4)") %>% html_text()
avg.ticket.price = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(5)") %>% html_text()

# Creating the scraped data into a data frame
annual.ticket.sales.df <- data.frame(year, tickets.sold, total.box.office, total.inflation.adj.box.office, avg.ticket.price) 
annual.ticket.sales.df_2013 <- filter(annual.ticket.sales.df, year >= 2013)
annual.ticket.sales.df_2013$total.inflation.adj.box.office <-  as.numeric(gsub("[\\$,]", "", annual.ticket.sales.df_2013$total.inflation.adj.box.office)) 
annual.ticket.sales.df_before<- filter(annual.ticket.sales.df, year >= 2007 & year <= 2013)
annual.ticket.sales.df_after <- filter(annual.ticket.sales.df, year >= 2013 & year <= 2019)
annual.ticket.sales.df_before$total.inflation.adj.box.office <- as.numeric(gsub("[\\$,]", "", annual.ticket.sales.df_before$total.inflation.adj.box.office))
annual.ticket.sales.df_after$total.inflation.adj.box.office <- as.numeric(gsub("[\\$,]", "", annual.ticket.sales.df_after$total.inflation.adj.box.office))

#Combine Netflix and Amazon tables together

Total_Subscribers_df <- data.frame(year = amazon.users.df$year.amazon, total_subs =  amazon.users.df$amazon.users + netflix.users.df$netflix.users)

## Model

##Perform one sided t.test
t_test_box_revenue_difference <- t.test(annual.ticket.sales.df_before$total.inflation.adj.box.office, annual.ticket.sales.df_after$total.inflation.adj.box.office, alternative = "greater")
print(t_test_box_revenue_difference)

### The hypothesis is True. Adjusted by inflation, the total box revenue from 2013-2019 decreases.

## What is the correlation between the number of subscribers and total box revenue
totalsubs_totalboxoffice_merged <- merge(Total_Subscribers_df, annual.ticket.sales.df_2013, by = "year")
regression_1 <- lm(total.inflation.adj.box.office ~ total_subs, data = totalsubs_totalboxoffice_merged)
summary(regression_1)

## It shows that the change in total box office revenue adjusted by inflation can be explained by the change in the subscribers of streaming platform 52.55% of the time



####################################################################################################################


#### Main Question 2: How does movie industry performance compare before and after the pandemic? ####

## Sub question 2a): How has ticket sales and box office revenue changed over time since the pandemic?
##### Scraping ######

# Sources:
# https://www.statista.com/statistics/187073/tickets-sold-at-the-north-american-box-office-since-1980/
# https://www.the-numbers.com/market/

link.thenumbers = "https://www.the-numbers.com/market/"
page.thenumbers = read_html(link.thenumbers)

# Scraping Annual Ticket sales table
year = page.thenumbers %>% html_nodes("center:nth-child(9) a") %>% html_text()
tickets.sold = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(2)") %>% html_text()
total.box.office = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(3)") %>% html_text()
total.inflation.adj.box.office = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(4)") %>% html_text()
avg.ticket.price = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(5)") %>% html_text()

# Creating the scraped data into a data frame
annual.ticket.sales.df <- data.frame(year, tickets.sold, total.box.office, total.inflation.adj.box.office, avg.ticket.price)

# Since the data for Netflix and amazon is from 2013 onwards, I'm filtering the data for tickets to be the same
annual.ticket.sales.df.relevant = annual.ticket.sales.df[annual.ticket.sales.df$year>= 2013, ]

# Arrange them in ascending order of year
annual.ticket.sales.df.relevant = annual.ticket.sales.df.relevant[order(annual.ticket.sales.df.relevant$year), ]

# Add columns for delta of tickets sold and total revenue
annual.ticket.sales.df.relevant$tickets.sold = as.numeric(gsub(",", "", annual.ticket.sales.df.relevant$tickets.sold))
annual.ticket.sales.df.relevant$total.box.office <- as.numeric(gsub("[^0-9.]", "", gsub(",", "", annual.ticket.sales.df.relevant$total.box.office)))
annual.ticket.sales.df.relevant = annual.ticket.sales.df.relevant %>%
  select(year,tickets.sold,total.box.office)%>%
  mutate(tickets_sold_delta = c(NA, diff(tickets.sold)),
         revenue_delta = c(NA, diff(total.box.office)))

##### 2a): Model ######

# Making regression model to analyze the trends in annual ticket sales
model.tickets.sold <- lm(annual.ticket.sales.df.relevant$tickets.sold ~ annual.ticket.sales.df.relevant$year, data = annual.ticket.sales.df.relevant)

par(mfrow = (c(1,2)))

# Plot the regression lines
plot(annual.ticket.sales.df.relevant$year, annual.ticket.sales.df.relevant$tickets.sold, col = "blue",
     xlab = "Year", ylab = "Ticket Sales", main = "Ticket Sale Trend Analysis")
lines(annual.ticket.sales.df.relevant$year, predict(model.tickets.sold), col = "blue", lty = 2)

# Making regression model to analyze trends in total box office
model.box.office <- lm(annual.ticket.sales.df.relevant$total.box.office ~ annual.ticket.sales.df.relevant$year, data = annual.ticket.sales.df.relevant)

# Plot the regression lines
plot(annual.ticket.sales.df.relevant$year, annual.ticket.sales.df.relevant$total.box.office, col = "red",
     xlab = "Year", ylab = "Total Box Office Revenue", main = "Box Office Revenue Trend Analysis")
lines(annual.ticket.sales.df.relevant$year, predict(model.box.office), col = "red", lty = 2)

# In both graphs we see a significant drop in tickets sold and revenue in 2019, most likely due to the COVID 19 pandemic

# Getting plotted points into a data frame so we can download as csv later
plot_data_tickets <- data.frame(
  Year = annual.ticket.sales.df.relevant$year,
  Observed_Tickets_Sold = annual.ticket.sales.df.relevant$tickets.sold,
  Predicted_Tickets_Sold = predict(model.tickets.sold)
)

plot_data_box_office <- data.frame(
  Year = annual.ticket.sales.df.relevant$year,
  Observed_Total_Box_Office = annual.ticket.sales.df.relevant$total.box.office,
  Predicted_Total_Box_Office = predict(model.box.office)
)



######################################################################################################################################

#### Main question 3: Is there a significant relationship between a movie's budget and its box office revenue?

# Scraping
movies_data = read.csv("https://raw.githubusercontent.com/danielgrijalva/movie-stats/master/movies.csv")
movies_data_cleaned = na.omit(movies_data)
movie_budget = lm(gross ~ budget, data = movies_data_cleaned)
summary(movie_budget)

# At a chosen significance level of 0.01 we can reject the null hypothesis that there isn't a significant relationship 
# between revenue and movie budget.

# Scatter plot
ggplot(movies_data, aes(x = budget, y = gross)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Gross Revenue vs. Budget",
       x = "Budget",
       y = "Gross Revenue") +
  theme_minimal()

## Sub question 3a): Does the genre of a movie impact the relationship between its budget and box office revenue?
model_genre = lm(gross ~ budget + genre, data = movies_data_cleaned)
summary(model_genre)

## looking at the summary, some of the genre have significant p values.
anova(movie_budget, model_genre)

##The anova test shows that including genre in the model significantly improves the performance of the model


####################################################################################################################

#### Main Question 4: How does movie reviews affect movie ratings? ####


#data scraped from imdb and csv converted from q3_data_collection.R
Blair_Witch <- read.csv("Blair_WitchReviews.csv")
Morbius <- read.csv("MorbiusReviews.csv")
Prometheus <- read.csv("PrometheusReviews.csv")
Twilight <- read.csv("TwilightReviews.csv")

#removing NA rows
Blair_Witch <- na.omit(Blair_Witch)
Morbius <- na.omit(Morbius)
Prometheus <- na.omit(Prometheus)
Twilight <- na.omit(Twilight)

#word cleaning function
word_processing <- function(movie_df, review_text, review_title){
  
  # Filter out reviews with rating greater than  5 "good reviews"
  #movie_df <- dplyr::filter(movie_df, rating > 5)

  # Filter out reviews with rating less than or equal to 5 "bad"
  movie_df <- dplyr::filter(movie_df, rating <= 5)
  
  #preparing review_text to be used in corpus
  corpus_text <- Corpus(VectorSource(movie_df$review_text))
  #transform text into lowercase
  corpus_text <- tm_map(corpus_text, content_transformer(tolower))
  #remove any punctuations
  corpus_text <- tm_map(corpus_text, removePunctuation)
  #remove numbers
  corpus_text <- tm_map(corpus_text, removeNumbers)
  #remove english stopwords examples - stopwords("en")
  corpus_text <- tm_map(corpus_text, removeWords, stopwords("english"))
  #strip whitespaces from deleted characters
  corpus_text <- tm_map(corpus_text, stripWhitespace)
  #lemmatize words - return to root form
  corpus_text <- tm_map(corpus_text, lemmatize_words)
  #remove additional keywords deemed not useful for the model
  corpus_text <- tm_map(corpus_text, removeWords, c("film","movie"))
  
  #convert text corpus into document-term matrix
  dtm_text <- DocumentTermMatrix(corpus_text)
  
  #remove any terms not appearing in 98% of documents
  dtm_text <- removeSparseTerms(dtm_text, sparse = 0.98)
  
  # Process review_title
  #preparing review_text to be used in corpus
  corpus_title <- Corpus(VectorSource(movie_df$review_title))
  #transform text into lowercase
  corpus_title <- tm_map(corpus_title, content_transformer(tolower))
  #remove any punctuation
  corpus_title <- tm_map(corpus_title, removePunctuation)
  #remove numbers
  corpus_title <- tm_map(corpus_title, removeNumbers)
  #remove english stopwords examples - stopwords("en")
  corpus_title <- tm_map(corpus_title, removeWords, stopwords("english"))
  #strip whitespaces from deleted characters
  corpus_title <- tm_map(corpus_title, stripWhitespace)
  #lemmatize words - return to root form
  corpus_title <- tm_map(corpus_title, lemmatize_words)
  #remove additional keywords deemed not useful for the model
  corpus_title <- tm_map(corpus_title, removeWords, c("movie","film"))
  
  #convert text corpus into document-term matrix
  dtm_title <- DocumentTermMatrix(corpus_title)
  #remove any terms not appearing in 98% of documents
  dtm_title <- removeSparseTerms(dtm_title, sparse = 0.98)
  
  #combine 
  dtm_combined <- cbind(dtm_text, dtm_title)
  
  processed_df <- data.frame(as.matrix(dtm_combined), y = movie_df$rating)
  
  return(processed_df)
}

#### Sub question 4a): Is there a significant relationship between specific words in movie reviews and rating?
gbmfunction<- function(processed_dataframe){
  
  #training and testing data
  set.seed(123)
  train <- sample(nrow(processed_dataframe), floor(.5*nrow(processed_dataframe)),replace = FALSE)
  train_data <- processed_dataframe[train,]
  test_data <- processed_dataframe[-train,]
  
  #creating gbm model with test data
  fit <- gbm(y ~ ., distribution = "tdist", data = train_data, n.trees = 1000, 
             interaction.depth = 10, shrinkage = 0.01,n.cores = NULL, verbose = FALSE)
  
  #predicting ratings using test data
  predicted_ratings <- predict(fit, test_data)
  
  #actual ratings from test data
  actual_ratings <- test_data$y
  #calculating residuals
  residuals <- predicted_ratings - actual_ratings 
  
  #data frame to compare actual vs predicted and residuals
  df_plot <- data.frame(
    actual_ratings,
    predicted_ratings,
    residuals
  )
  
  colnames(df_plot) <- c("Actual", "Predicted", "Residuals")
  #return both actual vs predicted dataframe and gbm model for future
  return(list("plot" = df_plot, "fit" = fit))
}

####summary plot visuals
GBMsummaryplot <- function(Regressionfit, movie_title){
  
  summary.gbm(Regressionfit, cBars = 10)
  title(main = paste0(movie_title, "GBM Summary"))
 
}

#processing text data 
Prometheus_processed <- word_processing(Prometheus, 'review_text','review_title')
Twilight_processed <- word_processing(Twilight, 'review_text','review_title')
Blair_processed <- word_processing(Blair_Witch, 'review_text','review_title')
Morbius_processed <- word_processing(Morbius, 'review_text', 'review_title')

#####analysis#####

#applying gbm function to processed datasets
Prometheusgbm <- gbmfunction(Prometheus_processed)
Twilightgbm <- gbmfunction(Twilight_processed)
Blairgbm <- gbmfunction(Blair_processed)
Morbiusgbm <- gbmfunction(Morbius_processed)

#top 10 relative influence words
head(summary(Prometheusgbm$fit), 10)
head(summary(Twilightgbm$fit), 10)
head(summary(Blairgbm$fit), 10)
head(summary(Morbiusgbm$fit), 10)

#easier plot to see
par(mfrow = c(1, 4))
GBMsummaryplot(Prometheusgbm$fit, 'Prometheus')
GBMsummaryplot(Twilightgbm$fit, 'Twilight')
GBMsummaryplot(Blairgbm$fit, 'Blair Witch')
GBMsummaryplot(Morbiusgbm$fit, 'Morbius')

####diagnostics####
#accuracy testing - mae
pro_mae <- mean(abs(Prometheusgbm$plot$Residuals))
twi_mae <- mean(abs(Twilightgbm$plot$Residuals))
bla_mae <- mean(abs(Blairgbm$plot$Residuals))
Mor_mae <- mean(abs(Morbiusgbm$plot$Residuals))

#accuracy testing - rmse
pro_rmse <- sqrt(mean((Prometheusgbm$plot$Residuals)^2))
twi_rmse <- sqrt(mean((Twilightgbm$plot$Residuals)^2))
bla_rmse <- sqrt(mean((Blairgbm$plot$Residuals)^2))
Mor_rmse <- sqrt(mean((Morbiusgbm$plot$Residuals)^2))

#placing all mae and rmse into one readable dataframe
movie_names <- c("Prometheus", "Twilight", "Blair_Witch", "Morbius")

mae_values <- c(pro_mae, twi_mae, bla_mae, Mor_mae)

rmse_values <- c(pro_rmse, twi_rmse, bla_rmse, Mor_rmse)

accuracy_df <- data.frame(Movie = movie_names, MAE = mae_values, RMSE = rmse_values)


