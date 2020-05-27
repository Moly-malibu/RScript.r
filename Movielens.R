
#Title:  Movielens
#Author: Monica Bustamante
#Date:   May/27/2020
#Output: R Script


#INSTALL PACKAGES AND LIBRARIES
list.of.packages <- c("lubridate","stringi",
                      "lattice", "tidyverse", "caret",
                      "tidyr","stringr","ggplot2",
                      "readr")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#Install packages
install.packages("rmarkdown")

#Install packages
install.packages("tidyverse", repos = "http://cran.us.r-project.org")

#Install packages caret
install.packages("caret", repos = "http://cran.us.r-project.org")

#Install packages 
install.packages("data.table", repos = "http://cran.us.r-project.org")

#Install libraries
library(ggplot2)
library(readr)
library(lubridate)
library(stringi)
library(tidyverse)
library(caret)
library(tidyr)
library(stringr)

#download data set Movielens
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


#Read table
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

#Split dataset
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

#Mutate, rename title
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")

# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#validation dataset
validation  <- validation %>% select(-rating)

#How many rows and columns are there in the edx dataset?
  
#To see more information about the dataset
head(edx, 5)

#Dimension Dataset
dim(edx)

str(edx)

#General information about dataset
summary(edx)

#How many rows and columns are there in the edx dataset
paste('The dataset has',nrow(edx),'rows and',ncol(edx),'columns.')

#To see more information about dataset
edx %>% summarise(
  uniq_movies = n_distinct(movieId),
  uniq_users = n_distinct(userId),
  uniq_genres = n_distinct(genres))


#Mean of rating dataset
rating_mean <- mean(edx$rating)
rating_mean

  
#How many zeros were given as ratings in the edx dataset.
paste(sum(edx$rating == 0), 'ratings and',
        sum(edx$rating == 3),'ratings with 3')


edx %>% filter(rating == 3) %>% tally()


#How many different movies are in the edx dataset
n_distinct(edx$movieId)

edx %>% summarize(n_movies = n_distinct(movieId))

#How many different users are in the edx dataset? n_distinct or lenght
n_distinct(edx$userId)



edx %>% summarize(n_users = n_distinct(userId))


#How many movie ratings are in each of the following genres in the edx dataset?
# str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

# separate_rows, much slower!
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))  

#Movie ratings by Drama. str_detect Detect The Presence Or Absence Of A Pattern In A String.
drama <- edx %>% filter(str_detect(genres,"Drama"))
paste('Drama has',nrow(drama),'movies')


#Movie ratings by Comedy
comedy <- edx %>% filter(str_detect(genres,"Comedy"))
paste('Comedy has',nrow(comedy),'movies')


##Movie ratings by Thriller
thriller <- edx %>% filter(str_detect(genres,"Thriller"))
paste('Thriller has',nrow(thriller),'movies')


#Movie ratings by Romance
romance <- edx %>% filter(str_detect(genres,"Romance"))
paste('Romance has',nrow(romance),'movies')

#Which movie has the greatest number of ratings?
  
#Greatest number of ratings. Arrange rows by variables
edx %>% group_by(title) %>% 
summarise(number = n()) %>% 
arrange(desc(number))

#What are the five most given ratings in order from most to least?
#Sort a variable in descending order.
edx %>% group_by(rating) %>% 
summarize(count = n()) %>% 
top_n(5) %>%
arrange(desc(count))
  

head(sort(-table(edx$rating)),5)

hist(edx$rating)
summary(edx$rating)


#True or False: In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
#Rating movies
rating4 <- table(edx$rating)["4"]
rating35 <- table(edx$rating)["3.5"]
rating3 <- table(edx$rating)["3"]

Result <- (rating35 < rating3 && rating35 < rating4)

print(Result)

rm(rating3, rating35,  rating4, Result)


#Graphic Rating movies
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


#Predicted movie ratings and calculates RMSE.
#Movie rating predictions will be compared to the true ratings in the validation set using RMSE

data <- movies %>% separate_rows(genres, sep ="\\|")
DAT.aggregate <- aggregate(formula = cbind(n = 1:nrow(dat)) ~ genres, data = data, FUN = length)


#Size of dataset 
movielens <- left_join(ratings, 
                       movies, by = "movieId")
nrow(movielens)


#Creates Year column.
edx <- edx %>% 
  mutate(title = str_trim(title)) %>%
  extract(title, c("title_tmp", "year"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  mutate(year = if_else(str_length(year) > 4,
                        as.integer(str_split(year, "-",
                                             simplify = T)[1]),
                        as.integer(year))) %>%
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>%
  select(-title_tmp)  %>%
  mutate(genres = if_else(genres == "(No Genres Listed)",
                          `is.na<-`(genres), genres))
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")


avg_ratings <- edx %>% 
  group_by(year) %>% 
  summarise(avg_rating = mean(rating))
plot(avg_ratings)


#Root Mean Square Error  
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

lambdas <- seq(0, 5, 0.25)
rmses <- sapply(lambdas,function(l){
  mu <- mean(edx$rating) #The mean of ratings from training set
  
  Movie_effect <- edx %>%  #Adjust mean by movie effect
    group_by(movieId) %>%
    summarize(Movie_effect = sum(rating - mu)/(n()+l))
  
  Movie_user <- edx %>% #Ajdust mean by movie effect and user  
    left_join(Movie_effect, by="movieId") %>%
    group_by(userId) %>%
    summarize(Movie_user = sum(rating - Movie_effect - mu)/(n()+l))
  
  predicted_ratings <- 
    edx %>% 
    left_join(Movie_user, by = "userId") %>%
    left_join(Movie_effect, by = "movieId") %>%
    mutate(pred = mu + Movie_effect + Movie_user) %>%
    .$pred #Predict ratings 
  
  return(RMSE(predicted_ratings, edx$rating))
})
plot(lambdas, rmses,
     col = "blue")

#Calculate Lambda optimal RMSE
lambda <- lambdas[which.min(rmses)]
paste('RMSE',min(rmses),'Lambda',lambda)


#CONCLUSION:

#Predict a list of rated movies.
#Discovered patterns: as people prefer movies with a medium to high rating. (3 to 5).
#The movies preferred by the customers was the end of the 1980 and 1990 periods.
  



  


