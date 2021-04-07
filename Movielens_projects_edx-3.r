
#Install packages
install.packages("tidyverse", repos = "http://cran.us.r-project.org")

#Install packages 
install.packages("data.table", repos = "http://cran.us.r-project.org")

#Install packages caret
install.packages("caret", repos = "http://cran.us.r-project.org", dependencies=TRUE)

#Install library
library(tidyverse)
library(caret)

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
set.seed(1)

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

#To see more information about the dataset
head(edx, 5)

#General information about dataset
print(summary(edx))

#How many rows and columns are there in the edx dataset
paste('The edx dataset has',nrow(edx),'rows and',ncol(edx),'columns.')

#To see more information about dataset
edx %>% summarise(
  uniq_movies = n_distinct(movieId),
  uniq_users = n_distinct(userId),
  uniq_genres = n_distinct(genres))

#Mean or average of rating dataset
rating_mean <- mean(edx$rating)
rating_mean

#How many zeros were given as ratings in the edx dataset.
paste(sum(edx$rating == 0), 'ratings with 0 were given and',
      sum(edx$rating == 3),'ratings with 3')

edx %>% filter(rating == 3) %>% tally()

#How many different movies are in the edx dataset
n_distinct(edx$movieId)

edx %>% summarize(n_movies = n_distinct(movieId))

#How many different users are in the edx dataset. n_distinct or lenght
n_distinct(edx$userId)

edx %>% summarize(n_users = n_distinct(userId))

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

edx %>% group_by(rating) %>% 
summarize(n=n())

#Greatest number of ratings. Arrange rows by variables
edx %>% group_by(title) %>% 
summarise(number = n()) %>% 
arrange(desc(number))

head(sort(-table(edx$rating)), 5)
hist(edx$rating)
summary(edx$rating)

edx %>%  # Ratings Distribution:
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.15, color = "yellow") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Graphic Rating Distribution")

#Sort a variable in descending order.
edx %>% group_by(rating) %>% 
summarize(count = n()) %>% 
top_n(5) %>%
arrange(desc(count))

#Rating movies
rating4 <- table(edx$rating)["4"]
rating35 <- table(edx$rating)["3.5"]
rating3 <- table(edx$rating)["3"]

Result <- (rating35 < rating3 && rating35 < rating4)

print(Result)

rm(rating35, rating3, rating4, Result)

#Graphic Rating movies
edx %>%
    group_by(rating) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = rating, y = count)) +
    geom_line()

# Plot mean movie ratings given by users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "yellow") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Ratings by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_light()

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

#Create DAT for graphic and see categories by genres and years
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% 
mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
                      
dat <- movies %>% separate_rows(genres, sep ="\\|")
DAT.aggregate <- aggregate(formula = cbind(n = 1:nrow(dat)) ~ genres, data = dat, FUN = length)

#Size of dataset 
movielens <- left_join(ratings, movies, by = "movieId")
nrow(movielens)

#Genres as Drama and Comedy have high rating.

ggplot(DAT.aggregate,aes(x=genres,y=n))+
    geom_col(group="genres",alpha=0.8,fill ="yellow", color="white")+
    geom_text(label=DAT.aggregate$n,angle=45,fontface = "bold")+
theme(axis.text.x=element_text(angle = -90, hjust = 0))

#Movies from the 1980s to 1990s and older have higher average ratings than recent movies.

movielens$year <- as.numeric(substr(as.character(movielens$title),
                  nchar(as.character(movielens$title))-4,
                  nchar(as.character(movielens$title))-1))
plot(table(movielens$year),
    col = "yellow")

#Validation set will be 20% of the movieLens data
set.seed(2) #Partition dataset test and train
test_index <- createDataPartition(y = edx$rating, times = 2, p = 0.1, list = FALSE)
train <- edx[-test_index,] #train set
test <- edx[test_index,]   #test set

#Define RMSE that mesure of how spread out thesse residuals are, or concentration the data around the linea of best fit.

RMSE <- function(true_ratings, predicted_ratings){
sqrt(mean((true_ratings - predicted_ratings)^2,na.rm =TRUE))
}

mu <- mean(train$rating)  #mean

rmse_naive <- RMSE(test$rating, mu)  #MODEL 1

rmse_results <- data_frame(method='Average', RMSE=rmse_naive)
rmse_results

#Model by Movie average

mu_m2 <-  mean(train$rating) #Model 2: Y u, i =?? + b i
movie_avgs <- train%>%
  group_by(movieId) %>%
  summarize(b_i=mean(rating-mu_m2))

predicted_rating <- mu_m2+test%>%
  left_join(movie_avgs, by='movieId')%>%
  pull(b_i)

rmse_m2 <- RMSE(predicted_rating, test$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method='Movie Effect Model', RMSE=rmse_m2))
rmse_results

#Model by User average

user_avgs <- train%>%
  left_join(movie_avgs, by='movieId') %>% # Y u, i =??+b i +?? u, i w
  group_by(userId) %>%
  summarize(b_u=mean(rating - mu_m2 - b_i))

predicted_ratings <- test%>%
  left_join(movie_avgs, by='movieId')%>%
  left_join(user_avgs, by='userId')%>%
  mutate(pred=mu_m2 + b_i + b_u)%>%
  pull(pred)

rmse_m3 <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method='Movie + User Effect Model', RMSE=rmse_m3))
rmse_results

rating_vp <- validation %>%
  left_join(movie_avgs, by = 'movieId')%>%
  left_join(user_avgs, by='userId')%>%
  mutate(pred=mu_m2 + b_i + b_u)%>%
  pull(pred)

validation_m3 <- RMSE(validation$rating, rating_vp)
rmse_results <- bind_rows(rmse_results, data_frame(Method='Validation', RMSE=validation_m3))
rmse_results

edx <- train %>% # It extracts the release year of the movie.
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
  mutate(genres = if_else(genres == "(no genres listed)",
                          `is.na<-`(genres), genres))
validation <- test %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Root Mean Square Error Loss Function
RMSE <- function(true_ratings, predicted_ratings){
        sqrt(mean((true_ratings - predicted_ratings)^2))
      }
      
lambdas <- seq(0, 5, 0.25)
rmses <- sapply(lambdas,function(l){
  
  #Mean of ratings from the edx training set
  mu <- mean(train$rating)
  
  #Adjust mean by movie effect and penalize low number on ratings
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  #Adjdust mean by user and movie effect and penalize low number of ratings
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #Predict ratings in the training set to derive optimal penalty value 'lambda'
  predicted_ratings <- 
    train %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, train$rating))
})
plot(lambdas, rmses, #Graphic
    col = "blue")

lambda <- 0.5
      
pred_y_lse <- sapply(lambda,function(l){
  
  #Derive the mearn from the training set
  mu <- mean(edx$rating)
  
  #Calculate movie effect with optimal lambda
b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  #Calculate user effect with optimal lambda
b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #Predict ratings on validation set
predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred #validation
  
  return(predicted_ratings)
  
})
#Plot Linear, where we can see that the movies before 2000 are preferred for the customers.
avg_ratings <- edx %>% 
group_by(year) %>% 
summarise(avg_rating = mean(rating))
plot(avg_ratings)

library(lubridate)

tibble(`Initial Date` = date(as_datetime(min(edx$timestamp), origin="1980-01-01")),
       `Final Date` = date(as_datetime(max(edx$timestamp), origin="1980-01-01"))) %>%
  mutate(Period = duration(max(edx$timestamp)-min(edx$timestamp)))

edx %>% mutate(date = date(as_datetime(timestamp, origin="1990-01-01"))) %>%
  group_by(date, title) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  head(25)


