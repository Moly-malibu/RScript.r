
---
author: "Monica Bustamante"
date: "10/07/2021"
output:
  html_document:
    df_print: paged
  word_document: default
  pdf_document:
    latex_engine: xelatex
---









                                         Introduction:
                                         
                                         
                                        
<p style='text-align: justify;'> Movielenss is a project developed by GroupLens, a research laboratory at the University of Minnesota. MovieLens provides online movie recommender algorithms, the full data set consists of more de 25 million ratings across more than 40,000 movies by more than 250.000 users, all users selected had rated at least 20 movies, each user is represented by id. 
    This project will predict features and the rating of movies by users using ratings that have been collected for several years by Movilens and thus convert them to algorithms and machine learning models, and then recommend users in their future searches, as a result, verify the performance of algorithms. For the evaluation, the residual mean square error (RMSE) of the predictions will be used and thus compare the real rating of the users. 
    In general, the algorithms and model will show a deep understanding of the variables, observations, and ratings given by users, and as a result, compare the final results and predictions. 

</p>




-  Create Edx Set, validation set
-  Install Packages
-  Install Libraries
-  Load Data set from HTTP
-  Create Rating
-  Split data
-  Clean Data Set Na
-  Create DataFrame
-  Create validation set
-  Analysis of the variables
-  Model Developing Approach



## 1. Installing essential Packages and Libraries


```R
#Install packages
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
install.packages("data.table", repos = "http://cran.us.r-project.org")
install.packages("caret", repos = "http://cran.us.r-project.org", dependencies=TRUE)
install.packages(basictabler)
```

```R
#Install library
library(tidyverse)
library(caret)
library(basictabler)
```


## 2. DataSet Downloading


<p style='text-align: justify;'> Data set is from the web http://files.grouplens.org/datasets/movielens/ml-10m.zip", and it is stored in temporary file.
</p>


```R
#download data set Movielens
dl <- tempfile()
 download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
```

```R
#Read table
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
```

```R
#Split dataset
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
```

```R
#Mutate, rename title
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                            title = as.character(title),
                                            genres = as.character(genres))
```


```R
movielens <- left_join(ratings, movies, by = "movieId")
```


## Split and Validation:

    Prepared the Movilens dataset split and validation by 10%. 
</p>


```R
# Validation set will be 10% of MovieLens data
set.seed(1)

# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
 edx <- movielens[-test_index,]
 temp <- movielens[test_index,]
```


```R
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")
```

```R
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
 edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

```

```R
#validation dataset
validation  <- validation %>% select(-rating)
```


# 3. Data Cleansing

Verify nan values in Edx validation dataframes:


```R
na_edx <- sapply(edx, function(x) sum(is.na(x)))
na_validation <- sapply(validation, function(x) sum(is.na(x)))
print(na_edx, na_validation)
```

       userId   movieId    rating timestamp     title    genres 
            0         0         0         0         0         0 




# 4. Basic information at Data Set

Acquire information by exploring and analyzing the dataset, understanding the effects of the different variables.


# A. How many rows and columns are there in the edx dataset?


```R
#To see more information about the dataset
tbl <-BasicTable$new()
tbl$addData(edx)
tbl(head(edx, 5))
```

<table>
<thead><tr><th scope=col>userId</th><th scope=col>movieId</th><th scope=col>rating</th><th scope=col>timestamp</th><th scope=col>title</th><th scope=col>genres</th></tr></thead>
<tbody>
	<tr><td>1                           </td><td>122                         </td><td>5                           </td><td>838985046                   </td><td>Boomerang (1992)            </td><td>Comedy|Romance              </td></tr>
	<tr><td>1                           </td><td>185                         </td><td>5                           </td><td>838983525                   </td><td>Net, The (1995)             </td><td>Action|Crime|Thriller       </td></tr>
	<tr><td>1                                                                   </td><td>231                                                                 </td><td>5                                                                   </td><td>838983392                                                           </td><td>Dumb &amp; Dumber (1994)                                            </td><td><span style=white-space:pre-wrap>Comedy                      </span></td></tr>
	<tr><td>1                           </td><td>292                         </td><td>5                           </td><td>838983421                   </td><td>Outbreak (1995)             </td><td>Action|Drama|Sci-Fi|Thriller</td></tr>
	<tr><td>1                           </td><td>316                         </td><td>5                           </td><td>838983392                   </td><td>Stargate (1994)             </td><td>Action|Adventure|Sci-Fi     </td></tr>
</tbody>
</table>


```R
#General information about dataset
summary(edx)
```


         userId         movieId          rating        timestamp        
     Min.   :    1   Min.   :    1   Min.   :0.500   Min.   :7.897e+08  
     1st Qu.:18122   1st Qu.:  648   1st Qu.:3.000   1st Qu.:9.468e+08  
     Median :35743   Median : 1834   Median :4.000   Median :1.035e+09  
     Mean   :35869   Mean   : 4120   Mean   :3.512   Mean   :1.033e+09  
     3rd Qu.:53602   3rd Qu.: 3624   3rd Qu.:4.000   3rd Qu.:1.127e+09  
     Max.   :71567   Max.   :65133   Max.   :5.000   Max.   :1.231e+09  
        title              genres         
     Length:9000061     Length:9000061    
     Class :character   Class :character  
     Mode  :character   Mode  :character  
                                          
                                          
                                          
<p style='text-align: justify;'> 
The edx data has 9,000,055 rows or observations and 6 columns or variables. 69,878 users rated one, 797 genres, and more of the 10,677 movies. Each row represents one user’s rating to a single movie.
    
The UserId has Median 35743, while Median is 1834 by MovieId, rating is 4.

</p>


```R
#How many rows and columns are there in the edx dataset
paste('The edx dataset has',nrow(edx),'rows and',ncol(edx),'columns.')
```


'The edx dataset has 9000061 rows and 6 columns.'


```R
#To see more information about dataset
edx %>% summarise(
  print(uniq_movies = n_distinct(movieId)),
  print(uniq_users = n_distinct(userId)),
  print(uniq_genres = n_distinct(genres)))
```


<table>
<thead><tr><th scope=col>uniq_movies</th><th scope=col>uniq_users</th><th scope=col>uniq_genres</th></tr></thead>
<tbody>
	<tr><td>10677</td><td>69878</td><td>797  </td></tr>
</tbody>
</table>




```R
#Mean or average of rating dataset
rating_mean <- mean(edx$rating)
rating_mean
```


3.51246397107753



# B. How many zeros were given as ratings in the edx dataset?


```R
#How many zeros were given as ratings in the edx dataset.
paste(sum(edx$rating == 0), 'ratings with 0 were given and',
      sum(edx$rating == 3),'ratings with 3')
```


'0 ratings with 0 were given and 2121638 ratings with 3'



```R
print(edx %>% filter(rating == 3) %>% tally())
```


<table>
<thead><tr><th scope=col>n</th></tr></thead>
<tbody>
	<tr><td>2121638</td></tr>
</tbody>
</table>




# C. How many different movies are in the edx dataset?


```R
#How many different movies are in the edx dataset
n_distinct(edx$movieId)
```


10677



```R
print(edx %>% summarize(n_movies = n_distinct(movieId)))
```


<table>
<thead><tr><th scope=col>n_movies</th></tr></thead>
<tbody>
	<tr><td>10677</td></tr>
</tbody>
</table>




# D. How many different users are in the edx dataset?


```R
#How many different users are in the edx dataset. n_distinct or lenght
n_distinct(edx$userId)
```


69878



```R
edx %>% summarize(n_users = n_distinct(userId))
```


<table>
<thead><tr><th scope=col>n_users</th></tr></thead>
<tbody>
	<tr><td>69878</td></tr>
</tbody>
</table>




# E. How many movie ratings are in each of the following genres in the edx dataset?


```R
          
# separate_rows, much slower.
edx %>% separate_rows(genres, sep = "\\|") %>%
    group_by(genres) %>%
    summarize(count = n()) %>%
    print(arrange(desc(count)))
```


<table>
<thead><tr><th scope=col>genres</th><th scope=col>count</th></tr></thead>
<tbody>
	<tr><td>Drama             </td><td>3909401           </td></tr>
	<tr><td>Comedy            </td><td>3541284           </td></tr>
	<tr><td>Action            </td><td>2560649           </td></tr>
	<tr><td>Thriller          </td><td>2325349           </td></tr>
	<tr><td>Adventure         </td><td>1908692           </td></tr>
	<tr><td>Romance           </td><td>1712232           </td></tr>
	<tr><td>Sci-Fi            </td><td>1341750           </td></tr>
	<tr><td>Crime             </td><td>1326917           </td></tr>
	<tr><td>Fantasy           </td><td> 925624           </td></tr>
	<tr><td>Children          </td><td> 737851           </td></tr>
	<tr><td>Horror            </td><td> 691407           </td></tr>
	<tr><td>Mystery           </td><td> 567865           </td></tr>
	<tr><td>War               </td><td> 511330           </td></tr>
	<tr><td>Animation         </td><td> 467220           </td></tr>
	<tr><td>Musical           </td><td> 432960           </td></tr>
	<tr><td>Western           </td><td> 189234           </td></tr>
	<tr><td>Film-Noir         </td><td> 118394           </td></tr>
	<tr><td>Documentary       </td><td>  93252           </td></tr>
	<tr><td>IMAX              </td><td>   8190           </td></tr>
	<tr><td>(no genres listed)</td><td>      6           </td></tr>
</tbody>
</table>




```R
#Movie ratings by Drama. str_detect Detect The Presence Or Absence Of A Pattern In A String.
drama <- edx %>% filter(str_detect(genres,"Drama"))
paste('Drama has',nrow(drama),'movies')
```

'Drama has 3909401 movies'

```R
#Movie ratings by Comedy
comedy <- edx %>% filter(str_detect(genres,"Comedy"))
paste('Comedy has',nrow(comedy),'movies')
```


'Comedy has 3541284 movies'



```R
##Movie ratings by Thriller
thriller <- edx %>% filter(str_detect(genres,"Thriller"))
paste('Thriller has',nrow(thriller),'movies')
```


'Thriller has 2325349 movies'



```R
#Movie ratings by Romance
romance <- edx %>% filter(str_detect(genres,"Romance"))
paste('Romance has',nrow(romance),'movies')
```


'Romance has 1712232 movies'



#  E. VARIABLE ANALYSIS BY RATING:


Find any insights to develop the recommendation model. The qualification is the classification of the information that allows it to be evaluated and valued based on a comparative evaluation of its standard quality or performance, quantity, or its combination. In the Movilens data set, the rating has a numerical ordinal scale of 0.5 to 5 stars from movie viewers. The maximum rating they give 5 stars or less if they do not like the movie.



# F. Which movie has the greatest number of ratings?


```R
edx %>% group_by(rating) %>% 
print(summarize(n=n()))
```


<table>
<thead><tr><th scope=col>rating</th><th scope=col>n</th></tr></thead>
<tbody>
	<tr><td>0.5    </td><td>  85420</td></tr>
	<tr><td>1.0    </td><td> 345935</td></tr>
	<tr><td>1.5    </td><td> 106379</td></tr>
	<tr><td>2.0    </td><td> 710998</td></tr>
	<tr><td>2.5    </td><td> 332783</td></tr>
	<tr><td>3.0    </td><td>2121638</td></tr>
	<tr><td>3.5    </td><td> 792037</td></tr>
	<tr><td>4.0    </td><td>2588021</td></tr>
	<tr><td>4.5    </td><td> 526309</td></tr>
	<tr><td>5.0    </td><td>1390541</td></tr>
</tbody>
</table>




```R
#Greatest number of ratings. Arrange rows by variables
edx %>% group_by(title) %>% 
summarise(number = n()) %>% 
print(arrange(desc(number)))
```


<table>
<thead><tr><th scope=col>title</th><th scope=col>number</th></tr></thead>
<tbody>
	<tr><td>Pulp Fiction (1994)                                                           </td><td>31336                                                                         </td></tr>
	<tr><td>Forrest Gump (1994)                                                           </td><td>31076                                                                         </td></tr>
	<tr><td>Silence of the Lambs, The (1991)                                              </td><td>30280                                                                         </td></tr>
	<tr><td>Jurassic Park (1993)                                                          </td><td>29291                                                                         </td></tr>
	<tr><td>Shawshank Redemption, The (1994)                                              </td><td>27988                                                                         </td></tr>
	<tr><td>Braveheart (1995)                                                             </td><td>26258                                                                         </td></tr>
	<tr><td>Terminator 2: Judgment Day (1991)                                             </td><td>26115                                                                         </td></tr>
	<tr><td>Fugitive, The (1993)                                                          </td><td>26050                                                                         </td></tr>
	<tr><td>Star Wars: Episode IV - A New Hope (a.k.a. Star Wars) (1977)                  </td><td>25809                                                                         </td></tr>
	<tr><td>Batman (1989)                                                                 </td><td>24343                                                                         </td></tr>
	<tr><td>Apollo 13 (1995)                                                              </td><td>24277                                                                         </td></tr>
	<tr><td>Toy Story (1995)                                                              </td><td>23826                                                                         </td></tr>
	<tr><td>Independence Day (a.k.a. ID4) (1996)                                          </td><td>23360                                                                         </td></tr>
	<tr><td>Dances with Wolves (1990)                                                     </td><td>23312                                                                         </td></tr>
	<tr><td>Schindler's List (1993)                                                       </td><td>23234                                                                         </td></tr>
	<tr><td>True Lies (1994)                                                              </td><td>22786                                                                         </td></tr>
	<tr><td>Star Wars: Episode VI - Return of the Jedi (1983)                             </td><td>22629                                                                         </td></tr>
	<tr><td>12 Monkeys (Twelve Monkeys) (1995)                                            </td><td>21959                                                                         </td></tr>
	<tr><td>Usual Suspects, The (1995)                                                    </td><td>21533                                                                         </td></tr>
	<tr><td>Speed (1994)                                                                  </td><td>21384                                                                         </td></tr>
	<tr><td>Fargo (1996)                                                                  </td><td>21370                                                                         </td></tr>
	<tr><td>Aladdin (1992)                                                                </td><td>21214                                                                         </td></tr>
	<tr><td>Matrix, The (1999)                                                            </td><td>20894                                                                         </td></tr>
	<tr><td>Star Wars: Episode V - The Empire Strikes Back (1980)                         </td><td>20836                                                                         </td></tr>
	<tr><td>Seven (a.k.a. Se7en) (1995)                                                   </td><td>20271                                                                         </td></tr>
	<tr><td>American Beauty (1999)                                                        </td><td>19859                                                                         </td></tr>
	<tr><td>Raiders of the Lost Ark (Indiana Jones and the Raiders of the Lost Ark) (1981)</td><td>19604                                                                         </td></tr>
	<tr><td>Back to the Future (1985)                                                     </td><td>19141                                                                         </td></tr>
	<tr><td>Mission: Impossible (1996)                                                    </td><td>18969                                                                         </td></tr>
	<tr><td>Ace Ventura: Pet Detective (1994)                                             </td><td>18907                                                                         </td></tr>
	<tr><td>...</td><td>...</td></tr>
	<tr><td>Nazis Strike, The (Why We Fight, 2) (1943)           </td><td>1                                                    </td></tr>
	<tr><td>Neil Young: Human Highway (1982)                     </td><td>1                                                    </td></tr>
	<tr><td>Once in the Life (2000)                              </td><td>1                                                    </td></tr>
	<tr><td>One Hour with You (1932)                             </td><td>1                                                    </td></tr>
	<tr><td>Part of the Weekend Never Dies (2008)                </td><td>1                                                    </td></tr>
	<tr><td>Please Vote for Me (2007)                            </td><td>1                                                    </td></tr>
	<tr><td>Prelude to War (Why We Fight, 1) (1943)              </td><td>1                                                    </td></tr>
	<tr><td>Prisoner of Paradise (2002)                          </td><td>1                                                    </td></tr>
	<tr><td>Quiet City (2007)                                    </td><td>1                                                    </td></tr>
	<tr><td>Relative Strangers (2006)                            </td><td>1                                                    </td></tr>
	<tr><td>Revenge of the Ninja (1983)                          </td><td>1                                                    </td></tr>
	<tr><td>Ring of Darkness (2004)                              </td><td>1                                                    </td></tr>
	<tr><td>Rockin' in the Rockies (1945)                        </td><td>1                                                    </td></tr>
	<tr><td>Säg att du älskar mig (2006)                         </td><td>1                                                    </td></tr>
	<tr><td>Shadows of Forgotten Ancestors (1964)                </td><td>1                                                    </td></tr>
	<tr><td>Splinter (2008)                                      </td><td>1                                                    </td></tr>
	<tr><td>Spooky House (2000)                                  </td><td>1                                                    </td></tr>
	<tr><td>Stacy's Knights (1982)                               </td><td>1                                                    </td></tr>
	<tr><td>Sun Alley (Sonnenallee) (1999)                       </td><td>1                                                    </td></tr>
	<tr><td>Symbiopsychotaxiplasm: Take One (1968)               </td><td>1                                                    </td></tr>
	<tr><td>Testament of Orpheus, The (Testament d'Orphée) (1960)</td><td>1                                                    </td></tr>
	<tr><td>Thérèse (2004)                                       </td><td>1                                                    </td></tr>
	<tr><td>Tokyo! (2008)                                        </td><td>1                                                    </td></tr>
	<tr><td>Train Ride to Hollywood (1978)                       </td><td>1                                                    </td></tr>
	<tr><td>Variety Lights (Luci del varietà) (1950)             </td><td>1                                                    </td></tr>
	<tr><td>Where A Good Man Goes (Joi gin a long) (1999)        </td><td>1                                                    </td></tr>
	<tr><td>Wings of Eagles, The (1957)                          </td><td>1                                                    </td></tr>
	<tr><td>Women of the Night (Yoru no onnatachi) (1948)        </td><td>1                                                    </td></tr>
	<tr><td>Won't Anybody Listen? (2000)                         </td><td>1                                                    </td></tr>
	<tr><td>Zona Zamfirova (2002)                                </td><td>1                                                    </td></tr>
</tbody>
</table>




```R
head(sort(-table(edx$rating)), 5)
hist(edx$rating)
summary(edx$rating)
```


    
           4        3        5      3.5        2 
    -2588021 -2121638 -1390541  -792037  -710998 



       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0.500   3.000   4.000   3.512   4.000   5.000 



![png](output_62_2.png)



```R
edx %>%  # Ratings Distribution:
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.15, color = "yellow") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Graphic Rating Distribution")
```

    Warning message:
    “Continuous limits supplied to discrete scale.
    Did you mean `limits = factor(...)` or `scale_*_continuous()`?”


![png](output_63_1.png)



# G. What are the five most given ratings in order from most to least?


```R
#Sort a variable in descending order.
edx %>% group_by(rating) %>% 
summarize(count = n()) %>% 
top_n(5) %>%
arrange(desc(count))
```

    Selecting by count



<table>
<thead><tr><th scope=col>rating</th><th scope=col>count</th></tr></thead>
<tbody>
	<tr><td>4.0    </td><td>2588021</td></tr>
	<tr><td>3.0    </td><td>2121638</td></tr>
	<tr><td>5.0    </td><td>1390541</td></tr>
	<tr><td>3.5    </td><td> 792037</td></tr>
	<tr><td>2.0    </td><td> 710998</td></tr>
</tbody>
</table>




# H. True or False: In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).


```R
#Rating movies
rating4 <- table(edx$rating)["4"]
rating35 <- table(edx$rating)["3.5"]
rating3 <- table(edx$rating)["3"]

Result <- (rating35 < rating3 && rating35 < rating4)

print(Result)

rm(rating35, rating3, rating4, Result)
```

    [1] TRUE


# 4.1. Graphic Rating movies


```R
#Graphic Rating movies
edx %>%
    group_by(rating) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = rating, y = count)) +
    geom_line()
```


![png](output_69_0.png)




# 4.2. Plot mean movie ratings given by users


```R
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
```

    Warning message:
    “Continuous limits supplied to discrete scale.
    Did you mean `limits = factor(...)` or `scale_*_continuous()`?”


![png](output_71_1.png)



```R
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
```


```R
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
```


```R
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
```


```R
#Size of dataset 
movielens <- left_join(ratings, movies, by = "movieId")
nrow(movielens)
```


10000054


# 4.3. Genres as Drama and Comedy have high rating.


```R
#Genres as Drama and Comedy have high rating.

ggplot(DAT.aggregate,aes(x=genres,y=n))+
    geom_col(group="genres",alpha=0.8,fill ="yellow", color="white")+
    geom_text(label=DAT.aggregate$n,angle=45,fontface = "bold")+
theme(axis.text.x=element_text(angle = -90, hjust = 0))
```


![png](output_77_0.png)



```R
#Movies from the 1980s to 1990s and older have higher average ratings than recent movies.

movielens$year <- as.numeric(substr(as.character(movielens$title),
                  nchar(as.character(movielens$title))-4,
                  nchar(as.character(movielens$title))-1))
plot(table(movielens$year),
    col = "yellow")
```


![png](output_78_0.png)


# 5. MODELING

Predicted movie ratings and calculates RMSE.

Movie rating predictions will be compared to the true ratings in the validation set using RMSE

# 5.1. Model Approach RMSE

Movilens is a very large database with different variables that have different effects on ratings. Genres have a significant effect on ratings, it is required to divide compound genres into individual genres and calculate the effect of each genre using relatively more complex calculations. Some movies have a very high number of ratings, while others have very few or low ratings. Coming from small samples -few numbers of grades- can adversely affect preaching. we will use a method known as Regularization to penalize  of very high or low grades that come from small samples. Also, we will divide the edx dataset into two parts: train (80%) and test (20%), then we will use train to train the model and test to cross-validate and fit the model to get the best lambda value that results in a minimum RMSE.

Created partition the data set Edx into 20% for test and 80% for training set, this step prepared the data split to create the model.


```R
#Validation set will be 20% of the movieLens data
set.seed(2) #Partition dataset test and train
test_index <- createDataPartition(y = edx$rating, times = 2, p = 0.1, list = FALSE)
train <- edx[-test_index,] #train set
test <- edx[test_index,]   #test set
```


```R
#Define RMSE that mesure of how spread out thesse residuals are, or concentration the data around the linea of best fit.

RMSE <- function(true_ratings, predicted_ratings){
sqrt(mean((true_ratings - predicted_ratings)^2,na.rm =TRUE))
}
```


```R
mu <- mean(train$rating)  #mean

rmse_naive <- RMSE(test$rating, mu)  #MODEL 1

rmse_results <- data_frame(method='Average NAIVE', RMSE=rmse_naive)
rmse_results
```


<table>
<thead><tr><th scope=col>method</th><th scope=col>RMSE</th></tr></thead>
<tbody>
	<tr><td>Average NAIVE</td><td>1.060381     </td></tr>
</tbody>
</table>




```R
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
```


<table>
<thead><tr><th scope=col>method</th><th scope=col>RMSE</th></tr></thead>
<tbody>
	<tr><td>Average NAIVE     </td><td>1.0603807         </td></tr>
	<tr><td>Movie Effect Model</td><td>0.9435103         </td></tr>
</tbody>
</table>




```R
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
```


<table>
<thead><tr><th scope=col>method</th><th scope=col>RMSE</th></tr></thead>
<tbody>
	<tr><td>Average NAIVE            </td><td>1.0603807                </td></tr>
	<tr><td>Movie Effect Model       </td><td>0.9435103                </td></tr>
	<tr><td>Movie + User Effect Model</td><td>0.8660346                </td></tr>
</tbody>
</table>



# 5.2. RMSE USED VALIDATION SET


```R
rating_vp <- validation %>%
  left_join(movie_avgs, by = 'movieId')%>%
  left_join(user_avgs, by='userId')%>%
  mutate(pred=mu_m2 + b_i + b_u)%>%
  pull(pred)

validation_m3 <- RMSE(validation$rating, rating_vp)
rmse_results <- bind_rows(rmse_results, data_frame(Method='Validation', RMSE=validation_m3))
rmse_results
```


<table>
<thead><tr><th scope=col>method</th><th scope=col>RMSE</th><th scope=col>Method</th></tr></thead>
<tbody>
	<tr><td>Average NAIVE            </td><td>1.0603807                </td><td>NA                       </td></tr>
	<tr><td>Movie Effect Model       </td><td>0.9435103                </td><td>NA                       </td></tr>
	<tr><td>Movie + User Effect Model</td><td>0.8660346                </td><td>NA                       </td></tr>
	<tr><td>NA                       </td><td>      NaN                </td><td>Validation               </td></tr>
</tbody>
</table>




```R
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
```

## 5.3. RMSE PLOT

```R
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
```


![png](output_92_0.png)



```R
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
```


![png](output_93_0.png)



```R
library(lubridate)

tibble(`Initial Date` = date(as_datetime(min(edx$timestamp), origin="1980-01-01")),
       `Final Date` = date(as_datetime(max(edx$timestamp), origin="1980-01-01"))) %>%
  mutate(Period = duration(max(edx$timestamp)-min(edx$timestamp)))
```

    
    Attaching package: ‘lubridate’
    
    The following objects are masked from ‘package:base’:
    
        date, intersect, setdiff, union
    



<table>
<thead><tr><th scope=col>Initial Date</th><th scope=col>Final Date</th><th scope=col>Period</th></tr></thead>
<tbody>
	<tr><td>2005-01-08               </td><td>2019-01-05               </td><td>441479128s (~13.99 years)</td></tr>
</tbody>
</table>




```R
edx %>% mutate(date = date(as_datetime(timestamp, origin="1990-01-01"))) %>%
  group_by(date, title) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  head(25)
```

    `summarise()` has grouped output by 'date'. You can override using the `.groups` argument.



<table>
<thead><tr><th scope=col>date</th><th scope=col>title</th><th scope=col>count</th></tr></thead>
<tbody>
	<tr><td>2018-05-22                                                             </td><td>Chasing Amy                                                            </td><td>266                                                                    </td></tr>
	<tr><td>2020-11-20                                                             </td><td>American Beauty                                                        </td><td>215                                                                    </td></tr>
	<tr><td>2019-12-11                                                             </td><td>Star Wars: Episode IV - A New Hope (a.k.a. Star Wars)                  </td><td>202                                                                    </td></tr>
	<tr><td>2019-12-11                                                             </td><td>Star Wars: Episode V - The Empire Strikes Back                         </td><td>200                                                                    </td></tr>
	<tr><td>2025-03-22                                                             </td><td>Lord of the Rings: The Two Towers, The                                 </td><td>197                                                                    </td></tr>
	<tr><td>2019-12-11                                                             </td><td>Star Wars: Episode VI - Return of the Jedi                             </td><td>189                                                                    </td></tr>
	<tr><td>2025-03-22                                                             </td><td>Lord of the Rings: The Fellowship of the Ring, The                     </td><td>177                                                                    </td></tr>
	<tr><td>2020-11-20                                                             </td><td>Jurassic Park                                                          </td><td>173                                                                    </td></tr>
	<tr><td>2020-11-20                                                             </td><td>Terminator 2: Judgment Day                                             </td><td>170                                                                    </td></tr>
	<tr><td>2019-12-11                                                             </td><td>Matrix, The                                                            </td><td>169                                                                    </td></tr>
	<tr><td>2020-11-20                                                             </td><td>Men in Black                                                           </td><td>167                                                                    </td></tr>
	<tr><td>2025-03-22                                                             </td><td>Shrek                                                                  </td><td>158                                                                    </td></tr>
	<tr><td>2020-11-20                                                             </td><td>Star Wars: Episode IV - A New Hope (a.k.a. Star Wars)                  </td><td>157                                                                    </td></tr>
	<tr><td>2020-11-20                                                             </td><td>Star Wars: Episode V - The Empire Strikes Back                         </td><td>154                                                                    </td></tr>
	<tr><td>2020-11-20                                                             </td><td>Matrix, The                                                            </td><td>151                                                                    </td></tr>
	<tr><td>2019-12-11                                                             </td><td>Raiders of the Lost Ark (Indiana Jones and the Raiders of the Lost Ark)</td><td>150                                                                    </td></tr>
	<tr><td>2020-11-20                                                             </td><td>Saving Private Ryan                                                    </td><td>150                                                                    </td></tr>
	<tr><td>2020-11-20                                                             </td><td>Star Wars: Episode VI - Return of the Jedi                             </td><td>150                                                                    </td></tr>
	<tr><td>2019-12-11                                                             </td><td>Saving Private Ryan                                                    </td><td>146                                                                    </td></tr>
	<tr><td>2019-12-11                                                             </td><td>E.T. the Extra-Terrestrial                                             </td><td>145                                                                    </td></tr>
	<tr><td>2019-12-11                                                             </td><td>Godfather, The                                                         </td><td>145                                                                    </td></tr>
	<tr><td>2025-03-22                                                             </td><td>Fight Club                                                             </td><td>145                                                                    </td></tr>
	<tr><td>2025-03-22                                                             </td><td>Matrix, The                                                            </td><td>143                                                                    </td></tr>
	<tr><td>2020-11-20                                                             </td><td>Braveheart                                                             </td><td>142                                                                    </td></tr>
	<tr><td>2025-03-22                                                             </td><td>Shawshank Redemption, The                                              </td><td>140                                                                    </td></tr>
</tbody>
</table>



# 6. Data Analysis:

    We will verify the division or split of the database, and thus compare with two graphs that will show us a better classification of the different features


```R
questions <- c("Movies are in the edx dataset.",
                "Genres are in the edx dataset.",
                "Titles are in the edx dataset.",
                "Users are in the edx dataset.",
                "Rating of movies per users."
)
values_edx <- c(round(n_distinct(edx$movieId),0),
            round(n_distinct(edx$genres),0),
            round(n_distinct(edx$title),0),
            round(n_distinct(edx$userId),0),
            round(n_distinct(edx$movieId)*n_distinct(edx$userId)/dim(edx)[1] ,2)
)
values_validation <- c(round(n_distinct(validation$movieId),0),
                    round(n_distinct(validation$genres),0),
                    round(n_distinct(validation$title),0),
                    round(n_distinct(validation$userId),0),
                    round(n_distinct(validation$movieId)*n_distinct(validation$userId)/dim(edx)[1] ,2)
)
train_val <- data.frame(questions = questions, edx=values_edx, validation = values_validation )
train_val

```


<table>
<thead><tr><th scope=col>questions</th><th scope=col>edx</th><th scope=col>validation</th></tr></thead>
<tbody>
	<tr><td>Movies are in the edx dataset.</td><td>10649.00                                </td><td>10169.00                                         </td></tr>
	<tr><td>Genres are in the edx dataset.</td><td>  796.00                                </td><td>  786.00                                         </td></tr>
	<tr><td>Titles are in the edx dataset.</td><td>10380.00                                </td><td>10168.00                                         </td></tr>
	<tr><td>Users are in the edx dataset. </td><td>69878.00                                </td><td>69692.00                                         </td></tr>
	<tr><td>Rating of movies per users.</td><td>  102.07                                   </td><td>   97.21                                         </td></tr>
</tbody>
</table>



# 6.1 PLOT Genres and Times Rating

```R
edx <- edx %>% separate_rows(genres,sep = "\\|") %>% mutate(value=1)
        genres_rating <- edx %>% group_by(genres) %>% summarise(n=n())
        genres_rating <- genres_rating[order(-genres_rating$n),]
        ggplot(genres_rating, aes(x = n, y =reorder(genres, n),fill=genres)) +
        geom_bar(stat = "identity") +
        theme(legend.position = "none")+
        labs(title="Bar plot for Genres ratings", x="Count", y="Genres ratings")
```


![png](output_99_0.png)


<p style='text-align: justify;'> 

We observe prevalence of genres, Drama continuing to be the most rated, and IMAX is the lowest, and give a correct cleaned the data deleted Nan, this model confirmed when before we agroup by genres.
</p>


```R
edx %>% count(movieId) %>% ggplot(aes(n))+
        geom_histogram(color = "black" , fill= "green")+
        scale_x_log10()+
        labs( x = "log10 of count movieID")+
        ggtitle("Rating times Per Movie")+
        theme_gray()
```

    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.



![png](output_101_1.png)



```R
install.packages("xtable", repos = "http://cran.us.r-project.org")
library(xtable)
```

# 6.2. Linear Regression Model Summary


```R
model <- lm(rating ~ userId + movieId, data = edx)
result <- predict(model,validation)

lm_rmse <- RMSE(validation$rating, result)

model %>%
summary() %>%
xtable() 
```


<table>
<thead><tr><th></th><th scope=col>Estimate</th><th scope=col>Std. Error</th><th scope=col>t value</th><th scope=col>Pr(&gt;|t|)</th></tr></thead>
<tbody>
	<tr><th scope=row>(Intercept)</th><td> 3.525399e+00</td><td>4.981499e-04 </td><td>7076.98374   </td><td> 0.000000e+00</td></tr>
	<tr><th scope=row>userId</th><td> 1.326330e-07</td><td>1.174836e-08 </td><td>  11.28950   </td><td> 1.479117e-29</td></tr>
	<tr><th scope=row>movieId</th><td>-7.617538e-07</td><td>2.592977e-08 </td><td> -29.37758   </td><td>1.072640e-189</td></tr>
</tbody>
</table>



Histograms log10 of movieId looks like a gaussian distribution. From histograms, we observe the effects over Rating times by age at rate, 
userID and movieID, which are assimilated to some type of distribution, which allows us to approach an prediction by mean value.

The Model Linear Regression give us the same result of RMSE, with positive correlation that mean variables in which both variables move in the same direction.


```R
library(Hmisc)
```


```R
#Now we can see the correlationn in this 3 features, 
edx_cor <- select(edx,c(rating,userId,movieId))
res_corr <- rcorr(as.matrix(edx_cor))
print(res_corr)
```

            rating userId movieId
    rating    1.00      0   -0.01
    userId    0.00      1    0.00
    movieId  -0.01      0    1.00
    
    n= 18931540 
    
    
    P
            rating userId movieId
    rating          0      0     
    userId   0             0     
    movieId  0      0            


# CONCLUSION

<p style='text-align: justify;'>

With the different analyzes we can see how we discover the tastes of the spectators, as the films with the highest rating are not necessarily the most viewed, likewise it leads us to discover the taste for films of past decades over the most avant-garde ones.


RMSE, root-mean-square deviation, model that was used to predict from a list of rated movies, and discovers patterns. It was determined which movies viewers prefer, with a medium to high rating. (3 to 4). The films preferred by the clients were those produced from the periods 1920 and 2000 with a maximum rating of 4. The model yielded an accuracy of 86%, highlighting that the larger the sample size increases the accuracy or precision of the model and vice versa.

</p>
