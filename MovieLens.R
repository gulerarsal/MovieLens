####################################################################################################################

# MovieLens Project
#
# For the Requirement of *HarvardX: PH125.9x Data Science: Capstone Course
# 
# Aauthor: Guler Arsal
#
####################################################################################################################
# # The following packages are required.
####################################################################################################################
if(!require(knitr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(lubridicate)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

library(knitr)
library(kableExtra)
library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(stringr)
library(scales)
library(forcats)


####################################################################################################################
# Movielens Dataset
####################################################################################################################

# Download movielens dataset
options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Remove unnecessary objects from the environment
rm(movies, ratings, dl, movies_file, ratings_file)


####################################################################################################################
# Splitting Datasets
####################################################################################################################

# Create edx and final_holdout_test sets 
# Final hold-out test set will be 10% of MovieLens data
# set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)



# Split the edx data into a training set & test set 
# Assign 20% of the ratings made by each user to the test set
# set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in edx test set are also in edx train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from final edx test set back into edx train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# Remove unnecessary objects from the environment
rm(removed, temp, test_index, movielens)


####################################################################################################################
# Exploratory Analysis
####################################################################################################################
# Movie Ratings (Outcome Variable)
####################################################################################################################
# Create a Table for Number of Ratings for Possible Rating Options
edx %>% 
  group_by(rating) %>% 
  summarise(n = comma(n())) %>%
  kbl(
    # format = 'latex', 
    # booktabs = TRUE, 
    col.names = c("Rating Options", "Number of Ratings"),
    align = c("c", "r"),
    caption = "Number of Ratings for Possible Rating Options") %>% 
  row_spec(0, bold = TRUE, align = "c") %>%
  row_spec(1, color="blue") %>%  
  row_spec(8, color="red") %>%   
  kable_styling(
    latex_options = c("striped", "hold_position", "repeat_header"),
    position = "left",
    stripe_color = "gray!15",
    font_size = 11,
    full_width = FALSE
  )


####################################################################################################################
# Users
####################################################################################################################
# Count Number of Ratings Per User
user_rating <- edx %>% 
  group_by(userId) %>% 
  summarise(n = n())

# Calculate Median and Mean of Ratings Per User
user_rating_median <- median(user_rating$n)
user_rating_mean <- round(mean(user_rating$n),0)

# Count Users with More Than 1000 Ratings
users_over_1000 <- user_rating %>% 
  filter(n > 1000) %>% 
  nrow()

# Plot Histogram of Number of Ratings Per User
user_rating  %>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, fill = "#FFDAB9", color = "black") +
  geom_vline(aes(xintercept=user_rating_median), color="blue", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=user_rating_mean), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=1000), color="darkgreen", linetype="dashed", linewidth=0.6) +
  scale_x_log10(n.breaks = 7) +
  xlab("\nNumber of Ratings (Log Scale)") +
  ylab("Number of Users\n")  +
  annotate("text", x = median(user_rating$n), y = 7000, label = "Median", color = "blue", vjust = 0, hjust = 1.1, size = 2.8) +
  annotate("text", x = mean(user_rating$n), y = 7000, label = "Mean", color = "red", vjust = 0, hjust = -0.1, size = 2.8) +
  annotate("text", x = 1000, y = 4000, label = "over 1,000", color = "darkgreen", vjust = 0, hjust = -0.1, size = 2.8)

# Calculate Average Ratings Per User 
average_rating <- edx %>% 
  group_by(userId) %>%
  summarise(avg_rat = round(sum(rating)/n(),2))

average_rating_mean <- round(mean(average_rating$avg_rat),2)

# Plot Histogram of Average Ratings Per User
edx %>%
  group_by(userId) %>%
  summarise(avg_rat = sum(rating)/n()) %>%
  ggplot(aes(avg_rat)) + 
  geom_histogram(bins = 30, fill = "#FFDAB9", color = "black") +
  geom_vline(aes(xintercept=mean(avg_rat)),           
             color="red", linetype="dashed", linewidth=1) + 
  xlab("\nAverage Rating") +
  ylab("Number of Users\n") +
  annotate("text", x = user_average_rating_mean, y = 10500, label = "Mean", color = "red", vjust = 0, hjust = 1.5, size = 3) 

# Remove unnecessary objects from the environment
rm(user_rating, user_average_rating, user_average_rating_mean, user_rating_median, users_over_1000)


####################################################################################################################
# Movies
####################################################################################################################
# Count Number of Ratings Per Movie
movie_rating <- edx %>% 
  group_by(movieId) %>% 
  summarise(n = n())

# Calculate Mean and Median of Ratings Per Movie
movie_rating_mean <- round(mean(movie_rating$n),0)
movie_rating_median <- median(movie_rating$n)

# Count Number of Movies with Only One Rating
movies_rated_once <- movie_rating %>%
  filter(n == 1) %>% 
  nrow()

# Plot Histogram of Number of Ratings Per Movie
movie_rating %>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, fill = "#FFDAB9", color = "black") +
  geom_vline(aes(xintercept=mean(n)),
             color="red", linetype="dashed", linewidth=1) + 
  geom_vline(aes(xintercept=median(n)),
             color="blue", linetype="dashed", linewidth=1) + 
  scale_x_log10(n.breaks = 7) +
  xlab("\nNumber of Ratings (Log Scale)") +
  ylab("Number of Movies\n") +
  annotate("text", x = movie_rating_median, y = 750, label = "Median", color = "blue", vjust = 0, hjust = 1.1, size = 3) +
  annotate("text", x = movie_rating_median, y = 750, label = "Mean", color = "red", vjust = 0, hjust = -0.8, size = 3) 

# Calculate Average Rating Per Movie 
movie_average_rating <- edx %>% 
  group_by(movieId) %>%
  summarise(avg_rat = round(sum(rating)/n(),2))

# Calculate Mean and Median of Average Ratings Per Movie
movie_average_rating_mean <- round(mean(movie_average_rating$avg_rat),2)
movie_average_rating_median <- round(median(movie_average_rating$avg_rat),2)

# Plot Histogram of Average Rating Per Movie
edx %>%
  group_by(movieId) %>%
  summarise(avg_rat = sum(rating)/n()) %>%
  ggplot(aes(avg_rat)) + 
  geom_histogram(bins = 30, fill = "#FFDAB9", color = "black") +
  geom_vline(aes(xintercept=mean(avg_rat)),           
             color="red", linetype="dashed", linewidth=1) + 
  geom_vline(aes(xintercept=median(avg_rat)),           
             color="blue", linetype="dashed", linewidth=1) + 
  xlab("\nAverage Rating") +
  ylab("Number of Movies\n") 
  annotate("text", x = movie_average_rating_mean, y = 1300, label = "Mean", color = "red", vjust = 0, hjust = 1.3, size = 3)  +
  annotate("text", x = movie_average_rating_median, y = 1300, label = "Median", color = "blue", vjust = 0, hjust = -0.2, size = 3) 

# Remove unnecessary objects from the environment
rm(movie_rating, movie_average_rating, movie_average_rating_mean, movie_rating_median, movies_rated_once)


####################################################################################################################
# Review Date
####################################################################################################################
# Create New Two Columns Based on timestamp Column
edx <- edx %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"),
                      review_year = format(review_date, "%Y"))

# Plot Distribution of Number of Ratings by Year Reviewed
edx  %>%
  ggplot(aes(review_year)) +
  geom_bar(fill = "#FFDAB9", color = "black") +
  xlab("\nReview Year") +
  ylab("Number of Ratings\n")

# Plot Distribution of Average Ratings by Week Reviewed
edx %>% 
  group_by(review_date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(review_date, rating)) +
  geom_point() +
  geom_smooth() +
  xlab("\nReview Date") +
  ylab("Average Ratings\n") 

# Plot Distribution of Average of Ratings by Year Reviewed
edx %>% 
  ggplot(aes(rating, review_year)) +
  geom_boxplot() +
  xlab("Average Ratings\n") +
  ylab("\nReview Year") + 
  theme(axis.title.y = element_text(margin = margin(r = 20)))

# Create a categorical variable to distinguish between ratings before and after 2003
edx <- edx %>% mutate(year2003 = factor(if_else(review_year < 2003, "Before 2003", "2003 and After"),
                                        levels = c("Before 2003", "2003 and After"))) 

# Create a Plot showing Number of Ratings for Possible Rating Options Before and After 2003
edx %>%
  group_by(rating, year2003) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = factor(rating), y = n, fill = year2003)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(labels = comma) +
  labs(
    x = "Rating Options\n",
    y = "\nNumber of Ratings",
    fill = "Review Year") +
  facet_wrap(~ year2003) +
  scale_fill_manual(
    values = c("Before 2003" = "#5DADE2", "2003 and After" = "#EC7063")  # Customize the colors
  ) +
  theme(legend.position = "none" )


####################################################################################################################
# Processing Title Column
####################################################################################################################
# clean The *title* Column to Extract Release Year and Adjust Title Formatting
edx <- edx %>%
  # Trim title
  mutate(title = str_trim(title)) %>% 
  # Extract year and temporary title
  extract(title, c("title_temp", "release_year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  # Convert release_year by taking the first year
  mutate(release_year = as.integer(if_else(str_length(release_year) > 4, str_split(release_year, "-", simplify = TRUE)[1], release_year)),
         title = if_else(is.na(title_temp), title, title_temp)) %>%
  # Drop temporary title
  select(-title_temp)


####################################################################################################################
# Movie Title
####################################################################################################################
# Create a Table for Top 10 Movies Receiving the Most Ratings
edx %>% 
  group_by(title) %>%
  summarize(average_rat = round(mean(rating), 2), 
            count = n()) %>%
  arrange(desc(count)) %>% 
  head(10) %>% 
  mutate(rank = row_number(),
         count = comma(count)) %>% 
  select(rank, title, count, average_rat) %>%
  kbl(
      # format = 'latex',       
      # booktabs = TRUE, 
      linesep = "\\addlinespace",
      col.names = c("Rank", "Title", "Frequency", "Average"),
      align = c("c", "l", "c", "c"),   
      caption = "Top 10 Movies Receiving the Most Ratings") %>%
  row_spec(0, bold = TRUE, align = c("c", "l", "c", "c")) %>%
  column_spec(1, width = "1cm") %>%  
  column_spec(2, width = "10cm") %>%  
  column_spec(3, width = "2cm") %>% 
  column_spec(4, width = "2cm") %>% 
  kable_styling(
      latex_options = c("striped", "HOLD_position"),
      position = "left",
      stripe_color = "gray!15",
      font_size = 11,
      full_width = FALSE)


# Create a Table for Top 10 Movies Receiving the Highest Ratings
edx %>% 
  group_by(title) %>%
  summarize(average_rat = round(mean(rating), 2), 
            count = n()) %>%
  arrange(desc(average_rat)) %>% 
  head(10) %>% 
  mutate(rank = row_number(),
         count = comma(count)) %>% 
  select(rank, title, average_rat, count) %>%
  kbl(
      # format = 'latex',       
      # booktabs = TRUE, 
      linesep = "\\addlinespace",
      col.names = c("Rank", "Title", "Average", "Frequency"),
      align = c("c", "l", "c", "c"),   
      caption = "Top 10 Movies Receiving the Highest Ratings") %>%
  row_spec(0, bold = TRUE, align = c("c", "l", "c", "c")) %>%
  column_spec(1, width = "1cm") %>%  
  column_spec(2, width = "10cm") %>%  
  column_spec(3, width = "2cm") %>% 
  column_spec(4, width = "2cm") %>% 
  kable_styling(
      latex_options = c("striped", "HOLD_position"),
      position = "left",
      stripe_color = "gray!15",
      font_size = 11,
      full_width = FALSE) %>%
  footnote(general = "The small sample sizes make these ratings unreliable", 
           general_title = "Note: ", 
           footnote_as_chunk = TRUE, 
           escape = FALSE)


####################################################################################################################
# Release Year
####################################################################################################################
# Plot Average Ratings by Release Year
edx %>% 
  group_by(release_year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(release_year, rating)) +
  geom_point() +
  geom_smooth() +
  xlab("\nRelease Year") +
  ylab("Average Ratings\n") 


####################################################################################################################
# Movie Genres
####################################################################################################################
# Create Distinct Genre Categories
edx_genres <- edx %>% separate_rows(genres, sep = "\\|") # Takes very long time to run!

# Summarize the data by genres
genre_summary <- edx_genres %>%
  group_by(genres) %>%
  summarise(`Number of Ratings` = n(),
            `Number of Movies` = comma(n_distinct(movieId)),
            `Average Rating` = round(mean(rating),2)) %>%
  mutate(`Number of Ratings` = format(`Number of Ratings`, big.mark = ',')) %>%
  arrange(desc(`Number of Ratings`)) %>%
  mutate(Rank = row_number(), 
         Genres = genres) %>%
  select(Rank, Genres,`Number of Ratings`, `Average Rating`, `Number of Movies`) 

# Create a Table for Genres 
genre_summary %>%  
  kbl(
      # format = 'latex',       
      # booktabs = TRUE, 
      linesep = "\\addlinespace",
      align = 'clrrr', 
      caption = "Genres Ranked Based on Number of Ratings Received") %>%
  row_spec(0, bold = TRUE, align = "l") %>%
  kable_styling(
      latex_options = c("striped", "HOLD_position"),
      position = "left", 
      stripe_color = "gray!10", 
      font_size = 11, 
      full_width = FALSE) 

# Calculate Mean for Horror Movies
mean_horror_rating <- edx_genres %>%
  filter(str_detect(genres, "Horror")) %>%  
  summarise(mean_rating = mean(rating, na.rm = TRUE)) %>% 
  pull(mean_rating) %>%  
  round(2) 

# Calculate Median for Horror Movies
median_horror_rating <- edx_genres %>%
  filter(str_detect(genres, "Horror")) %>%  
  summarise(median_rating = median(rating, na.rm = TRUE)) %>% 
  pull(median_rating) %>%  
  round(2) 

# Calculate Mean for Film_Noir Movies
mean_filmnoir_rating <- edx_genres %>%
  filter(str_detect(genres, "Film-Noir")) %>%  
  summarise(mean_rating = mean(rating, na.rm = TRUE)) %>% 
  pull(mean_rating) %>%  
  round(2) 

# Calculate Median for Horror Movies
median_filmnoir_rating <- edx_genres %>%
  filter(str_detect(genres, "Film-Noir")) %>%  
  summarise(median_rating = median(rating, na.rm = TRUE)) %>% 
  pull(median_rating) %>%  
  round(2) 

# Create Boxplots of Genre Ratings (Ranked by Mean Rating)
edx_genres %>%
  group_by(genres) %>%
  mutate(mean_rating = mean(rating)) %>%  
  ggplot(aes(x = rating, y = fct_reorder(genres, mean_rating))) + 
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 1.5) +
  xlab("Average Rating\n") +
  ylab("\nGenres") 

# Remove unnecessary objects from the environment
rm(genre_summary, mean_horror_rating, median_horror_rating, mean_filmnoir_rating, median_filmnoir_rating)



####################################################################################################################
# Model Development  
####################################################################################################################
# Preparation for Model Development
####################################################################################################################

# Update Test Set in Line with Changes Made to edx Dataset
# Create New Two Columns Based on timestamp Column
test_set <- test_set %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"),
                                review_year = format(review_date, "%Y")) 

# clean The *title* Column to Extract Release Year and Adjust Title Formatting
test_set <- test_set %>% 
  # Trim title
  mutate(title = str_trim(title)) %>% 
  # Extract year and temporary title
  extract(title, c("title_temp", "release_year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  # Convert release_year by taking the first year
  mutate(release_year = as.integer(if_else(str_length(release_year) > 4, str_split(release_year, "-", simplify = TRUE)[1], release_year)),
         title = if_else(is.na(title_temp), title, title_temp)) %>%
  # Drop temporary title
  select(-title_temp)

# Create Distinct Genre Categories
test_set_genres <- test_set %>% separate_rows(genres, sep = "\\|") # Takes very long time to run!



# Update Train Set in Line with Changes Made to edx Dataset
# Create New Two Columns Based on timestamp Column
train_set <- train_set %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"),
                                  review_year = format(review_date, "%Y")) 

# clean The *title* Column to Extract Release Year and Adjust Title Formatting
train_set <- train_set %>% 
  # Trim title
  mutate(title = str_trim(title)) %>% 
  # Extract year and temporary title
  extract(title, c("title_temp", "release_year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  # Convert release_year by taking the first year
  mutate(release_year = as.integer(if_else(str_length(release_year) > 4, str_split(release_year, "-", simplify = TRUE)[1], release_year)),
         title = if_else(is.na(title_temp), title, title_temp)) %>%
  # Drop temporary title
  select(-title_temp)

# Create Distinct Genre Categories
train_set_genres <- train_set %>% separate_rows(genres, sep = "\\|") # Takes very long time to run!


####################################################################################################################
# Initial Models (Non-Regularized)
####################################################################################################################
# Model 1: Average Rating (Baseline)
####################################################################################################################
# Mean of Observed Values
mu <- mean(train_set$rating)
# Calculate RMSE
rmse_m1 <- RMSE(test_set$rating, mu)

# Define a Clamp Function
clamp <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

# Calculate RMSE again with a lower limit of 0.5 and an upper limit of 5
rmse_m1_clamp <- RMSE(clamp(test_set$rating, lower = 0.5, upper = 5), mu)


####################################################################################################################
# Model 2: Movie Effect Model
####################################################################################################################
# Calculate Movie Effect (b_m)
movie_averages <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_m = mean(rating - mu))

# Predict Ratings 
predict_b_m <- mu + test_set %>% 
  left_join(movie_averages, by = "movieId") %>% 
  .$b_m

# Calculate RMSE  
rmse_m2 <- RMSE(test_set$rating, predict_b_m)

# Calculate RMSE again with a lower limit of 0.5 and an upper limit of 5
rmse_m2_clamp <- RMSE(clamp(predict_b_m, lower = 0.5, upper = 5), test_set$rating)


####################################################################################################################
# Model 3: Movie + User Effects Model
####################################################################################################################
# Calculate User Effect (b_u) Adjusted for Movie Effect
user_averages <- train_set %>% 
  left_join(movie_averages, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_m))

# Predict Ratings
predict_b_u <- test_set %>% 
  left_join(movie_averages, by = "movieId") %>% 
  left_join(user_averages, by = "userId") %>%  
  mutate (prediction = mu + b_m + b_u) %>% 
  .$prediction

# Calculate RMSE  
rmse_m3 <- RMSE(test_set$rating, predict_b_u)

# Calculate RMSE again with a lower limit of 0.5 and an upper limit of 5
rmse_m3_clamp <- RMSE(clamp(predict_b_u, lower = 0.5, upper = 5), test_set$rating)


####################################################################################################################
# Model 4a: Movie + User + Genre Effects Model
####################################################################################################################
# Calculate Genre Effect (b_g) Adjusted for Movie and User Effects
genre_averages <- train_set %>% 
  left_join(movie_averages, by = "movieId") %>% 
  left_join(user_averages, by = "userId") %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_m - b_u))

# Predict Ratings
predict_b_g <- test_set %>% 
  left_join(movie_averages, by = "movieId") %>% 
  left_join(user_averages, by = "userId") %>% 
  left_join(genre_averages, by = "genres") %>%   
  mutate (prediction = mu + b_u + b_m + b_g) %>% 
  .$prediction

# Calculate RMSE  
rmse_m4a <- RMSE(test_set$rating, predict_b_g)

# Calculate RMSE again with a lower limit of 0.5 and an upper limit of 5
rmse_m4a_clamp <- RMSE(clamp(predict_b_g, lower = 0.5, upper = 5), test_set$rating)


####################################################################################################################
# Model 4b: Movie + User + Genre-Specific Effects Model
####################################################################################################################
# Movie (b_m) and User (b_u) and Genre (b_g) Effects
genre_averages <- train_set_genres %>% 
  left_join(movie_averages, by = "movieId") %>% 
  left_join(user_averages, by = "userId") %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_m - b_u))

# Predict Ratings
predict_b_g <- test_set_genres %>% 
  left_join(movie_averages, by = "movieId") %>% 
  left_join(user_averages, by = "userId") %>% 
  left_join(genre_averages, by = "genres") %>%   
  mutate (prediction = mu + b_u + b_m + b_g) %>% 
  .$prediction

# Calculate RMSE  
rmse_m4b <- RMSE(test_set_genres$rating, predict_b_g)

# Calculate RMSE again with a lower limit of 0.5 and an upper limit of 5
rmse_m4b_clamp <- RMSE(clamp(predict_b_g, lower = 0.5, upper = 5), test_set_genres$rating)


####################################################################################################################
# Model 5: Movie + User + Genre-Specific + Release Year Effects Model
####################################################################################################################
# Calculate Release Year Effect (b_ry) Adjusted for Movie, User, and Genre-Specific Effects
release_year_averages <- train_set_genres %>% 
  left_join(movie_averages, by = "movieId") %>% 
  left_join(user_averages, by = "userId") %>%
  left_join(genre_averages, by = "genres") %>%
  group_by(release_year) %>% 
  summarize(b_ry = mean(rating - mu - b_m - b_u - b_g))

# Predict ratings adjusting for movie, user, genre-specific, and release_year effects
predict_b_ry <- test_set_genres %>% 
  left_join(movie_averages, by = "movieId") %>% 
  left_join(user_averages, by = "userId") %>% 
  left_join(genre_averages, by = "genres") %>%
  left_join(release_year_averages, by = "release_year") %>%   
  mutate(prediction = mu + b_m + b_u + b_g + b_ry) %>% 
  .$prediction

# Calculate RMSE  
rmse_m5 <- RMSE(test_set_genres$rating, predict_b_ry)

# Calculate RMSE again with a lower limit of 0.5 and an upper limit of 5
rmse_m5_clamp <- RMSE(clamp(predict_b_ry, lower = 0.5, upper = 5), test_set_genres$rating)


####################################################################################################################
# Model 6: Movie + User + Genre-Specific + Release Year + Review Date Effects Model
####################################################################################################################
# Calculate Review Date Effect (b_rd) Adjusted for Movie, User, Genre-Specific, and Release Year Effects
review_date_averages <- train_set_genres %>% 
  left_join(movie_averages, by = "movieId") %>% 
  left_join(user_averages, by = "userId") %>%
  left_join(genre_averages, by = "genres") %>%
  left_join(release_year_averages, by = "release_year") %>%
  group_by(review_date) %>% 
  summarize(b_rd = mean(rating - mu - b_m - b_u - b_g - b_ry))

# Predict ratings adjusting for movie, user, genre-specific, release_year, and review_date effects
predict_b_rd <- test_set_genres %>% 
  left_join(movie_averages, by = "movieId") %>% 
  left_join(user_averages, by = "userId") %>% 
  left_join(genre_averages, by = "genres") %>%
  left_join(release_year_averages, by = "release_year") %>%
  left_join(review_date_averages, by = "review_date") %>%   
  mutate(prediction = mu + b_m + b_u + b_g + b_ry + b_rd) %>% 
  .$prediction

# Calculate RMSE  
rmse_m6 <- RMSE(test_set_genres$rating, predict_b_rd)

# Calculate RMSE again with a lower limit of 0.5 and an upper limit of 5
rmse_m6_clamp <- RMSE(clamp(predict_b_rd, lower = 0.5, upper = 5), test_set_genres$rating)


####################################################################################################################
# Summary of the Initial Models
####################################################################################################################

# Create a data frame to store the results for Models 1-6
rmse_results <- data_frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4a", "Model 4b", "Model 5", "Model 6"),
  Method = c(
    "Average Rating",
    "Movie Effect",
    "Movie + User Effects",
    "Movie + User + Genre Effects",
    "Movie + User + Genre-Specific Effects",
    "Movie + User + Genre-Specific + Release Year Effects",
    "Movie + User + Genre-Specific + Release Year + Review Date Effects"
  ),
  RMSE = c(
    round(rmse_m1, 4),
    round(rmse_m2, 4),
    round(rmse_m3, 4),
    round(rmse_m4a, 4),
    round(rmse_m4b, 4),
    round(rmse_m5, 4),
    round(rmse_m6, 4)
  )
)

# Calculate the difference from the previous model and create table
rmse_results %>%
  mutate(Diff = c(NA, diff(RMSE))) %>%
  kbl(
      # format = 'latex', 
      # booktabs = TRUE, 
      linesep = "\\addlinespace",
      align = c("l", "l", "c", "c"),
      caption = "RMSE Results for the Initial Models") %>% 
  row_spec(0, bold = TRUE) %>%
  column_spec(1, width = "1.6cm") %>%  
  column_spec(2, width = "10cm") %>%  
  column_spec(3, width = "1.5cm") %>% 
  column_spec(4, width = "1.5cm") %>% 
  kable_styling(
      latex_options = c("striped", "HOLD_position"),
      position = "left",
      stripe_color = "gray!15",
      font_size = 11,
      full_width = FALSE)



####################################################################################################################
# Clamping Adjustments
####################################################################################################################
# Please note that clamping estimations for each model were computed earlier (with the models)
# Create a data frame to store the results for Models 1-6 including clamping adjustments
rmse_results <- data_frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4a", "Model 4b", "Model 5", "Model 6"),
  Method = c(
    "Average Rating",
    "Movie Effect",
    "Movie + User Effects",
    "Movie + User + Genre Effects",
    "Movie + User + Genre-Specific Effects",
    "Movie + User + Genre-Specific + Release Year Effects",
    "Movie + User + Genre-Specific + Release Year + Review Date Effects"
  ),
  RMSE = c(
    round(rmse_m1, 4),
    round(rmse_m2, 4),
    round(rmse_m3, 4),
    round(rmse_m4a, 4),
    round(rmse_m4b, 4),
    round(rmse_m5, 4),
    round(rmse_m6, 4)
  ),
  
  Clamp = c(
    round(rmse_m1_clamp, 4),
    round(rmse_m2_clamp, 4),
    round(rmse_m3_clamp, 4),
    round(rmse_m4a_clamp, 4),
    round(rmse_m4b_clamp, 4),
    round(rmse_m5_clamp, 4),
    round(rmse_m6_clamp, 4)
  ))

# Create table
rmse_results %>%
  kbl(
      # format = 'latex', 
      # booktabs = TRUE, 
      linesep = "\\addlinespace",
      align = c("l", "l", "c", "c"),
      caption = "RMSE Results with and without Clamping") %>% 
  row_spec(0, bold = TRUE) %>%
  column_spec(1, width = "1.6cm") %>%  
  column_spec(2, width = "10cm") %>%  
  column_spec(3, width = "1.5cm") %>% 
  column_spec(4, width = "1.5cm") %>% 
  kable_styling(
      latex_options = c("striped", "HOLD_position"),
      position = "left",
      stripe_color = "gray!15",
      font_size = 11,
      full_width = FALSE)



####################################################################################################################
# Regularized Models
####################################################################################################################

# Remove unnecessary objects from the environment - just to make sure to use correct ones
rm(mu,
   predict_b_m, predict_b_u, predict_b_g, predict_b_ry, predict_b_rd, 
   movie_averages, user_averages, genre_averages, release_year_averages, review_date_averages)

# Mean of Observed Values
mu <- mean(train_set$rating)

# Set Up the Lambda Sequence
# Previously lambda sequence was set to seq(0, 10, 0.25)
# After determining that the optimal lambda was λ=5
# The sequence was refined
lambdas <- seq(4.75, 5.25, 0.05)

# Function to compute RMSE for a given lambda with clamping - very very long time to run!
compute_rmse_clamped <- function(lambda) {
  
  # Regularized Movie Effect (b_m)
  movie_averages <- train_set %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu) / (n() + lambda))
  
  # Regularized User Effect (b_u)
  user_averages <- train_set %>%
    left_join(movie_averages, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_m) / (n() + lambda))
  
  # Regularized Genre-Specific Effect (b_g)
  genre_averages <- train_set_genres %>%
    left_join(movie_averages, by = "movieId") %>%
    left_join(user_averages, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_m - b_u) / (n() + lambda))
  
  # Regularized Release Year Effect (b_ry)
  release_year_averages <- train_set_genres %>%
    left_join(movie_averages, by = "movieId") %>%
    left_join(user_averages, by = "userId") %>%
    left_join(genre_averages, by = "genres") %>%
    group_by(release_year) %>%
    summarize(b_ry = sum(rating - mu - b_m - b_u - b_g) / (n() + lambda))
  
  # Regularized Review Date Effect (b_rd)
  review_date_averages <- train_set_genres %>%
    left_join(movie_averages, by = "movieId") %>%
    left_join(user_averages, by = "userId") %>%
    left_join(genre_averages, by = "genres") %>%
    left_join(release_year_averages, by = "release_year") %>%
    group_by(review_date) %>%
    summarize(b_rd = sum(rating - mu - b_m - b_u - b_g - b_ry) / (n() + lambda))
  
  # Predict ratings using test set
  predictions <- test_set_genres %>%
    left_join(movie_averages, by = "movieId") %>%
    left_join(user_averages, by = "userId") %>%
    left_join(genre_averages, by = "genres") %>%
    left_join(release_year_averages, by = "release_year") %>%
    left_join(review_date_averages, by = "review_date") %>%
    mutate(prediction = mu + b_m + b_u + b_g + b_ry + b_rd) %>%
    .$prediction
  
  # Clamp predictions to the valid range (0.5 to 5)
  clamped_predictions <- pmin(pmax(predictions, 0.5), 5)
  
  # Return RMSE for clamped predictions
  RMSE(test_set_genres$rating, clamped_predictions)
}

# Apply the function over the lambda values for clamped RMSE
rmse_values <- sapply(lambdas, compute_rmse_clamped)

# Find the best lambda (lambda with the minimum clamped RMSE)
best_lambda <- lambdas[which.min(rmse_values)]

# Print the best clamped RMSE
min_rmse <- min(rmse_values)


# Plot RMSE Against Lambda
qplot(lambdas, rmse_values) + 
  xlab("\nLambdas") +
  ylab("RMSE values\n") 



# Update the data frame to include the performance metrics for the new model
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model = "Model 7", 
                                     Method="Regularized Model", 
                                     RMSE = NA_real_,
                                     Clamp = round(min_rmse,4)))

# Create table
rmse_results %>%
  kbl(
      # format = 'latex', 
      # booktabs = TRUE, 
      linesep = "\\addlinespace",
      align = c("l", "l", "c", "c"),
      caption = "RMSE Results including the Regularized Model") %>% 
  row_spec(0, bold = TRUE) %>%
  column_spec(1, width = "1.6cm") %>%  
  column_spec(2, width = "10cm") %>%  
  column_spec(3, width = "1.5cm") %>% 
  column_spec(4, width = "1.5cm") %>% 
  kable_styling(
      latex_options = c("striped", "HOLD_position"),
      position = "left",
      stripe_color = "gray!15",
      font_size = 11,
      full_width = FALSE)


####################################################################################################################
# Final Model Assessment 
####################################################################################################################
# Update final_holdout_test 
####################################################################################################################
# Remove unnecessary objects from the environment - just to make sure to use correct ones
rm(mu, test_set, test_set_genres, train_set, train_set_genres)


# Update final_holdout_test in Line with Changes Made to edx Dataset
# Create New Two Columns Based on timestamp Column
final_holdout_test <- final_holdout_test %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"),
                                                    review_year = format(review_date, "%Y")) 

# clean The *title* Column to Extract Release Year and Adjust Title Formatting
final_holdout_test <- final_holdout_test %>% 
  # Trim title
  mutate(title = str_trim(title)) %>% 
  # Extract year and temporary title
  extract(title, c("title_temp", "release_year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  # Convert release_year by taking the first year
  mutate(release_year = as.integer(if_else(str_length(release_year) > 4, str_split(release_year, "-", simplify = TRUE)[1], release_year)),
         title = if_else(is.na(title_temp), title, title_temp)) %>%
  # Drop temporary title
  select(-title_temp)

# Create Distinct Genre Categories
final_holdout_test_genres <- final_holdout_test %>% separate_rows(genres, sep = "\\|") # Takes very long time to run!



####################################################################################################################
# FINAL MODEL
####################################################################################################################

# Step 1: Train on edx_genres dataset
# Calculate the Regularized Effects
mu <- mean(edx$rating)

# Movie Effect
movie_averages <- edx %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu) / (n() + best_lambda))

# User Effect
user_averages <- edx %>%
  left_join(movie_averages, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_m) / (n() + best_lambda))

# Genre-Specific Effect
genre_averages <- edx_genres %>%
  left_join(movie_averages, by = "movieId") %>%
  left_join(user_averages, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_m - b_u) / (n() + best_lambda))

# Release Year Effect
release_year_averages <- edx_genres %>%
  left_join(movie_averages, by = "movieId") %>%
  left_join(user_averages, by = "userId") %>%
  left_join(genre_averages, by = "genres") %>%
  group_by(release_year) %>%
  summarize(b_ry = sum(rating - mu - b_m - b_u - b_g) / (n() + best_lambda))

# Review Date Effect
review_date_averages <- edx_genres %>%
  left_join(movie_averages, by = "movieId") %>%
  left_join(user_averages, by = "userId") %>%
  left_join(genre_averages, by = "genres") %>%
  left_join(release_year_averages, by = "release_year") %>%
  group_by(review_date) %>%
  summarize(b_rd = sum(rating - mu - b_m - b_u - b_g - b_ry) / (n() + best_lambda))


# Step 2: Apply the Regularized Model to the Final Holdout Test Dataset
predict_b_rd <- final_holdout_test_genres %>%
  left_join(movie_averages, by = "movieId") %>%
  left_join(user_averages, by = "userId") %>%
  left_join(genre_averages, by = "genres") %>%
  left_join(release_year_averages, by = "release_year") %>%
  left_join(review_date_averages, by = "review_date") %>%
  mutate(prediction = mu + b_m + b_u + b_g + b_ry + b_rd) %>%
  .$prediction


# Step 3: Calculate the RMSE for the Regularized Model
rmse_final <- RMSE(final_holdout_test_genres$rating, predict_b_rd)

# Calculate RMSE again with a lower limit of 0.5 and an upper limit of 5
rmse_final_clamp <- RMSE(clamp(predict_b_rd, lower = 0.5, upper = 5), final_holdout_test_genres$rating)


# Residuals Figure
# Clamp predictions
predict_b_rd_clamp <- clamp(predict_b_rd, lower = 0.5, upper = 5)

# Create a data frame for residuals
residuals_df <- data.frame(Fitted = predict_b_rd_clamp,
                           Residuals = predict_b_rd_clamp - final_holdout_test_genres$rating)

# Plot Histogram of Residuals 
ggplot(residuals_df, aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "#FFDAB9", color = "black") +
  ylab("Frequency\n") + 
  scale_y_continuous(labels = comma) 






